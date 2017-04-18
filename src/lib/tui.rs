use itertools::Itertools;
use lib::*;
use lib::buffer::Buffer;
use lib::frame::{Frame, RenderingSection, ActiveWindow, Partition};
use lib::input::{Keymap, Key, Cmd, bind_key, key_seq_to_string, Event};
use lib::seq_set::SequenceSet;
use lib::window::{Window, MoveRes};
use std::{fs, process};
use std::boxed::FnBox;
use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::io::{self, Write, stdout, Stdout};
use std::iter::repeat;
use std::path::Path;
use std::rc::Rc;
use std::time::Duration;
use termion::{terminal_size, cursor};
use termion::raw::{IntoRawMode, RawTerminal};

fn modulo(lhs: isize, rhs: usize) -> usize {
    let rhs = rhs as isize;
    (((lhs % rhs) + rhs) % rhs) as usize
}

struct Clipring {
    entries: Vec<Vec<String>>,
    active: usize,
}

impl Clipring {
    fn get(&self) -> Option<&[String]> {
        self.entries.get(self.active).map(|v| v.as_slice())
    }

    fn push(&mut self, e: Vec<String>) {
        self.entries.push(e);
        self.active = self.entries.len() - 1
    }

    fn cycle(&mut self, n: isize) {
        if !self.entries.is_empty() {
            self.active = modulo(self.active as isize + n, self.entries.len());
        }
    }
}

pub struct Tui {
    buffers: HashMap<String, Rc<RefCell<Buffer>>>,
    global_keymap: Keymap,
    frame: Frame,
    term: RawTerminal<Stdout>,
    key_seq: Vec<Key>,
    prev_rendering_sections: Vec<RenderingSection>,
    clipring: Clipring,
}

impl Tui {
    fn new() -> Self {
        let mut buffers = HashMap::new();
        let scratch = "*scratch*";
        let scratch_buf = Rc::new(RefCell::new(Buffer::new(scratch)));
        buffers.insert(scratch.to_string(), scratch_buf.clone());

        let mut keymap = Keymap::new();

        let bindings = vec![(vec![Key::Ctrl('g')], Cmd::Cancel),
                            (vec![Key::Ctrl(' ')], Cmd::SetMark),
                            (vec![Key::Ctrl('f')], Cmd::MoveH(1)),
                            (vec![Key::Ctrl('b')], Cmd::MoveH(-1)),
                            (vec![Key::Ctrl('n')], Cmd::MoveV(1)),
                            (vec![Key::Ctrl('p')], Cmd::MoveV(-1)),
                            (vec![Key::Right], Cmd::MoveH(1)),
                            (vec![Key::Left], Cmd::MoveH(-1)),
                            (vec![Key::Down], Cmd::MoveV(1)),
                            (vec![Key::Up], Cmd::MoveV(-1)),
                            (vec![Key::Ctrl('e')], Cmd::EndOfLine),
                            (vec![Key::Ctrl('a')], Cmd::BeginningOfLine),
                            (vec![Key::Alt('p')], Cmd::PageUp),
                            (vec![Key::Alt('n')], Cmd::PageDown),
                            (vec![Key::Alt('>')], Cmd::EndOfBuffer),
                            (vec![Key::Alt('<')], Cmd::BeginningOfBuffer),
                            (vec![Key::Ctrl('s')], Cmd::SearchForward),
                            (vec![Key::Ctrl('r')], Cmd::SearchBackward),
                            (vec![Key::Ctrl('d')], Cmd::DeleteCharsH(1)),
                            (vec![Key::Delete], Cmd::DeleteCharsH(1)),
                            (vec![Key::Ctrl('h')], Cmd::DeleteCharsH(-1)),
                            (vec![Key::Backspace], Cmd::DeleteCharsH(-1)),
                            (vec![Key::Ctrl('w')], Cmd::Cut),
                            (vec![Key::Alt('w')], Cmd::Copy),
                            (vec![Key::Ctrl('y')], Cmd::Paste),
                            (vec![Key::Alt('y')], Cmd::CycleClipring(-1)),
                            (vec![Key::Alt('Y')], Cmd::CycleClipring(1)),
                            (vec![Key::Ctrl('/')], Cmd::Undo),
                            (vec![Key::Ctrl('_')], Cmd::Undo),
                            (vec![Key::Ctrl('7')], Cmd::Undo),
                            (vec![Key::Char('\n')], Cmd::Newline),
                            (vec![Key::Char('\t')], Cmd::Tab),
                            (vec![Key::Ctrl('x'), Key::Ctrl('s')], Cmd::Save),
                            (vec![Key::Ctrl('x'), Key::Ctrl('f')], Cmd::OpenFile),
                            (vec![Key::Ctrl('x'), Key::Char('b')], Cmd::SwitchBuf),
                            (vec![Key::Alt('g')], Cmd::GoToLine),
                            (vec![Key::Ctrl('x'), Key::Ctrl('c')], Cmd::Exit),
                            (vec![Key::Ctrl('x'), Key::Char('2')], Cmd::SplitV),
                            (vec![Key::Ctrl('x'), Key::Char('3')], Cmd::SplitH),
                            (vec![Key::Ctrl('x'), Key::Char('0')], Cmd::DeleteWindow),
                            (vec![Key::Ctrl('x'), Key::Char('1')], Cmd::DeleteOtherWindows),
                            (vec![Key::Ctrl('x'), Key::Char('o')], Cmd::OtherWindow(1)),
                            (vec![Key::Ctrl('x'), Key::Char('i')], Cmd::OtherWindow(-1))];

        for (key_seq, cmd) in bindings {
            if bind_key(&mut keymap, &key_seq, cmd).is_some() {
                panic!("Duplicate default keybinding `{:?}`", key_seq)
            }
        }

        let (w, h) = terminal_size().expect("Failed to get terminal size");

        Tui {
            buffers: buffers,
            global_keymap: keymap,
            frame: Frame::new(w, h, scratch_buf),
            term: stdout().into_raw_mode().expect("Terminal failed to enter raw mode"),
            key_seq: Vec::new(),
            prev_rendering_sections: Vec::new(),
            clipring: Clipring { entries: Vec::new(), active: 0 },
        }
    }

    fn active_window(&self) -> &Rc<RefCell<Window>> {
        self.frame.active_window.window()
    }

    /// Check for change in terminal size. Update sizes as necessary
    ///
    /// Returns whether terminal size has changed
    fn update_term_size(&mut self) {
        let (w, h) = terminal_size().expect("Failed to get terminal size");
        self.frame.resize(w, h);
    }

    /// Switch to buffer `name` in the active view
    fn switch_to_buffer_with_name(&mut self, name: &str) {
        let buffer = self.buffers
                         .entry(name.to_string())
                         .or_insert(Rc::new(RefCell::new(Buffer::new(name))))
                         .clone();
        self.active_window().borrow_mut().switch_to_buffer(buffer)
    }

    fn open_file_from_path(&mut self, name: &str) {
        match fs::canonicalize(name) {
            Ok(filepath) => {
                let buffer = Buffer::from_file(filepath);
                let name = buffer.name().to_string();

                let buffer_shared = Rc::new(RefCell::new(buffer));

                self.buffers.insert(name.clone(), buffer_shared);
                self.switch_to_buffer_with_name(&name)
            }
            Err(e) => self.message(format!("Error opening file: {}", e)),
        }
    }

    fn set_mark_at_point(&self) {
        self.active_window().borrow_mut().set_mark_at_point()
    }

    fn insert_char_at_point(&self, c: char) {
        self.active_window().borrow_mut().insert_char_at_point(c)
    }

    fn move_point_h(&self, n: isize) -> MoveRes {
        self.active_window().borrow_mut().move_point_h(n)
    }

    fn move_point_v(&self, n: isize) -> MoveRes {
        self.active_window().borrow_mut().move_point_v(n)
    }

    fn page_up(&self) -> MoveRes {
        self.active_window().borrow_mut().page_up()
    }

    fn page_down(&self) -> MoveRes {
        self.active_window().borrow_mut().page_down()
    }

    fn search_forward_open_prompt(&mut self, default: &str) {
        match self.frame.active_window.clone() {
            ActiveWindow::Window(_) => {
                self.prompt("Search: ",
                            default,
                            move |ted, query| if ted.active_window()
                                                         .borrow_mut()
                                                         .search_forward(query) {
                                ted.active_window().borrow_mut().reposition_view();
                                ted.search_forward_open_prompt(query)
                            } else {
                                ted.message("Search failed")
                            })
            }
            ActiveWindow::Prompt(_) => self.message("Can't search in minibuffer"),
        }
    }

    fn search_backward_open_prompt(&mut self, default: &str) {
        match self.frame.active_window.clone() {
            ActiveWindow::Window(_) => {
                self.prompt("Search backward: ",
                            default,
                            move |ted, query| if ted.active_window()
                                                         .borrow_mut()
                                                         .search_backward(query) {
                                ted.active_window().borrow_mut().reposition_view();
                                ted.search_backward_open_prompt(query)
                            } else {
                                ted.message("Search failed")
                            })
            }
            ActiveWindow::Prompt(_) => self.message("Can't search in minibuffer"),
        }
    }

    fn delete_chars_h(&self, n: isize) -> MoveRes {
        self.active_window().borrow_mut().delete_chars_h(n)
    }

    fn copy_selection(&mut self) {
        let r = {
            let mut window = self.active_window().borrow_mut();
            window.copy_selection()
        };
        if let Some(sel) = r {
            self.clipring.push(sel)
        }
    }

    fn cut_selection(&mut self) {
        let r = {
            let mut window = self.active_window().borrow_mut();
            window.cut_selection()
        };
        if let Some(sel) = r {
            self.clipring.push(sel)
        }
    }

    fn paste(&mut self) {
        if self.clipring.get().is_some() {
            self.active_window().borrow_mut().paste(self.clipring.get().unwrap())
        } else {
            self.message("Clipring is empty")
        }
    }

    fn cycle_clipring(&mut self, n: isize) {
        self.clipring.cycle(n);
        if self.clipring.get().is_some() {
            self.frame.minibuffer.echo(&format!("{:?}",
                                                self.clipring
                                                    .get()
                                                    .unwrap()
                                                    .iter()
                                                    .map(String::as_str)
                                                    .intersperse("\n")
                                                    .collect::<String>()))
        } else {
            self.message("Clipring is empty")
        }
    }

    fn undo(&mut self) {
        if self.active_window().borrow_mut().undo() {
            self.message("Undo")
        } else {
            self.message("Nothing to undo")
        }
    }

    fn insert_new_line(&self) {
        self.active_window().borrow_mut().insert_new_line()
    }

    fn insert_tab(&self) {
        self.active_window().borrow_mut().insert_tab()
    }

    fn save_buffer(&mut self, buffer: Rc<RefCell<Buffer>>) {
        fn handle_res(ted: &mut Tui, res: io::Result<()>, filepath: &Path) {
            match res {
                Ok(()) => ted.message(format!("Saved to \"{}\"", filepath.display())),
                Err(e) => ted.message(format!("Error writing file \"{}\", {:?}",
                                              filepath.display(),
                                              e)),
            }
        }

        let filepath = buffer.borrow().filepath().map(|p| p.to_path_buf());

        match filepath {
            Some(p) => {
                let r = buffer.borrow_mut().save_to_path(&p);
                handle_res(self, r, &p)
            }
            None => self.prompt("Save file: ", "", move |ted, s| {
                let p = Path::new(&s);
                let r = buffer.borrow_mut().save_to_path(p);
                handle_res(ted, r, p)
            }),
        }
    }

    fn save_active_buffer(&mut self) {
        let buffer = {
            let window_b = self.active_window().borrow();
            window_b.buffer().clone()
        };

        self.save_buffer(buffer)
    }

    fn open_file(&mut self) {
        self.prompt("Open file: ", "", |ted, s| ted.open_file_from_path(s))
    }

    fn switch_buffer(&mut self) {
        self.prompt("Switch to buffer: ",
                    "",
                    |ted, s| ted.switch_to_buffer_with_name(s))
    }

    fn split_h(&mut self) {
        match self.frame.active_window {
            ActiveWindow::Window(ref w) => Window::split_h(w.clone()),
            ActiveWindow::Prompt(_) => self.message("Can't split minibuffer window"),
        }
    }

    fn split_v(&mut self) {
        match self.frame.active_window {
            ActiveWindow::Window(ref w) => Window::split_v(w.clone()),
            ActiveWindow::Prompt(_) => self.message("Can't split minibuffer window"),
        }
    }

    /// Select the `n`th window following or preceding the current window.
    ///
    /// The topmost prompt is included in the selection cycle
    fn select_other_window(&mut self, n: isize) {
        if n != 0 {
            self.active_window().borrow_mut().is_active = false;

            let (other_window, was_prompt) = match self.frame.active_window {
                ActiveWindow::Window(ref w) => {
                    let next_window = w.borrow().other_window(n);
                    if (n > 0 && next_window.borrow().is_first_in_frame()) ||
                       (n < 0 && next_window.borrow().is_last_in_frame()) {
                        if let Some(prompt) = self.frame
                                                  .minibuffer
                                                  .prompt_stack
                                                  .last()
                                                  .map(|p| p.window.clone()) {
                            (ActiveWindow::Prompt(prompt), false)
                        } else {
                            (ActiveWindow::Window(next_window.clone()), false)
                        }
                    } else {
                        (ActiveWindow::Window(next_window), false)
                    }
                }
                ActiveWindow::Prompt(_) => {
                    if n > 0 {
                        (ActiveWindow::Window(self.frame.windows.borrow().first_window()), true)
                    } else {
                        (ActiveWindow::Window(self.frame.windows.borrow().last_window()), true)
                    }
                }
            };
            self.frame.active_window = other_window;
            self.active_window().borrow_mut().is_active = true;

            if was_prompt {
                self.select_other_window(n.signum() * (n.abs() - 1));
            }
        }
    }

    fn delete_active_window(&mut self) {
        let new_active_window = match self.frame.active_window {
            ActiveWindow::Prompt(_) => {
                self.message("Can't delete minibuffer window");
                return;
            }
            ActiveWindow::Window(ref window) => {
                let maybe_sibling = window.borrow().delete();

                if let Some(sibling) = maybe_sibling {
                    sibling.borrow_mut().is_active = true;
                    ActiveWindow::Window(sibling)
                } else {
                    self.frame.minibuffer.echo("Can't delete root window");
                    return;
                }
            }
        };
        self.frame.active_window = new_active_window;
    }

    fn delete_inactive_windows(&mut self) {
        match self.frame.active_window.clone() {
            ActiveWindow::Prompt(_) => self.message("Can't delete all ordinary windows"),
            ActiveWindow::Window(window) => {
                let (w, h) = self.frame.windows.borrow().size();
                window.borrow_mut().set_size(w, h);

                let root_partition = Partition::new_root_from_window(window);

                self.frame.windows = root_partition;
            }
        }
    }

    /// Prompt the user for input, then pass the parsed input to the given callback
    fn prompt<F>(&mut self, prompt: &str, default: &str, callback: F)
        where F: FnOnce(&mut Tui, &str) + 'static
    {
        let prompt_w = self.frame.minibuffer.push_new_prompt(self.frame.active_window.clone(),
                                                             prompt,
                                                             default,
                                                             Box::new(callback));

        self.frame.active_window = ActiveWindow::Prompt(prompt_w);
    }


    fn prompt_submit(&mut self) {
        if let Some(prompt) = self.frame.minibuffer.prompt_stack.pop() {
            let input = prompt.input();

            self.frame.active_window = prompt.subject_window;

            FnBox::call_box(prompt.callback, (self, input.as_str()))
        } else {
            self.message("No prompt to submit")
        }
    }

    fn go_to_line(&mut self) {
        self.prompt("Go to line: ", "", |ted, s| match s.parse::<isize>() {
            Ok(n) => {
                ted.frame.active_window.window().borrow_mut().move_point_to_line(n - 1);
            }
            Err(_) => ted.message("Please enter a number."),
        })
    }

    fn cancel(&mut self) {
        match self.frame.active_window {
            ActiveWindow::Prompt(_) => {
                self.select_other_window(1);

                self.frame
                    .minibuffer
                    .prompt_stack
                    .pop()
                    .expect("Active prompt not in prompt stack");
            }
            ActiveWindow::Window(ref w) => w.borrow_mut().cancel(),
        }
        self.message("Cancel");
    }

    /// Returns true if exit
    fn eval(&mut self, cmd: Cmd) {
        let mut r = MoveRes::Ok;
        match cmd {
            Cmd::SetMark => self.set_mark_at_point(),
            Cmd::Insert(c) => self.insert_char_at_point(c),
            Cmd::MoveH(n) => r = self.move_point_h(n),
            Cmd::MoveV(n) => r = self.move_point_v(n),
            Cmd::EndOfLine => self.active_window().borrow_mut().move_point_to_end_of_line(),
            Cmd::BeginningOfLine => self.active_window()
                                        .borrow_mut()
                                        .move_point_to_beginning_of_line(),
            Cmd::PageUp => r = self.page_up(),
            Cmd::PageDown => r = self.page_down(),
            Cmd::EndOfBuffer => self.active_window().borrow_mut().move_point_to_end_of_buffer(),
            Cmd::BeginningOfBuffer => self.active_window()
                                          .borrow_mut()
                                          .move_point_to_beginning_of_buffer(),
            Cmd::SearchForward => self.search_forward_open_prompt(""),
            Cmd::SearchBackward => self.search_backward_open_prompt(""),
            Cmd::DeleteCharsH(n) => r = self.delete_chars_h(n),
            Cmd::Copy => self.copy_selection(),
            Cmd::Cut => self.cut_selection(),
            Cmd::Paste => self.paste(),
            Cmd::CycleClipring(n) => self.cycle_clipring(n),
            Cmd::Undo => self.undo(),
            Cmd::Newline => self.insert_new_line(),
            Cmd::Tab => self.insert_tab(),
            Cmd::Exit => self.try_exit(HashSet::new()),
            Cmd::GoToLine => self.go_to_line(),
            Cmd::Save => self.save_active_buffer(),
            Cmd::OpenFile => self.open_file(),
            Cmd::SwitchBuf => self.switch_buffer(),
            Cmd::SplitH => self.split_h(),
            Cmd::SplitV => self.split_v(),
            Cmd::DeleteWindow => self.delete_active_window(),
            Cmd::DeleteOtherWindows => self.delete_inactive_windows(),
            Cmd::OtherWindow(n) => self.select_other_window(n),
            Cmd::PromptSubmit => self.prompt_submit(),
            Cmd::Cancel => self.cancel(),
        }
        match r {
            MoveRes::Beg => self.message("Beginning of buffer"),
            MoveRes::End => self.message("End of buffer"),
            MoveRes::Ok => (),
        }
        self.active_window().borrow_mut().reposition_view();
    }

    /// Get the message buffer
    fn message_buffer_mut(&mut self) -> RefMut<Buffer> {
        let messages = "*messages*";
        self.buffers
            .entry(messages.to_string())
            .or_insert(Rc::new(RefCell::new(Buffer::new(messages))))
            .borrow_mut()
    }

    /// Write a message to the *message* buffer
    fn message<S: AsRef<str>>(&mut self, msg: S) {
        let msg = msg.as_ref();
        self.frame.minibuffer.echo(msg);
        self.message_buffer_mut().push_line(msg);
    }

    fn handle_key_event(&mut self, mut key: Key) {
        /// Keymaps sorted by priority
        fn keymaps_is_prefix(keymaps: &[&Keymap], key_seq: &[Key]) -> bool {
            keymaps.iter()
                   .any(|keymap| !keymap.get_node(key_seq).map(|n| n.is_empty()).unwrap_or(true))
        }
        fn keymaps_get(keymaps: &[&Keymap], key_seq: &[Key]) -> Option<Cmd> {
            keymaps.iter().filter_map(|keymap| keymap.get(key_seq)).next().cloned()
        }

        // Remap any "M-FOO" to "ESC FOO" and "<null>" to "C-<space>"
        match key {
            Key::Alt(c) => {
                key = Key::Char(c);
                self.key_seq.push(Key::Esc);
                self.key_seq.push(key);
            }
            Key::Null => {
                key = Key::Ctrl(' ');
                self.key_seq.push(key);
            }
            _ => {
                self.key_seq.push(key);
            }
        }

        let maybe_cmd = {
            let active_buffer = &self.frame.active_window.window().borrow().buffer().clone();
            let active_buffer_b = active_buffer.borrow();
            let keymaps = [&active_buffer_b.local_keymap, &self.global_keymap];

            if let Some(Cmd::Cancel) = keymaps_get(&keymaps, &[key]) {
                Some(Cmd::Cancel)
            } else if let Some(cmd) = keymaps_get(&keymaps, &self.key_seq) {
                self.key_seq.clear();
                Some(cmd)
            } else if keymaps_is_prefix(&keymaps, &self.key_seq) {
                self.frame
                    .minibuffer
                    .echo(&format!("{} ...", key_seq_to_string(&self.key_seq)));

                return;
            } else if let Key::Char(c) = key {
                if self.key_seq.len() == 1 {
                    self.key_seq.clear();
                    Some(Cmd::Insert(c))
                } else {
                    None
                }
            } else {
                None
            }
        };

        match maybe_cmd {
            Some(Cmd::Cancel) => {
                self.key_seq.pop();
                if !self.key_seq.is_empty() {
                    self.key_seq.clear();
                    self.message("Key sequence canceled");
                } else {
                    self.eval(Cmd::Cancel)
                }
            }
            Some(cmd) => self.eval(cmd),
            None => {
                let s = format!("Key {} is undefined", key_seq_to_string(&self.key_seq));
                self.message(s);
                self.key_seq.clear();
            }
        }
    }

    /// Return whether to exit
    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Key(k) => {
                self.frame.minibuffer.clear_echo();
                self.handle_key_event(k)
            }
            Event::Update => (),
            Event::Mouse(_) => {
                self.frame.minibuffer.clear_echo();
                self.message(format!("Mousevent is undefined"));
            }
            Event::Unsupported(u) => {
                self.frame.minibuffer.clear_echo();
                self.message(format!("Unsupported event {:?}", u));
            }
        }
        self.update_term_size();
        self.redraw();
    }

    fn cursor_pos(&self) -> (u16, u16) {
        let active = self.frame.active_window.window().borrow();
        let (left, top) = active.pos_in_buffer();
        let cursor_x_relative_window = (active.point().col_i - left) as u16;
        let cursor_y_relative_window = (active.point().line_i - top) as u16;

        let (window_x_absolute, window_y_absolute) = match self.frame.active_window {
            ActiveWindow::Prompt(_) => {
                let (_, h) = self.frame.size();
                (0, h - 1)
            }
            ActiveWindow::Window(_) => active.pos_in_frame(),
        };

        (1 + window_x_absolute + cursor_x_relative_window,
         1 + window_y_absolute + cursor_y_relative_window)
    }

    fn _redraw(&mut self) -> io::Result<()> {
        let rendering_sections = self.frame.render();

        write!(self.term, "{}", cursor::Hide)?;

        // Redraw modified sections
        for (section, old_section) in
            rendering_sections.iter()
                              .zip(self.prev_rendering_sections
                                       .iter()
                                       .map(Some)
                                       .chain(repeat(None))) {
            let (same_pos, same_len) = old_section.map(|os| {
                                                      ((os.x, os.y) == (section.x, section.y),
                                                       os.lines.len() == section.lines.len())
                                                  })
                                                  .unwrap_or((false, false));

            for (i, line) in section.lines.iter().enumerate() {
                let old_line = old_section.and_then(|s| s.lines.get(i));

                if !same_pos || !same_len || Some(line) != old_line {
                    // If anything has changed, redraw
                    write!(self.term,
                           "{}{}",
                           cursor::Goto(section.x + 1, section.y + i as u16 + 1),
                           line)?
                }
            }
        }

        self.prev_rendering_sections = rendering_sections;

        let (cursor_x, cursor_y) = self.cursor_pos();

        write!(self.term,
               "{}{}",
               cursor::Show,
               cursor::Goto(cursor_x, cursor_y))?;

        self.term.flush()
    }

    fn redraw(&mut self) {
        self._redraw().expect("Redraw failed")
    }

    /// Clean up before exiting
    ///
    /// Returns whether exiting should continue. A return value of false
    /// indicates that the user canceled the exit.
    fn cleanup(&mut self, mut ignore_buffers: HashSet<String>) -> bool {
        let buffers = self.buffers.values().cloned().collect::<Vec<_>>();
        for buffer in buffers {
            let (name, modified) = {
                let b = buffer.borrow();
                (b.name().to_string(), b.is_modified())
            };

            if modified && !ignore_buffers.contains(&name) {
                self.prompt(&format!("Buffer {} has been modified. Save? (yes, ignore, cancel) ",
                                     name),
                            "",
                            move |ted, inp| {
                    let mut decisions = SequenceSet::new();
                    decisions.insert_str("yes");
                    decisions.insert_str("ignore");
                    decisions.insert_str("cancel");

                    match decisions.contains_str(inp) {
                        seq_set::StringEntry::Some(ref s) if s == "yes" => {
                            ted.save_buffer(buffer);
                            ted.try_exit(ignore_buffers)
                        }
                        seq_set::StringEntry::Some(ref s) if s == "ignore" => {
                            ignore_buffers.insert(name);
                            ted.try_exit(ignore_buffers)
                        }
                        seq_set::StringEntry::Some(ref s) if s == "cancel" => (),
                        seq_set::StringEntry::Some(_) => unreachable!(),
                        seq_set::StringEntry::NotUnique(matches) => {
                            ted.message(format!("Multiple possible matches. {}",
                                                matches.iter()
                                                       .map(String::as_str)
                                                       .intersperse(", ")
                                                       .collect::<String>()));
                            ted.try_exit(HashSet::new())
                        }
                        seq_set::StringEntry::None => {
                            ted.message(format!("Undefined option \"{}\"", inp));
                            ted.try_exit(HashSet::new())
                        }
                    }
                });
                return false;
            }
        }

        true
    }

    fn try_exit(&mut self, ignore_buffers: HashSet<String>) {
        if self.cleanup(ignore_buffers) {
            write!(self.term, "{}", ::termion::clear::All).ok();
            process::exit(0)
        }
    }
}

/// Start editor in terminal user interface mode
pub fn start_tui(opt_filename: Option<&str>) {
    let mut ted = Tui::new();

    if let Some(filename) = opt_filename {
        ted.open_file_from_path(filename);
    }

    write!(ted.term, "{}{}", *COLOR_BG, *COLOR_TEXT).expect("Failed to reset colors");

    ted.redraw();

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in input::Events::with_update_period(Duration::from_millis(300)) {
        ted.handle_event(event);
    }

    panic!("No more events!")
}