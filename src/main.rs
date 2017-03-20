#[macro_use]
extern crate clap;
extern crate termion;
extern crate unicode_width;
extern crate unicode_segmentation;
extern crate futures;
extern crate tokio_timer;
#[macro_use]
extern crate lazy_static;

#[cfg(feature = "profiling")]
extern crate cpuprofiler;

use clap::{Arg, App};

#[cfg(feature = "profiling")]
use cpuprofiler::PROFILER;
use futures::{Sink, Stream};
use futures::sync::mpsc::channel;
use std::{str, thread, fs};
use std::cell::{RefCell, RefMut, Ref};
use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::{self, Write, stdout, stdin, Stdout, BufRead};
use std::iter::once;
use std::path::PathBuf;
use std::rc::{Rc, Weak};
use std::time::Duration;
use termion::{color, cursor};
use termion::event::{Event, Key, MouseEvent};
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};
use tokio_timer::Timer;
use unicode_segmentation::UnicodeSegmentation;

macro_rules! println_err {
    ($fmt:expr $(, $args:expr )*) => {
        writeln!(io::stderr(), $fmt, $($args),*).unwrap()
    };
}

/// Vertical scroll margin as a normalized percentage of window height
const SCROLL_MARGIN_V: f32 = 0.3;
/// Horizontal scroll margin as a normalized percentage of window width
const SCROLL_MARGIN_H: f32 = 0.02;
/// Whether to insert spaces on TAB press
const TAB_INSERTS_SPACES: bool = false;
/// Tab display width / n.o. spaces to convert to
const TAB_WIDTH: usize = 8;
// 5 nums/padding + 1 space
const LINUM_WIDTH: usize = 6;

lazy_static! {
    static ref COLOR_BG: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x18, 0x18, 0x10));
    static ref COLOR_TEXT: color::Fg<color::Rgb> = color::Fg(color::Rgb(0xF0, 0xE6, 0xD6));
    static ref COLOR_DIM_TEXT: color::Fg<color::Rgb> = color::Fg(color::Rgb(0x60, 0x56, 0x50));
    // At horizontal edges, mark a cell if line continues here on scroll
    static ref COLOR_BG_LINE_CONTINUES: color::Bg<color::Rgb> =
       color::Bg(color::Rgb(0xF0, 0x40, 0x70));
    static ref COLOR_BG_MODELINE: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x99, 0x90, 0x80));
}

/// The width in columns of a string if it is displayed in a terminal.
///
/// Takes into account the current configuration of tab-width
fn display_width(s: &str) -> usize {
    use unicode_width::UnicodeWidthStr;
    s.graphemes(true).map(|g| if g == "\t" { TAB_WIDTH } else { g.width() }).sum()
}

/// Formatting of key events
fn ted_key_to_string(k: Key) -> String {
    match k {
        Key::Ctrl(c) => format!("C-{}", c),
        Key::Alt(c @ '\u{1}'...'\u{1F}') => format!("C-M-{}", (c as u8 - 1 + 'a' as u8) as char),
        Key::Alt(c) => format!("M-{}", c),
        Key::Char('\t') => format!("<tab>"),
        Key::Char('\n') => format!("<return>"),
        Key::Char(c) => format!("{}", c),
        Key::F(n) => format!("<f{}>", n),
        Key::Backspace => format!("<backspace>"),
        Key::Delete => format!("<delete>"),
        Key::Up => format!("<up>"),
        Key::Down => format!("<down>"),
        Key::Left => format!("<left>"),
        Key::Right => format!("<right>"),
        Key::Home => format!("<home>"),
        Key::End => format!("<end>"),
        Key::Insert => format!("<insert>"),
        Key::PageDown => format!("<page down>"),
        Key::PageUp => format!("<page down>"),
        Key::Esc => format!("<escape>"),
        Key::Null => format!("<null>"),
        Key::__IsNotComplete => format!("UNSUPPORTED"),
    }
}

/// A command to execute on e.g. a keypress
#[derive(Debug, Clone)]
enum Cmd {
    /// Insert a character into the buffer at point
    Insert(char),
    /// Insert a tab or spaces
    Tab,
    /// Move point a character forward
    Forward,
    /// Move point a character backward
    Backward,
    /// Move point a character downward
    Downward,
    /// Move point a character upward
    Upward,
    /// Delete a character forwards
    DeleteForward,
    /// Delete a character backwards
    DeleteBackward,
    /// Insert a newline
    Newline,
    /// Exit the program
    Exit,
    /// Read a number from prompt and go to that line
    GoToLine,
    /// Save file
    Save,
    SplitH,
    SplitV,
}

type Keymap = HashMap<Key, Cmd>;

#[derive(Debug, Clone, Copy)]
struct Point {
    /// 0-based index of, hopefully, column in terminal
    ///
    /// E.g. a grapheme that consists of 8 bytes, but should be rendered as
    /// a half-width glyph / single monospace, will count as 1 column.
    /// Because of the nature of fonts and the unicode standard, this might
    /// not always be the case, which means that this value might be incorrect.
    /// This should, however, only affect the rendered appearance of the cursor
    /// in relation to the text, and it should not happen often.
    col_i: usize,
    /// The byte-index corresponding to `self.col` in the current line string
    col_byte_i: usize,
    /// Column index before moving vertically.
    ///
    /// Used to keep track of column when moving along lines
    /// that might be shorter than current line
    prev_col_i: usize,
    /// 0-based index of line in memory
    line_i: usize,
}

impl Point {
    fn new() -> Point {
        Point {
            col_i: 0,
            col_byte_i: 0,
            prev_col_i: 0,
            line_i: 0,
        }
    }

    /// Update the column-position of the point in buffer `buffer`
    fn update_col_i(&mut self, buffer: &Buffer) {
        self.col_i = display_width(&buffer.lines[self.line_i].data[0..self.col_byte_i]);
    }
}

#[derive(Debug)]
struct Line {
    data: String,
    /// Track whether this line has been modified in buffer.
    ///
    /// Used to determine whether to redraw when rendering
    changed: bool,
}

impl Line {
    fn new(s: String) -> Line {
        Line { data: s, changed: true }
    }

    fn insert_str(&mut self, i: usize, s: &str) {
        self.data.insert_str(i, s);
        self.changed = true;
    }

    fn push_str(&mut self, s: &str) {
        self.data.push_str(s);
        self.changed = true;
    }

    fn remove(&mut self, i: usize) {
        self.data.remove(i);
        self.changed = true;
    }
}

/// A buffer is a equivalent to a temporary file in memory.
/// Could be purely temporary data, which does not exist in storage until saved,
/// or simply a normal file loaded to memory for editing
#[derive(Debug)]
struct Buffer {
    lines: Vec<Line>,
    filepath: Option<PathBuf>,
    /// A buffer-local keymap that may override bindings in the global keymap
    local_keymap: Keymap,
}

impl Buffer {
    fn new() -> Self {
        Buffer {
            lines: vec![Line::new(String::new())],
            filepath: None,
            local_keymap: Keymap::new(),
        }
    }

    fn new_file_buffer(filepath: PathBuf) -> Self {
        Buffer { filepath: Some(filepath), ..Buffer::new() }
    }
}

/// A window into a buffer
#[derive(Debug, Clone)]
struct Window {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// The index of the leftmose column of the buffer visible in the window
    left: usize,
    /// The index of the topmost line of the buffer visible in the window
    top: usize,
    /// Track whether the window has been resized or if the area of the buffer to show
    /// has changed, etc.
    ///
    /// Used to determine what to redraw
    changed: bool,
    parent_partition: Weak<RefCell<WindowPartition>>,
    buffer: Rc<RefCell<Buffer>>,
    point: Point,
}

impl Window {
    fn new(w: u16,
           h: u16,
           buffer: Rc<RefCell<Buffer>>,
           parent: Weak<RefCell<WindowPartition>>)
           -> Window {
        Window {
            w: w,
            h: h,
            left: 0,
            top: 0,
            changed: true,
            parent_partition: parent,
            buffer: buffer,
            point: Point::new(),
        }
    }

    /// Insert a string at point
    fn insert_str_at_point(&mut self, s: &str) {
        let mut buffer = self.buffer.borrow_mut();
        {
            let line = &mut buffer.lines[self.point.line_i];

            line.insert_str(self.point.col_byte_i, s);
        }

        self.point.col_byte_i += s.len();
        self.point.update_col_i(&buffer);
        self.point.prev_col_i = self.point.col_i;
    }

    /// Insert a character at point
    fn insert_char_at_point(&mut self, c: char) {
        let mut s = [0; 4];
        self.insert_str_at_point(c.encode_utf8(&mut s))
    }

    /// Move point forward `n` graphemes
    ///
    /// Returns true if end of buffer reached before finished
    fn move_point_forward(&mut self, mut n: usize) -> bool {
        let buffer = self.buffer.borrow();

        'a: while let Some(line) = buffer.lines.get(self.point.line_i) {
            let offset = self.point.col_byte_i;
            let is = line.data[self.point.col_byte_i..]
                .grapheme_indices(true)
                .map(|(i, _)| i + offset)
                .chain(once(line.data.len()));
            for i in is {
                if n == 0 {
                    self.point.col_byte_i = i;
                    self.point.update_col_i(&buffer);
                    self.point.prev_col_i = self.point.col_i;

                    return false;
                } else {
                    n -= 1;
                }
            }
            if self.point.line_i + 1 == buffer.lines.len() {
                self.point.update_col_i(&buffer);
                self.point.prev_col_i = self.point.col_i;

                return true;
            } else {
                self.point.line_i += 1;
                self.point.col_byte_i = 0;
            }
        }
        self.point.update_col_i(&buffer);
        self.point.prev_col_i = self.point.col_i;

        true
    }

    /// Returns true if sucessfully moved backward n characters.
    /// If beginning of buffer reached before n = 0, return true
    fn move_point_backward(&mut self, mut n: usize) -> bool {
        let buffer = self.buffer.borrow();

        let mut line = &buffer.lines[self.point.line_i];
        'a: loop {
            let grapheme_indices_rev = if line.data.len() == self.point.col_byte_i {
                Box::new(line.data[0..self.point.col_byte_i]
                    .grapheme_indices(true)
                    .map(|(i, _)| i)
                    .chain(once(self.point.col_byte_i))
                    .rev()) as Box<Iterator<Item = usize>>
            } else {
                Box::new(line.data[0..(self.point.col_byte_i + 1)]
                    .grapheme_indices(true)
                    .map(|(i, _)| i)
                    .rev()) as Box<Iterator<Item = usize>>
            };
            for i in grapheme_indices_rev {
                let beginning_reached = self.point.line_i == 0 && i == 0;
                if n == 0 || beginning_reached {
                    self.point.col_byte_i = i;
                    self.point.update_col_i(&buffer);
                    self.point.prev_col_i = self.point.col_i;

                    return beginning_reached;
                } else {
                    n -= 1;
                }
            }
            self.point.line_i -= 1;

            if let Some(prev_line) = buffer.lines.get(self.point.line_i) {
                line = prev_line;
                self.point.col_byte_i = line.data.len();
            } else {
                self.point.update_col_i(&buffer);
                self.point.prev_col_i = self.point.col_i;

                return true;
            }
        }
    }

    /// Move point upward (negative) or downward (positive)
    ///
    /// Return whether end/beginning of buffer reached
    fn move_point_v(&mut self, n: isize) -> bool {
        let buffer = self.buffer.borrow();
        let n_lines = buffer.lines.len();

        let end_of_buffer = if self.point.line_i as isize + n >= n_lines as isize {
            self.point.line_i = n_lines - 1;
            false
        } else if (self.point.line_i as isize) + n < 0 {
            self.point.line_i = 0;
            false
        } else {
            self.point.line_i = (self.point.line_i as isize + n) as usize;
            true
        };

        let line = &buffer.lines[self.point.line_i].data;
        self.point.col_i = 0;
        self.point.col_byte_i = 0;
        for (i, g) in line.grapheme_indices(true) {
            let w = display_width(g);
            if self.point.col_i + w > self.point.prev_col_i {
                self.point.col_byte_i = i;
                return end_of_buffer;
            }
            self.point.col_i += w;
        }
        self.point.col_byte_i = line.len();
        end_of_buffer
    }

    // TODO: More generic deletion.
    //       Something like `delete-selection`

    /// Return true if at end of buffer
    fn delete_forward_at_point(&mut self) -> bool {
        let mut buffer = self.buffer.borrow_mut();
        let n_lines = buffer.lines.len();

        self.point.prev_col_i = self.point.col_i;

        if self.point.col_byte_i < buffer.lines[self.point.line_i].data.len() {
            buffer.lines[self.point.line_i].remove(self.point.col_byte_i);

            false
        } else if self.point.line_i < n_lines - 1 {
            let next_line = buffer.lines.remove(self.point.line_i + 1);
            buffer.lines[self.point.line_i].push_str(&next_line.data);

            for changed_or_moved_line in &mut buffer.lines[self.point.line_i..] {
                changed_or_moved_line.changed = true;
            }
            false
        } else {
            true
        }
    }

    /// Return true if at beginning of buffer
    fn delete_backward_at_point(&mut self) -> bool {
        let mut buffer = self.buffer.borrow_mut();

        if self.point.col_i > 0 {
            let i = {
                let line = &mut buffer.lines[self.point.line_i];
                let (i, _) = line.data[0..self.point.col_byte_i]
                    .grapheme_indices(true)
                    .rev()
                    .next()
                    .unwrap();

                line.remove(i);
                i
            };
            self.point.col_byte_i = i;
            self.point.update_col_i(&buffer);
            self.point.prev_col_i = self.point.col_i;

            false
        } else if self.point.line_i > 0 {
            let line = buffer.lines.remove(self.point.line_i);

            self.point.col_byte_i = buffer.lines[self.point.line_i - 1].data.len();
            self.point.line_i -= 1;
            self.point.update_col_i(&buffer);
            self.point.prev_col_i = self.point.col_i;

            buffer.lines[self.point.line_i].push_str(&line.data);

            for changed_or_moved_line in &mut buffer.lines[self.point.line_i..] {
                changed_or_moved_line.changed = true;
            }
            false
        } else {
            self.point.prev_col_i = self.point.col_i;

            true
        }
    }

    fn insert_new_line(&mut self) {
        let mut buffer = self.buffer.borrow_mut();
        let rest = buffer.lines[self.point.line_i].data.split_off(self.point.col_byte_i);

        buffer.lines.insert(self.point.line_i + 1, Line::new(rest));

        for changed_or_moved_line in &mut buffer.lines[self.point.line_i..] {
            changed_or_moved_line.changed = true;
        }

        self.point.line_i += 1;
        self.point.col_byte_i = 0;
        self.point.col_i = 0;
        self.point.prev_col_i = 0;
    }

    /// Insert tab or spaces
    fn insert_tab(&mut self) {
        if TAB_INSERTS_SPACES {
            let spaces = vec![' ' as u8; TAB_WIDTH];
            self.insert_str_at_point(str::from_utf8(&spaces).unwrap());
        } else {
            self.insert_char_at_point('\t');
        }
    }

    fn save_file(&self) {
        fn write_lines_to_file(f: fs::File, lines: &[Line]) -> io::Result<()> {
            let mut bw = io::BufWriter::new(f);

            let (fst, rest) = lines.split_first().unwrap();

            bw.write_all(fst.data.as_bytes())?;
            for l in rest {
                bw.write_all("\n".as_bytes()).and_then(|_| bw.write_all(l.data.as_bytes()))?;
            }
            Ok(())
        }

        let buffer = self.buffer.borrow();
        match buffer.filepath {
            Some(ref p) => fs::File::create(p)
                .and_then(|f| write_lines_to_file(f, &buffer.lines))
                .expect("Failed to write buffer to file"),
            None => unimplemented!(),
        }
    }

    /// Position the view of the window such that the pointer is in view.
    fn reposition_view(&mut self) {
        let buf = self.buffer.borrow();
        let ver_margin = max(1, (self.h as f32 * SCROLL_MARGIN_V) as i32);
        let margin_top = self.top as i32 + ver_margin;
        let margin_bot = self.top as i32 + self.h as i32 - ver_margin;
        let move_relative_up = min(self.point.line_i as i32 - margin_top, 0);
        let move_relative_down = max(self.point.line_i as i32 - margin_bot, 0) + move_relative_up;
        let buf_bot = buf.lines.len();

        let new_view_top = min(buf_bot,
                               max(0, self.top as i32 + move_relative_down) as usize);
        if new_view_top != self.top {
            self.changed = true
        }
        self.top = new_view_top;

        let hor_margin = max(1, (self.w as f32 * SCROLL_MARGIN_H) as i32);
        let margin_left = self.left as i32 + hor_margin;
        let margin_right = self.left as i32 + self.w as i32 - hor_margin;
        let move_relative_left = min(self.point.col_i as i32 - margin_left, 0);
        let move_relative_right = max(self.point.col_i as i32 - margin_right, 0) +
                                  move_relative_left;
        let rightmost_col = buf.lines[self.top..]
            .iter()
            .take(self.h as usize)
            .map(|l| display_width(&l.data))
            .max()
            .unwrap_or(0);

        let new_view_left = min(rightmost_col,
                                max(0, self.left as i32 + move_relative_right) as usize);
        if new_view_left != self.left {
            self.changed = true
        }
        self.left = new_view_left;
    }

    fn set_size(&mut self, w: u16, h: u16) {
        self.w = w;
        self.h = h;
    }

    /// Get the absolute position in terminal cells of this window in the frame
    fn pos_in_frame(&self) -> (u16, u16) {
        let parent = self.parent_partition.upgrade().unwrap();
        let parent_b = parent.borrow();
        parent_b.pos_in_frame()
    }

    fn split<F: FnOnce(Rc<RefCell<WindowPartition>>, Rc<RefCell<WindowPartition>>)
                       -> _WindowPartition,
             G: FnOnce(u16, u16) -> ((u16, u16), (u16, u16))>
            (&mut self, split_dir: F, new_sizes: G) {
        let ((first_w, first_h), (second_w, second_h)) = new_sizes(self.w, self.h);
        self.changed = true;

        let second_window =
            Rc::new(RefCell::new(Window { w: second_w, h: second_h, ..self.clone() }));

        self.w = first_w;
        self.h = first_h;

        let parent_rc = self.parent_partition.upgrade().unwrap();
        let mut parent = parent_rc.borrow_mut();

        let first_partition = Rc::new(RefCell::new((*parent).clone()));
        self.parent_partition = Rc::downgrade(&first_partition);

        let second_partition = Rc::new(RefCell::new(WindowPartition {
            content: _WindowPartition::Window(second_window.clone()),
            changed: true,
            parent: Some(Rc::downgrade(&parent_rc)),
            is_first_child: false,
        }));
        second_window.borrow_mut().parent_partition = Rc::downgrade(&second_partition);

        parent.content = split_dir(first_partition, second_partition);
        parent.changed = true;
    }

    fn split_h(&mut self) {
        self.split(_WindowPartition::SplitH,
                   |w, h| ((w / 2, h), ((w - 1) / 2, h)))
    }

    fn split_v(&mut self) {
        self.split(_WindowPartition::SplitV, |w, h| ((w, h / 2), (w, h / 2)))
    }

    fn redraw_at(&mut self,
                 term: &mut RawTerminal<Stdout>,
                 start_x: u16,
                 start_y: u16,
                 redraw_all: bool)
                 -> Result<(), io::Error> {
        let redraw_all = redraw_all || self.changed;

        let tab_spaces = String::from_utf8(vec![' ' as u8; TAB_WIDTH]).unwrap();
        let mut buffer = self.buffer.borrow_mut();
        let (top, h) = (self.top, self.h);

        let line_in_view = |line_i| line_i >= top && line_i <= top + (h - 1) as usize;
        let should_redraw_line = |line: &Line| (redraw_all || line.changed);

        write!(term,
               "{}{}{}",
               cursor::Goto(start_x, start_y),
               *COLOR_BG,
               *COLOR_TEXT)?;

        for (_, draw_y, line) in
            buffer.lines
                  .iter_mut()
                  .enumerate()
                  .skip_while(|&(i, _)| !line_in_view(i))
                  .enumerate()
                  .map(|(y, (i, l))| (i, start_y + y as u16, l))
                  .take_while(|&(i, _, _)| line_in_view(i))
                  .filter(|&(_, _, &mut ref l)| should_redraw_line(l)) {

            write!(term, "{}", cursor::Goto(start_x, draw_y))?;

            let view_left_col_i = self.left;
            let grapheme_in_view = |end_col_i| {
                let right = view_left_col_i + self.w as usize;
                end_col_i > view_left_col_i && end_col_i <= right
            };
            let mut last_col = 0;
            for (end_col, g) in line.data
                                    .graphemes(true)
                                    .map(|g| if g == "\t" { &tab_spaces } else { g })
                                    .scan(0, |end_col, g| {
                                        *end_col += display_width(g);
                                        Some((*end_col, g))
                                    })
                                    .skip_while(|&(end_col, _)| !grapheme_in_view(end_col)) {
                if !grapheme_in_view(end_col) {
                    break;
                }
                write!(term, "{}", g)?;
                last_col = end_col;
            }
            write!(term,
                   "{}",
                   (last_col..(self.left + self.w as usize)).map(|_| ' ').collect::<String>())?;
            line.changed = false;
        }

        let clear_line = (0..self.w).map(|_| ' ').collect::<String>();
        let bot_line_y = start_y + (buffer.lines.len() - self.top) as u16;
        for y in bot_line_y..h {
            write!(term, "{}{}", cursor::Goto(start_x, y), &clear_line)?;
        }

        self.changed = false;
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum _WindowPartition {
    Window(Rc<RefCell<Window>>),
    SplitH(Rc<RefCell<WindowPartition>>, Rc<RefCell<WindowPartition>>),
    SplitV(Rc<RefCell<WindowPartition>>, Rc<RefCell<WindowPartition>>),
}

#[derive(Debug, Clone)]
struct WindowPartition {
    content: _WindowPartition,
    changed: bool,
    parent: Option<Weak<RefCell<WindowPartition>>>,
    is_first_child: bool,
}

impl WindowPartition {
    fn new_root_window(buffer: Rc<RefCell<Buffer>>)
                       -> (Rc<RefCell<WindowPartition>>, Rc<RefCell<Window>>) {
        let window = Rc::new(RefCell::new(Window::new(0, 0, buffer, Weak::new())));
        let part = Rc::new(RefCell::new(WindowPartition {
            content: _WindowPartition::Window(window.clone()),
            changed: true,
            parent: None,
            is_first_child: true,
        }));

        window.borrow_mut().parent_partition = Rc::downgrade(&part);

        (part, window)
    }
    fn size(&self) -> (u16, u16) {
        match self.content {
            _WindowPartition::Window(ref window) => {
                let window = window.borrow();
                (window.w, window.h)
            }
            _WindowPartition::SplitH(ref left, ref right) => {
                let ((l_w, h), (r_w, _)) = (left.borrow().size(), right.borrow().size());
                (l_w + r_w, h)
            }
            _WindowPartition::SplitV(ref top, ref bot) => {
                let ((w, t_h), (_, b_h)) = (top.borrow().size(), bot.borrow().size());
                (w, t_h + b_h)
            }
        }
    }

    fn set_size(&self, w: u16, h: u16) {
        assert!(w >= 2 && h >= 2, "Tried to set window size < 2");

        let (prev_w, prev_h) = self.size();
        match self.content {
            _WindowPartition::Window(ref window) => window.borrow_mut().set_size(w, h),
            _WindowPartition::SplitH(ref left, ref right) => {
                let (left, right) = (left.borrow_mut(), right.borrow_mut());
                let l_w = left.size().0;
                let new_l_w = ((l_w as f32 / prev_w as f32) * w as f32) as u16;
                let new_r_w = w - new_l_w - 1;
                left.set_size(new_l_w, h);
                right.set_size(new_r_w, h);
            }
            _WindowPartition::SplitV(ref top, ref bot) => {
                let (top, bot) = (top.borrow_mut(), bot.borrow_mut());
                let t_h = top.size().0;
                let new_t_h = ((t_h as f32 / prev_h as f32) * h as f32) as u16;
                let new_b_h = h - new_t_h;
                top.set_size(w, new_t_h);
                bot.set_size(w, new_b_h);
            }
        }
    }

    /// Get the absolute position in terminal cells of this window partition in the frame
    fn pos_in_frame(&self) -> (u16, u16) {
        if let Some(ref parent_weak) = self.parent {
            let parent_rc = parent_weak.upgrade().unwrap();
            let parent = parent_rc.borrow();
            let (parent_x, parent_y) = parent.pos_in_frame();

            match parent.content {
                _WindowPartition::Window(..) => (parent_x, parent_y),
                _WindowPartition::SplitH(ref first, _) => if self.is_first_child {
                    (parent_x, parent_y)
                } else {
                    (parent_x + 1 + first.borrow().size().0, parent_y)
                },
                _WindowPartition::SplitV(ref first, _) => if self.is_first_child {
                    (parent_x, parent_y)
                } else {
                    (parent_x, parent_y + first.borrow().size().1)
                },
            }
        } else {
            (0, 0)
        }
    }

    fn redraw_at(&self,
                 term: &mut RawTerminal<Stdout>,
                 x: u16,
                 y: u16,
                 redraw_all: bool)
                 -> Result<(), io::Error> {
        let redraw_all = redraw_all || self.changed;
        match self.content {
            _WindowPartition::Window(ref window) => window.borrow_mut()
                                                          .redraw_at(term, x, y, redraw_all),
            _WindowPartition::SplitH(ref left, ref right) => {
                let left = left.borrow();
                let (l_w, l_h) = left.size();

                left.redraw_at(term, x, y, redraw_all)?;
                let r = right.borrow().redraw_at(term, x + l_w + 1, y, redraw_all);

                if redraw_all {
                    write!(term, "{}", *COLOR_DIM_TEXT)?;
                    for y2 in y..(y + l_h) {
                        write!(term, "{}|", cursor::Goto(x + l_w, y2))?;
                    }
                    r.and(write!(term, "{}", *COLOR_TEXT))
                } else {
                    r
                }
            }
            _WindowPartition::SplitV(ref top, ref bot) => {
                let top = top.borrow();
                let t_h = top.size().1;

                top.redraw_at(term, x, y, redraw_all)?;
                bot.borrow().redraw_at(term, x, y + t_h, redraw_all)
            }
        }
    }
}

/// A frame of windows into buffers
#[derive(Debug)]
struct Frame {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// Windows into the buffers to show in this frame
    windows: Rc<RefCell<WindowPartition>>,
    /// The index of the active window in `self.windows`
    _active_window: Rc<RefCell<Window>>,
}

impl Frame {
    fn new(buffer: Rc<RefCell<Buffer>>) -> Frame {
        let (windows, window) = WindowPartition::new_root_window(buffer);
        Frame {
            w: 0,
            h: 0,
            windows: windows,
            _active_window: window,
        }
    }

    fn active_window(&self) -> Ref<Window> {
        self._active_window.borrow()
    }

    fn active_window_mut(&self) -> RefMut<Window> {
        self._active_window.borrow_mut()
    }

    fn resize(&mut self, w: u16, h: u16) {
        self.w = w;
        self.h = h;
        self.windows.borrow().set_size(w, h)
    }

    /// Check for change in terminal size. Update sizes as necessary
    ///
    /// Returns whether terminal size has changed
    fn update_term_size(&mut self) -> bool {
        let (w, h) = termion::terminal_size().expect("Failed to get terminal size");
        let changed = self.w != w && self.h != h;

        if changed {
            self.resize(w, h);
            self.active_window_mut().reposition_view();
        }
        changed
    }

    fn redraw(&mut self,
              term: &mut RawTerminal<Stdout>,
              redraw_all: bool)
              -> Result<(), io::Error> {
        let term_size_changed = self.update_term_size();
        let redraw_all = redraw_all || term_size_changed;

        self.windows.borrow().redraw_at(term, 1, 1, redraw_all)
    }
}

struct TedTui {
    buffers: HashMap<String, Rc<RefCell<Buffer>>>,
    global_keymap: Keymap,
    frame: Frame,
    term: RawTerminal<Stdout>,
}

impl TedTui {
    fn new() -> Self {
        let mut buffers = HashMap::new();
        let scratch = Rc::new(RefCell::new(Buffer::new()));
        buffers.insert("*scratch*".to_string(), scratch.clone());

        let mut keymap = Keymap::new();
        keymap.insert(Key::Ctrl('f'), Cmd::Forward);
        keymap.insert(Key::Ctrl('b'), Cmd::Backward);
        keymap.insert(Key::Ctrl('n'), Cmd::Downward);
        keymap.insert(Key::Ctrl('p'), Cmd::Upward);
        keymap.insert(Key::Ctrl('d'), Cmd::DeleteForward);
        keymap.insert(Key::Delete, Cmd::DeleteForward);
        keymap.insert(Key::Ctrl('h'), Cmd::DeleteBackward);
        keymap.insert(Key::Backspace, Cmd::DeleteBackward);
        keymap.insert(Key::Char('\n'), Cmd::Newline);
        keymap.insert(Key::Char('\t'), Cmd::Tab);
        keymap.insert(Key::Ctrl('s'), Cmd::Save);
        keymap.insert(Key::Alt('g'), Cmd::GoToLine);
        keymap.insert(Key::Esc, Cmd::Exit);
        keymap.insert(Key::Ctrl('z'), Cmd::SplitV);
        keymap.insert(Key::Ctrl('x'), Cmd::SplitH);

        TedTui {
            buffers: buffers,
            global_keymap: keymap,
            frame: Frame::new(scratch),
            term: stdout().into_raw_mode().unwrap(),
        }
    }

    fn active_window(&self) -> Ref<Window> {
        self.frame.active_window()
    }

    fn active_window_mut(&self) -> RefMut<Window> {
        self.frame.active_window_mut()
    }

    fn active_buffer(&self) -> Rc<RefCell<Buffer>> {
        self.active_window().buffer.clone()
    }

    /// Switch to buffer `name` in the active view
    fn switch_to_buffer(&mut self, name: &str) {
        let buffer = self.buffers
                         .entry(name.to_string())
                         .or_insert(Rc::new(RefCell::new(Buffer::new())))
                         .clone();
        self.active_window_mut().buffer = buffer;
    }

    fn open_file(&mut self, name: &str) {
        let filepath = fs::canonicalize(name).expect("File does not exist");
        let reader = io::BufReader::new(fs::File::open(&filepath)
            .expect("File could not be opened"));
        let file_lines = reader.lines()
                               .map(|result| result.map(|s| Line::new(s)))
                               .collect::<Result<Vec<_>, _>>()
                               .expect("File contains invalid utf8 data and cannot be displayed");

        let buffer = Rc::new(RefCell::new(Buffer::new_file_buffer(filepath)));
        buffer.borrow_mut().lines =
            if !file_lines.is_empty() { file_lines } else { vec![Line::new(String::new())] };

        self.buffers.insert(name.to_string(), buffer);
        self.switch_to_buffer(name)
    }

    fn split_h(&self) {
        self.active_window_mut().split_h();
    }

    fn split_v(&mut self) {
        self.active_window_mut().split_v()
    }

    /// Returns true if exit
    fn eval(&mut self, cmd: &Cmd) -> bool {
        enum MoveRes {
            Beg,
            End,
            Ok,
        }

        let mut r = MoveRes::Ok;
        match *cmd {
            Cmd::Insert(c) => {
                self.active_window_mut().insert_char_at_point(c);
            }
            Cmd::Forward => if self.active_window_mut().move_point_forward(1) {
                r = MoveRes::End
            },
            Cmd::Backward => if self.active_window_mut().move_point_backward(1) {
                r = MoveRes::Beg
            },
            Cmd::Downward => if self.active_window_mut().move_point_v(1) {
                r = MoveRes::End
            },
            Cmd::Upward => if self.active_window_mut().move_point_v(-1) {
                r = MoveRes::Beg
            },
            Cmd::DeleteForward => if self.active_window_mut().delete_forward_at_point() {
                r = MoveRes::End
            },
            Cmd::DeleteBackward => if self.active_window_mut().delete_backward_at_point() {
                r = MoveRes::Beg
            },
            Cmd::Newline => self.active_window_mut().insert_new_line(),
            Cmd::Tab => self.active_window_mut().insert_tab(),
            Cmd::Exit => {
                return true;
            }
            Cmd::GoToLine => unimplemented!(),
            Cmd::Save => self.active_window().save_file(),
            Cmd::SplitH => self.split_h(),
            Cmd::SplitV => self.split_v(),
        }
        match r {
            MoveRes::Beg => self.message("Beginning of buffer"),
            MoveRes::End => self.message("End of buffer"),
            MoveRes::Ok => (),
        }
        self.active_window_mut().reposition_view();
        false
    }

    /// Get the message buffer
    fn message_buffer_mut(&mut self) -> RefMut<Buffer> {
        self.buffers
            .entry("*messages*".to_string())
            .or_insert(Rc::new(RefCell::new(Buffer::new())))
            .borrow_mut()
    }

    /// Write a message to the *message* buffer
    fn message<S: Into<String>>(&mut self, msg: S) {
        self.message_buffer_mut().lines.push(Line::new(msg.into()));
    }

    /// Returns whether to exit
    fn handle_key_event(&mut self, key: Key) -> bool {
        let mapped = {
            let active_buffer = self.active_buffer();
            let active_buffer_b = active_buffer.borrow();
            self.global_keymap.get(&key).or(active_buffer_b.local_keymap.get(&key)).cloned()
        };
        if let Some(cmd) = mapped {
            self.eval(&cmd)
        } else if let Key::Char(c) = key {
            self.eval(&Cmd::Insert(c))
        } else {
            self.message(format!("Key {} is undefined", ted_key_to_string(key)));
            false
        }
    }

    /// Return whether to exit
    fn handle_event(&mut self, event: TuiEvent) -> bool {
        match event {
            TuiEvent::Key(k) => {
                let exit = self.handle_key_event(k);
                if exit {
                    return true;
                }
            }
            TuiEvent::Update => (),
            TuiEvent::Mouse(_) => {
                self.message(format!("Mousevent is undefined"));
            }
            TuiEvent::Unsupported(u) => {
                self.message(format!("Unsupported event {:?}", u));
            }
        }
        self.redraw(false);
        false
    }

    fn cursor_pos(&self) -> (u16, u16) {
        let active = self.active_window();
        let cursor_x_relative_window = (active.point.col_i - active.left) as u16;
        let cursor_y_relative_window = (active.point.line_i - active.top) as u16;
        let (window_x_absolute, window_y_absolute) = active.pos_in_frame();
        (1 + window_x_absolute + cursor_x_relative_window,
         1 + window_y_absolute + cursor_y_relative_window)
    }

    fn redraw(&mut self, redraw_all: bool) {
        let (cursor_x, cursor_y) = self.cursor_pos();

        println_err!("Cursor pos {}, {}", cursor_x, cursor_y);

        let r1 = write!(self.term, "{}", cursor::Hide);
        let r2 = self.frame.redraw(&mut self.term, redraw_all);
        let r3 = write!(self.term,
                        "{}{}",
                        cursor::Show,
                        cursor::Goto(cursor_x, cursor_y));
        self.term.flush();
        r1.and(r2).and(r3).expect("Redraw failed");
    }
}

enum TuiEvent {
    Update,
    Key(Key),
    Mouse(MouseEvent),
    Unsupported(Vec<u8>),
}

impl From<Event> for TuiEvent {
    fn from(e: Event) -> TuiEvent {
        match e {
            Event::Key(k) => TuiEvent::Key(k),
            Event::Mouse(m) => TuiEvent::Mouse(m),
            Event::Unsupported(u) => TuiEvent::Unsupported(u),

        }
    }
}

/// Iterator for event loop of terminal events
struct TuiEvents {
    event_it: Box<Iterator<Item = Option<Result<TuiEvent, io::Error>>>>,
}

impl TuiEvents {
    fn new(update_rate_hz: u64) -> Self {
        let (tx, term_read_event_rx) = channel(0);

        thread::spawn(|| {
            let mut tx = tx.wait();
            while let Some(read_event_result) = stdin().events().next() {
                tx.send(Some(read_event_result.map(TuiEvent::from)))
                  .expect("Failed to send on channel");
            }
            tx.send(None).expect("Failed to send on channel");
        });

        let update_period = Duration::from_millis(1000 / update_rate_hz);

        let timer = Timer::default();
        let update_event_stream =
            timer.interval(update_period).map(|_| Some(Ok(TuiEvent::Update))).map_err(|_| ());

        let event_it = term_read_event_rx.select(update_event_stream).wait().map(|r| r.unwrap());

        TuiEvents { event_it: Box::new(event_it) as Box<Iterator<Item = _>> }
    }
}

impl Iterator for TuiEvents {
    type Item = TuiEvent;

    fn next(&mut self) -> Option<Self::Item> {
        self.event_it.next().unwrap().map(|result| result.expect("Failed to read terminal event"))
    }
}

/// Start editor in terminal user interface mode
fn start_tui(opt_filename: Option<&str>) {
    let mut ted = TedTui::new();

    if let Some(filename) = opt_filename {
        ted.open_file(filename);
    }
    ted.redraw(true);

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in TuiEvents::new(5) {
        let exit = ted.handle_event(event);
        if exit {
            break;
        }
    }

    println_err!("Exiting...");

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().stop().expect("Failed to stop profiler");
}

fn main() {
    let matches = App::new("TedWM")
        .version(crate_version!())
        .author("Johan \"Bryal\" J. <96.bryal@gmail.com>")
        .about("A Text Editor + Window Manager inspired by Emacs and EXWM")
        .arg(Arg::with_name("tui-mode")
            .short("t")
            .long("tui-mode")
            .help("Don't open in new GUI window, instead run in Terminal User Interface mode"))
        .arg(Arg::with_name("file").help("File to edit"))
        .get_matches();

    if matches.is_present("tui-mode") {
        // Start in terminal mode
        start_tui(matches.value_of("file"));
    } else {
        // Start GUI mode. Vulkan?
        unimplemented!();
    }
}
