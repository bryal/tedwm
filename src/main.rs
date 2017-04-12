#[macro_use]
extern crate clap;
extern crate termion;
extern crate unicode_width;
extern crate unicode_segmentation;
extern crate futures;
extern crate tokio_timer;
#[macro_use]
extern crate lazy_static;
extern crate sequence_trie;

#[cfg(feature = "profiling")]
extern crate cpuprofiler;

use clap::{Arg, App};

#[cfg(feature = "profiling")]
use cpuprofiler::PROFILER;
use futures::{Sink, Stream};
use futures::sync::mpsc::channel;
use sequence_trie::SequenceTrie;
use std::{str, thread, fs, mem};
use std::cell::{RefCell, RefMut, Ref};
use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::{self, Write, stdout, stdin, Stdout, BufRead};
use std::iter::{once, repeat};
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
    static ref COLOR_BG_MODELINE: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x4B, 0x4B, 0x40));
    static ref COLOR_BG_MODELINE_INACTIVE: color::Bg<color::Rgb> =
        color::Bg(color::Rgb(0x29, 0x29, 0x22));
}

/// The width in columns of a string if it is displayed in a terminal.
///
/// Takes into account the current configuration of tab-width
fn display_width(s: &str) -> usize {
    use unicode_width::UnicodeWidthStr;
    s.graphemes(true).map(|g| if g == "\t" { TAB_WIDTH } else { g.width() }).sum()
}


fn n_spaces(n: usize) -> String {
    (0..n).map(|_| ' ').collect::<String>()
}

/// Pad a line in the editor with spaces so that the number of symbols when printed will equal `len`
fn pad_line(mut s: String, len: u16) -> String {
    let padding = n_spaces(max(0, len as isize - display_width(&s) as isize) as usize);
    s.push_str(&padding);
    s
}

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

/// Formatting of key events
fn ted_key_seq_to_string(ks: &[Key]) -> String {
    let (first, rest) = ks.split_first().expect("Key slice empty");

    let mut k_s = ted_key_to_string(first.clone());

    for k in rest.iter().cloned() {
        k_s.push(' ');
        k_s.push_str(&ted_key_to_string(k))
    }
    k_s
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
    /// Delete the active window
    DeleteWindow,
    /// Delete all windows except active window
    DeleteOtherWindows,
    /// Select the nth window following or preceding the active window cyclically
    OtherWindow(isize),
    /// Cancel a multi-key stroke or prompting command
    Cancel,
}

type Keymap = SequenceTrie<Key, Cmd>;

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
}

impl Line {
    fn new(s: String) -> Line {
        Line { data: s }
    }

    fn insert_str(&mut self, i: usize, s: &str) {
        self.data.insert_str(i, s);
    }

    fn push_str(&mut self, s: &str) {
        self.data.push_str(s);
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

/// Describes whether a move was prevented by the point being at the
/// beginning or end of buffer
enum MoveRes {
    Beg,
    End,
    Ok,
}

/// A window into a buffer
#[derive(Debug, Clone)]
struct Window {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// The index of the leftmost column of the buffer visible in the window
    left: usize,
    /// The index of the topmost line of the buffer visible in the window
    top: usize,
    parent_partition: Weak<RefCell<Partition>>,
    buffer: Rc<RefCell<Buffer>>,
    point: Point,
    /// Whether this is the active window
    is_active: bool,
}

impl Window {
    fn new(w: u16,
           h: u16,
           buffer: Rc<RefCell<Buffer>>,
           parent: Weak<RefCell<Partition>>)
           -> Window {
        Window {
            w: w,
            h: h,
            left: 0,
            top: 0,
            parent_partition: parent,
            buffer: buffer,
            point: Point::new(),
            is_active: false,
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

    /// Move point horizontally by `n` graphemes
    ///
    /// Returns whether move was prevented by beginning/end of buffer
    fn move_point_h(&mut self, mut n_with_dir: isize) -> MoveRes {
        let buffer = self.buffer.borrow();

        let forward = n_with_dir >= 0;
        let mut n = n_with_dir.abs() as usize;

        let mut line = &buffer.lines[self.point.line_i];

        loop {
            let grapheme_byte_is = if forward {
                let offset = self.point.col_byte_i;

                Box::new(line.data[self.point.col_byte_i..]
                    .grapheme_indices(true)
                    .map(move |(i, _)| i + offset)
                    .chain(once(line.data.len()))) as Box<Iterator<Item = _>>
            } else {
                Box::new(line.data[0..self.point.col_byte_i]
                    .grapheme_indices(true)
                    .map(|(i, _)| i)
                    .chain(once(self.point.col_byte_i))
                    .rev()) as Box<Iterator<Item = _>>
            };

            for i in grapheme_byte_is {
                if n == 0 {
                    self.point.col_byte_i = i;
                    self.point.update_col_i(&buffer);
                    self.point.prev_col_i = self.point.col_i;

                    return MoveRes::Ok;
                } else {
                    n -= 1;
                }
            }
            if forward {
                if self.point.line_i + 1 >= buffer.lines.len() {
                    return MoveRes::End;
                }
                self.point.line_i += 1;
                line = &buffer.lines[self.point.line_i];
                self.point.col_byte_i = 0;
            } else {
                if self.point.line_i == 0 {
                    return MoveRes::Beg;
                }
                self.point.line_i -= 1;
                line = &buffer.lines[self.point.line_i];
                self.point.col_byte_i = line.data.len();
            }
        }
    }

    /// Move point upward (negative) or downward (positive) by `n` lines
    ///
    /// Returns whether move was prevented by beginning/end of buffer
    fn move_point_v(&mut self, n: isize) -> MoveRes {
        let buffer = self.buffer.borrow();
        let n_lines = buffer.lines.len();

        let move_res = if self.point.line_i as isize + n >= n_lines as isize {
            self.point.line_i = n_lines - 1;
            MoveRes::End
        } else if (self.point.line_i as isize) + n < 0 {
            self.point.line_i = 0;
            MoveRes::Beg
        } else {
            self.point.line_i = (self.point.line_i as isize + n) as usize;
            MoveRes::Ok
        };

        let line = &buffer.lines[self.point.line_i].data;
        self.point.col_i = 0;
        self.point.col_byte_i = 0;
        for (i, g) in line.grapheme_indices(true) {
            let w = display_width(g);
            if self.point.col_i + w > self.point.prev_col_i {
                self.point.col_byte_i = i;
                return move_res;
            }
            self.point.col_i += w;
        }
        self.point.col_byte_i = line.len();
        move_res
    }

    // TODO: More generic deletion.
    //       Something like `delete-selection`

    /// Delete the grapheme infront of the cursor
    ///
    /// Returns whether deletion was prevented by end of buffer
    fn delete_forward_at_point(&mut self) -> MoveRes {
        let mut buffer = self.buffer.borrow_mut();
        let n_lines = buffer.lines.len();

        self.point.prev_col_i = self.point.col_i;

        if self.point.col_byte_i < buffer.lines[self.point.line_i].data.len() {
            let line = &mut buffer.lines[self.point.line_i].data;

            let grapheme_len = line[self.point.col_byte_i..].graphemes(true).next().unwrap().len();
            let (start, end) = (self.point.col_byte_i, self.point.col_byte_i + grapheme_len);

            line.drain(start..end);

            MoveRes::Ok
        } else if self.point.line_i < n_lines - 1 {
            let next_line = buffer.lines.remove(self.point.line_i + 1);
            buffer.lines[self.point.line_i].push_str(&next_line.data);

            MoveRes::Ok
        } else {
            MoveRes::End
        }
    }

    /// Delete the grapheme right behind the cursor
    ///
    /// Returns whether deletion was prevented by beginning of buffer
    fn delete_backward_at_point(&mut self) -> MoveRes {
        let mut buffer = self.buffer.borrow_mut();

        if self.point.col_i > 0 {
            let i = {
                let line = &mut buffer.lines[self.point.line_i];
                let (i, len) = line.data[0..self.point.col_byte_i]
                    .grapheme_indices(true)
                    .rev()
                    .next()
                    .map(|(i, g)| (i, g.len()))
                    .unwrap();

                line.data.drain(i..(i + len));
                i
            };
            self.point.col_byte_i = i;
            self.point.update_col_i(&buffer);
            self.point.prev_col_i = self.point.col_i;

            MoveRes::Ok
        } else if self.point.line_i > 0 {
            let line = buffer.lines.remove(self.point.line_i);

            self.point.col_byte_i = buffer.lines[self.point.line_i - 1].data.len();
            self.point.line_i -= 1;
            self.point.update_col_i(&buffer);
            self.point.prev_col_i = self.point.col_i;

            buffer.lines[self.point.line_i].push_str(&line.data);

            MoveRes::Ok
        } else {
            self.point.prev_col_i = self.point.col_i;
            MoveRes::Beg
        }
    }

    fn insert_new_line(&mut self) {
        let mut buffer = self.buffer.borrow_mut();
        let rest = buffer.lines[self.point.line_i].data.split_off(self.point.col_byte_i);

        buffer.lines.insert(self.point.line_i + 1, Line::new(rest));

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
        self.left = new_view_left;
    }

    fn set_size(&mut self, w: u16, h: u16) {
        self.w = w;
        self.h = h;
    }

    /// Get the absolute position in terminal cells of this window in the frame
    fn pos_in_frame(&self) -> (u16, u16) {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.pos_in_frame()
    }

    fn split<F: FnOnce(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>) -> _Partition,
             G: FnOnce(u16, u16) -> ((u16, u16), (u16, u16))>
        (&mut self,
         split_dir: F,
         new_sizes: G) {

        let ((first_w, first_h), (second_w, second_h)) = new_sizes(self.w, self.h);

        self.set_size(first_w, first_h);

        let second_window = Rc::new(RefCell::new(Window {
            w: second_w,
            h: second_h,
            is_active: false,
            ..self.clone()
        }));

        let parent_rc = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let mut parent = parent_rc.borrow_mut();

        let first_partition = Rc::new(RefCell::new((*parent).clone()));

        {
            let mut first_b = first_partition.borrow_mut();
            first_b.parent = Some(Rc::downgrade(&parent_rc));
            first_b.is_first_child = true;
        }

        self.parent_partition = Rc::downgrade(&first_partition);

        let second_partition = Rc::new(RefCell::new(Partition {
            content: _Partition::Window(second_window.clone()),
            parent: Some(Rc::downgrade(&parent_rc)),
            is_first_child: false,
        }));
        second_window.borrow_mut().parent_partition = Rc::downgrade(&second_partition);

        parent.content = split_dir(first_partition, second_partition);
    }

    fn split_h(&mut self) {
        self.split(_Partition::SplitH, |w, h| ((w / 2, h), ((w - 1) / 2, h)))
    }

    fn split_v(&mut self) {
        self.split(_Partition::SplitV, |w, h| ((w, (h + 1) / 2), (w, h / 2)))
    }

    /// Delete this window from parent and unsplit the partition
    ///
    /// If this window was the only child, do not delete and return `None`,
    /// otherwise, return the first sibling
    fn delete(&self) -> Option<Rc<RefCell<Window>>> {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.delete().map(|part| part.borrow().first_window())
    }

    /// Select the `n`th window following or preceding this window
    /// in cyclic ordering
    fn other_window(&self, n: isize) -> Rc<RefCell<Window>> {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let other_leaf_partition = Partition::cycle(parent, n);
        let other_leaf_partition = other_leaf_partition.borrow();
        other_leaf_partition.window().expect("Other partition was not leaf partition")
    }

    fn render_lines(&self) -> Vec<String> {
        fn h_cols(line: &str, left: usize, n_cols: u16) -> String {
            let tab_spaces = n_spaces(TAB_WIDTH);

            line.graphemes(true)
                .scan(0, |col, g| if g == "\t" {
                    *col += TAB_WIDTH;
                    Some((*col, tab_spaces.as_str()))
                } else {
                    *col += display_width(g);
                    Some((*col, g))
                })
                .skip_while(|&(end_col, _)| end_col <= left)
                .take_while(|&(end_col, _)| end_col <= left + n_cols as usize)
                .map(|(_, g)| g)
                .collect()
        }

        let buffer = self.buffer.borrow();

        if buffer.lines.len() < self.top {
            vec![n_spaces(self.w as usize); self.h as usize]
        } else {
            buffer.lines[self.top..]
                .iter()
                .map(|l| h_cols(&l.data, self.left, self.w))
                .map(|l| pad_line(l.clone(), self.w))
                .chain(repeat(n_spaces(self.w as usize)))
                .take(self.h as usize - 1)
                .collect()
        }
    }

    fn render_modeline(&self) -> String {
        let r = self.point.line_i as f32 / max(1, self.buffer.borrow().lines.len() - 1) as f32;
        let s = format!(" {}%", (r * 100.0).round() as u8);
        let bg = if self.is_active { &*COLOR_BG_MODELINE } else { &*COLOR_BG_MODELINE_INACTIVE };
        format!("{}{}{}", bg, pad_line(s, self.w), *COLOR_BG)
    }

    fn render(&self) -> Vec<String> {
        let mut rendering_lines = self.render_lines();
        rendering_lines.push(self.render_modeline());
        rendering_lines
    }
}

#[derive(PartialEq, Eq, Debug)]
struct RenderingSection {
    x: u16,
    y: u16,
    lines: Vec<String>,
}

#[derive(Debug, Clone)]
enum _Partition {
    Window(Rc<RefCell<Window>>),
    SplitH(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
    SplitV(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
}

#[derive(Debug, Clone)]
struct Partition {
    content: _Partition,
    parent: Option<Weak<RefCell<Partition>>>,
    is_first_child: bool,
}

impl Partition {
    fn new_root_from_buffer(buffer: Rc<RefCell<Buffer>>)
                            -> (Rc<RefCell<Partition>>, Rc<RefCell<Window>>) {
        let window = Rc::new(RefCell::new(Window::new(0, 0, buffer, Weak::new())));
        let part = Rc::new(RefCell::new(Partition {
            content: _Partition::Window(window.clone()),
            parent: None,
            is_first_child: true,
        }));

        window.borrow_mut().parent_partition = Rc::downgrade(&part);

        (part, window)
    }

    fn new_root_from_window(window: Rc<RefCell<Window>>) -> Rc<RefCell<Partition>> {
        let part = Rc::new(RefCell::new(Partition {
            content: _Partition::Window(window.clone()),
            parent: None,
            is_first_child: true,
        }));

        window.borrow_mut().parent_partition = Rc::downgrade(&part);

        part
    }

    fn size(&self) -> (u16, u16) {
        match self.content {
            _Partition::Window(ref window) => {
                let window = window.borrow();
                (window.w, window.h)
            }
            _Partition::SplitH(ref left, ref right) => {
                let ((l_w, h), (r_w, _)) = (left.borrow().size(), right.borrow().size());
                (l_w + 1 + r_w, h)
            }
            _Partition::SplitV(ref top, ref bot) => {
                let ((w, t_h), (_, b_h)) = (top.borrow().size(), bot.borrow().size());
                (w, t_h + b_h)
            }
        }
    }

    fn set_size(&self, w: u16, h: u16) {
        assert!(w >= 2 && h >= 2, "Tried to set window size < 2");

        let (prev_w, prev_h) = self.size();
        match self.content {
            _Partition::Window(ref window) => window.borrow_mut().set_size(w, h),
            _Partition::SplitH(ref left, ref right) => {
                let (left, right) = (left.borrow_mut(), right.borrow_mut());
                let (l_w, _) = left.size();
                let new_l_w = ((l_w as f32 / prev_w as f32) * w as f32) as u16;
                let new_r_w = w - new_l_w - 1;
                left.set_size(new_l_w, h);
                right.set_size(new_r_w, h);
            }
            _Partition::SplitV(ref top, ref bot) => {
                let (top, bot) = (top.borrow_mut(), bot.borrow_mut());
                let (_, t_h) = top.size();
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
                _Partition::Window(..) => unreachable!(),
                _Partition::SplitH(ref first, _) => if self.is_first_child {
                    (parent_x, parent_y)
                } else {
                    (parent_x + 1 + first.borrow().size().0, parent_y)
                },
                _Partition::SplitV(ref first, _) => if self.is_first_child {
                    (parent_x, parent_y)
                } else {
                    (parent_x, parent_y + first.borrow().size().1)
                },
            }
        } else {
            (0, 0)
        }
    }

    /// Returns the `n`th leaf partition following or preceding this partition
    fn cycle(this: Rc<RefCell<Partition>>, n: isize) -> Rc<RefCell<Partition>> {
        let this_b = this.borrow();

        if n == 0 || this_b.parent.is_none() {
            Partition::first_leaf(this.clone())
        } else {
            let parent_weak = this_b.parent.as_ref().unwrap();
            let parent = parent_weak.upgrade().expect("Failed to upgrade parent");
            let parent_b = parent.borrow();

            match parent_b.content {
                _Partition::Window(..) => unreachable!(),
                _Partition::SplitV(_, ref second) |
                _Partition::SplitH(_, ref second) if this_b.is_first_child => {
                    Partition::cycle(Partition::first_leaf(second.clone()), n - 1)
                }
                _ => Partition::cycle(parent.clone(), n),
            }
        }
    }

    /// Returns the leftmost, topmost leaf in this partition
    fn first_leaf(this: Rc<RefCell<Partition>>) -> Rc<RefCell<Partition>> {
        let this_b = this.borrow();
        match this_b.content {
            _Partition::Window(_) => this.clone(),
            _Partition::SplitH(ref left, _) => Partition::first_leaf(left.clone()),
            _Partition::SplitV(ref top, _) => Partition::first_leaf(top.clone()),
        }
    }

    /// Returns the leftmost, topmost leaf in this partition
    fn first_window(&self) -> Rc<RefCell<Window>> {
        match self.content {
            _Partition::Window(ref window) => window.clone(),
            _Partition::SplitH(ref left, _) => left.borrow().first_window(),
            _Partition::SplitV(ref top, _) => top.borrow().first_window(),
        }
    }

    fn window(&self) -> Option<Rc<RefCell<Window>>> {
        match self.content {
            _Partition::Window(ref window) => Some(window.clone()),
            _ => None,
        }
    }

    /// Delete this partition from parent and unsplit the partition
    ///
    /// If this partition was the only child, do not delete and return `None`,
    /// otherwise, return the sibling
    fn delete(&self) -> Option<Rc<RefCell<Partition>>> {
        fn set_children_parent(this: &Partition, new_parent: Weak<RefCell<Partition>>) {
            match this.content {
                _Partition::Window(ref window) => window.borrow_mut().parent_partition = new_parent,
                _Partition::SplitH(ref first, ref second) |
                _Partition::SplitV(ref first, ref second) => {
                    first.borrow_mut().parent = Some(new_parent.clone());
                    second.borrow_mut().parent = Some(new_parent);
                }
            }
        }

        if let Some(parent_weak) = self.parent.clone() {
            let parent = parent_weak.upgrade().expect("Failed to upgrade parent");
            let mut parent_b = parent.borrow_mut();
            let (parent_w, parent_h) = parent_b.size();

            let content = unsafe { mem::replace(&mut parent_b.content, mem::uninitialized()) };

            let sibling = match content {
                _Partition::Window(..) => unreachable!(),
                _Partition::SplitV(first, second) |
                _Partition::SplitH(first, second) => if self.is_first_child {
                    second
                } else {
                    first
                },
            };

            let sibling_owned = Rc::try_unwrap(sibling)
                .expect("Sibling was not unique reference")
                .into_inner();

            sibling_owned.set_size(parent_w, parent_h);

            set_children_parent(&sibling_owned, parent_weak);

            parent_b.content = sibling_owned.content;

            Some(parent.clone())
        } else {
            None
        }
    }

    fn render(&self) -> Vec<RenderingSection> {
        match self.content {
            _Partition::Window(ref window) => vec![RenderingSection {
                                                       x: 0,
                                                       y: 0,
                                                       lines: window.borrow().render(),
                                                   }],
            _Partition::SplitH(ref l, ref r) => {
                let l = l.borrow();
                let (l_w, l_h) = l.size();
                let l_renderings = l.render();

                let mut r_renderings = r.borrow().render();
                for r_rendering in &mut r_renderings {
                    r_rendering.x += l_w + 1
                }

                let mut separator_lines =
                    repeat("|".to_string()).take(l_h as usize).collect::<Vec<_>>();
                separator_lines[0].insert_str(0, &COLOR_DIM_TEXT.to_string());
                separator_lines.last_mut()
                               .expect("window partition height is 0")
                               .push_str(&COLOR_TEXT.to_string());
                let separator_rendering = RenderingSection { x: l_w, y: 0, lines: separator_lines };

                l_renderings.into_iter()
                            .chain(once(separator_rendering))
                            .chain(r_renderings)
                            .collect()
            }
            _Partition::SplitV(ref top, ref bot) => {
                let top = top.borrow();
                let top_h = top.size().1;
                let top_renderings = top.render();

                let mut bot_renderings = bot.borrow().render();
                for bot_rendering in &mut bot_renderings {
                    bot_rendering.y += top_h
                }

                top_renderings.into_iter().chain(bot_renderings).collect()
            }
        }
    }
}

struct Minibuffer {
    w: u16,
    window_stack: Vec<Window>,
    _echo: Option<String>,
}

impl Minibuffer {
    fn new(w: u16) -> Minibuffer {
        Minibuffer {
            w: w,
            window_stack: Vec::new(),
            _echo: None,
        }
    }

    fn set_width(&mut self, w: u16) {
        self.w = w;
        for window in &mut self.window_stack {
            window.set_size(w, 2);
        }
    }

    fn echo(&mut self, s: &str) {
        if display_width(s) <= self.w as usize {
            self._echo = Some(s.into())
        } else {
            let mut fitted = s.graphemes(true)
                              .scan(0, |len, g| {
                                  *len += display_width(g);
                                  if *len < self.w as usize - 3 { Some(g) } else { None }
                              })
                              .collect::<String>();
            fitted.push_str("...");

            self._echo = Some(fitted);
        }
    }

    fn clear_echo(&mut self) {
        self._echo = None;
    }

    fn render(&self) -> Vec<String> {
        match (self._echo.clone(), self.window_stack.last()) {
            (Some(s), _) => vec![pad_line(s, self.w)],
            (None, Some(w)) => w.render_lines(),
            (None, None) => vec![n_spaces(self.w as usize)],
        }
    }
}


/// A frame of windows into buffers
struct Frame {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// Windows into the buffers to show in this frame
    windows: Rc<RefCell<Partition>>,
    /// The index of the active window in `self.windows`
    active_window: Rc<RefCell<Window>>,
    minibuffer: Minibuffer,
}

impl Frame {
    fn new(buffer: Rc<RefCell<Buffer>>) -> Frame {
        let (windows, window) = Partition::new_root_from_buffer(buffer);

        let mut f = Frame {
            w: 0,
            h: 0,
            windows: windows,
            active_window: window,
            minibuffer: Minibuffer::new(0),
        };

        f.active_window.borrow_mut().is_active = true;
        f.update_term_size();

        f
    }

    fn resize(&mut self, w: u16, h: u16) {
        self.w = w;
        self.h = h;
        self.windows.borrow().set_size(w, h - 1);
        self.minibuffer.set_width(w);
    }

    /// Check for change in terminal size. Update sizes as necessary
    ///
    /// Returns whether terminal size has changed
    fn update_term_size(&mut self) -> bool {
        let (w, h) = termion::terminal_size().expect("Failed to get terminal size");
        let changed = self.w != w || self.h != h;

        if changed {
            self.resize(w, h);
            self.active_window.borrow_mut().reposition_view();
        }
        changed
    }

    fn render(&mut self) -> Vec<RenderingSection> {
        let mut rendering_sections = self.windows.borrow().render();
        let minibuffer_rendering = RenderingSection {
            x: 0,
            y: self.h - 1,
            lines: self.minibuffer.render(),
        };

        rendering_sections.push(minibuffer_rendering);

        rendering_sections
    }
}

struct TedTui {
    buffers: HashMap<String, Rc<RefCell<Buffer>>>,
    global_keymap: Keymap,
    frame: Frame,
    term: RawTerminal<Stdout>,
    key_seq: Vec<Key>,
    prev_rendering_sections: Vec<RenderingSection>,
}

impl TedTui {
    fn new() -> Self {
        let mut buffers = HashMap::new();
        let scratch = Rc::new(RefCell::new(Buffer::new()));
        buffers.insert("*scratch*".to_string(), scratch.clone());

        let mut keymap = Keymap::new();
        keymap.insert(&[Key::Ctrl('g')], Cmd::Cancel);

        keymap.insert(&[Key::Ctrl('f')], Cmd::Forward);
        keymap.insert(&[Key::Ctrl('b')], Cmd::Backward);
        keymap.insert(&[Key::Ctrl('n')], Cmd::Downward);
        keymap.insert(&[Key::Ctrl('p')], Cmd::Upward);
        keymap.insert(&[Key::Ctrl('d')], Cmd::DeleteForward);
        keymap.insert(&[Key::Delete], Cmd::DeleteForward);
        keymap.insert(&[Key::Ctrl('h')], Cmd::DeleteBackward);
        keymap.insert(&[Key::Backspace], Cmd::DeleteBackward);
        keymap.insert(&[Key::Char('\n')], Cmd::Newline);
        keymap.insert(&[Key::Char('\t')], Cmd::Tab);
        keymap.insert(&[Key::Ctrl('x'), Key::Ctrl('s')], Cmd::Save);
        keymap.insert(&[Key::Alt('g')], Cmd::GoToLine);
        keymap.insert(&[Key::Esc], Cmd::Exit);
        keymap.insert(&[Key::Ctrl('x'), Key::Ctrl('c')], Cmd::Exit);
        keymap.insert(&[Key::Ctrl('x'), Key::Char('2')], Cmd::SplitV);
        keymap.insert(&[Key::Ctrl('x'), Key::Char('3')], Cmd::SplitH);
        keymap.insert(&[Key::Ctrl('x'), Key::Char('0')], Cmd::DeleteWindow);
        keymap.insert(&[Key::Ctrl('x'), Key::Char('1')], Cmd::DeleteOtherWindows);
        keymap.insert(&[Key::Ctrl('x'), Key::Char('o')], Cmd::OtherWindow(1));
        keymap.insert(&[Key::Ctrl('x'), Key::Char('i')], Cmd::OtherWindow(-1));

        TedTui {
            buffers: buffers,
            global_keymap: keymap,
            frame: Frame::new(scratch),
            term: stdout().into_raw_mode().unwrap(),
            key_seq: Vec::new(),
            prev_rendering_sections: Vec::new(),
        }
    }

    fn active_window(&self) -> Ref<Window> {
        self.frame.active_window.borrow()
    }

    fn active_window_mut(&self) -> RefMut<Window> {
        self.frame.active_window.borrow_mut()
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
        self.active_window_mut().split_h()
    }

    fn split_v(&mut self) {
        self.active_window_mut().split_v()
    }

    fn select_other_window(&mut self, n: isize) {
        let next_window = self.frame.active_window.borrow().other_window(n);
        self.frame.active_window.borrow_mut().is_active = false;
        next_window.borrow_mut().is_active = true;
        self.frame.active_window = next_window;
    }

    fn delete_active_window(&mut self) {
        let maybe_sibling = self.frame.active_window.borrow().delete();

        if let Some(sibling) = maybe_sibling {
            sibling.borrow_mut().is_active = true;
            self.frame.active_window = sibling;
        } else {
            self.frame.minibuffer.echo("Can't delete root window")
        }
    }

    fn delete_inactive_windows(&mut self) {
        let active_window = self.frame.active_window.clone();
        self.frame.windows = Partition::new_root_from_window(active_window);
        self.frame.active_window.borrow_mut().set_size(self.frame.w, self.frame.h);
    }

    /// Returns true if exit
    fn eval(&mut self, cmd: &Cmd) -> bool {
        let mut r = MoveRes::Ok;
        match *cmd {
            Cmd::Insert(c) => {
                self.active_window_mut().insert_char_at_point(c);
            }
            Cmd::Forward => r = self.active_window_mut().move_point_h(1),
            Cmd::Backward => r = self.active_window_mut().move_point_h(-1),
            Cmd::Downward => r = self.active_window_mut().move_point_v(1),
            Cmd::Upward => r = self.active_window_mut().move_point_v(-1),
            Cmd::DeleteForward => r = self.active_window_mut().delete_forward_at_point(),
            Cmd::DeleteBackward => r = self.active_window_mut().delete_backward_at_point(),
            Cmd::Newline => self.active_window_mut().insert_new_line(),
            Cmd::Tab => self.active_window_mut().insert_tab(),
            Cmd::Exit => return true,
            Cmd::GoToLine => unimplemented!(),
            Cmd::Save => self.active_window().save_file(),
            Cmd::SplitH => self.split_h(),
            Cmd::SplitV => self.split_v(),
            Cmd::DeleteWindow => self.delete_active_window(),
            Cmd::DeleteOtherWindows => self.delete_inactive_windows(),
            Cmd::OtherWindow(n) => self.select_other_window(n),
            Cmd::Cancel => self.message("Cancel"),
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
        let msg = msg.into();
        self.frame.minibuffer.echo(&msg);
        self.message_buffer_mut().lines.push(Line::new(msg));
    }

    /// Returns whether to exit
    fn handle_key_event(&mut self, key: Key) -> bool {
        /// Keymaps sorted by priority
        fn keymaps_is_prefix(keymaps: &[&Keymap], key_seq: &[Key]) -> bool {
            keymaps.iter()
                   .any(|keymap| !keymap.get_node(key_seq).map(|n| n.is_empty()).unwrap_or(true))
        }
        fn keymaps_get(keymaps: &[&Keymap], key_seq: &[Key]) -> Option<Cmd> {
            keymaps.iter().filter_map(|keymap| keymap.get(key_seq)).next().cloned()
        }

        self.key_seq.push(key);

        let maybe_cmd = {
            let active_buffer = self.active_buffer();
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
                    .echo(&format!("{} ...", ted_key_seq_to_string(&self.key_seq)));
                return false;
            } else if let Key::Char(c) = key {
                self.key_seq.clear();
                Some(Cmd::Insert(c))
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
                    false
                } else {
                    self.eval(&Cmd::Cancel)
                }
            }
            Some(cmd) => self.eval(&cmd),
            None => {
                let s = format!("Key {} is undefined", ted_key_seq_to_string(&self.key_seq));
                self.message(s);
                self.key_seq.clear();
                false
            }
        }
    }

    /// Return whether to exit
    fn handle_event(&mut self, event: TuiEvent) -> bool {
        match event {
            TuiEvent::Key(k) => {
                self.frame.minibuffer.clear_echo();
                let exit = self.handle_key_event(k);
                if exit {
                    return true;
                }
            }
            TuiEvent::Update => (),
            TuiEvent::Mouse(_) => {
                self.frame.minibuffer.clear_echo();
                self.message(format!("Mousevent is undefined"));
            }
            TuiEvent::Unsupported(u) => {
                self.frame.minibuffer.clear_echo();
                self.message(format!("Unsupported event {:?}", u));
            }
        }
        self.frame.update_term_size();
        self.redraw();
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
    event_it: Box<Iterator<Item = TuiEvent>>,
}

impl TuiEvents {
    fn with_update_period(update_period: Duration) -> Self {
        let (tx, term_read_event_rx) = channel(0);

        thread::spawn(|| {
            let mut tx = tx.wait();

            for read_event_result in stdin().events() {
                tx.send(TuiEvent::from(read_event_result.expect("Failed to read terminal event")))
                  .expect("Failed to send on channel");
            }
            panic!("No more events on stdin");
        });

        let timer = Timer::default();
        let update_event_stream =
            timer.interval(update_period).map(|_| TuiEvent::Update).map_err(|_| ());

        let event_it = term_read_event_rx.select(update_event_stream).wait().map(|r| r.unwrap());

        TuiEvents { event_it: Box::new(event_it) as Box<Iterator<Item = _>> }
    }
}

impl Iterator for TuiEvents {
    type Item = TuiEvent;

    fn next(&mut self) -> Option<Self::Item> {
        self.event_it.next()
    }
}

/// Start editor in terminal user interface mode
fn start_tui(opt_filename: Option<&str>) {
    let mut ted = TedTui::new();

    if let Some(filename) = opt_filename {
        ted.open_file(filename);
    }

    write!(ted.term, "{}{}", *COLOR_BG, *COLOR_TEXT).expect("Failed to reset colors");

    ted.redraw();

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in TuiEvents::with_update_period(Duration::from_millis(30)) {
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
