#![feature(slice_patterns, fnbox, inclusive_range_syntax)]

#[macro_use]
extern crate clap;
extern crate itertools;
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

mod seq_set;

use clap::{Arg, App};
#[cfg(feature = "profiling")]
use cpuprofiler::PROFILER;
use futures::{Sink, Stream};
use futures::sync::mpsc::channel;
use itertools::Itertools;
use seq_set::SequenceSet;
use sequence_trie::SequenceTrie;
use std::{str, thread, fs, mem, ptr, process};
use std::boxed::FnBox;
use std::cell::{RefCell, RefMut};
use std::cmp::{max, min, Ord, Ordering};
use std::collections::{HashMap, HashSet};
use std::io::{self, Write, stdout, stdin, Stdout, BufRead};
use std::iter::{once, repeat};
use std::path::{Path, PathBuf};
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
        write!(io::stderr(), "{}\n\r", format!($fmt, $($args),*))
            .expect("Failed to write to stderr")
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
    static ref COLOR_BG_SELECTION: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x3A, 0x3A, 0x32));
    static ref COLOR_DIM_TEXT: color::Fg<color::Rgb> = color::Fg(color::Rgb(0x60, 0x56, 0x50));
    // At horizontal edges, mark a cell if line continues here on scroll
    static ref COLOR_BG_LINE_CONTINUES: color::Bg<color::Rgb> =
       color::Bg(color::Rgb(0xF0, 0x40, 0x70));
    static ref COLOR_BG_MODELINE: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x4B, 0x4B, 0x40));
    static ref COLOR_BG_MODELINE_INACTIVE: color::Bg<color::Rgb> =
        color::Bg(color::Rgb(0x29, 0x29, 0x22));
}

fn modulo(lhs: isize, rhs: usize) -> usize {
    let rhs = rhs as isize;
    (((lhs % rhs) + rhs) % rhs) as usize
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

/// Formatting of key events
fn ted_key_seq_to_string(ks: &[Key]) -> String {
    let (mut s, rest) = match *ks {
        [] => panic!("Key seqyence empty"),
        [Key::Char('\t'), ref rest..] => ("<tab>".to_string(), rest),
        [Key::Char('\n'), ref rest..] => ("<return>".to_string(), rest),
        [Key::Char(c @ '\u{1}'...'\u{1F}'), ref rest..] => {
            (format!("C-{}", (c as u8 - 1 + 'a' as u8) as char), rest)
        }
        [Key::Char(c), ref rest..] => (format!("{}", c), rest),
        [Key::Ctrl(c), ref rest..] => (format!("C-{}", c), rest),
        [Key::Alt(c @ '\u{1}'...'\u{1F}'), ref rest..] => {
            (format!("C-M-{}", (c as u8 - 1 + 'a' as u8) as char), rest)
        }
        [Key::Alt(c), ref rest..] => (format!("M-{}", c), rest),
        [Key::F(n), ref rest..] => (format!("<f{}>", n), rest),
        [Key::Backspace, ref rest..] => ("<backspace>".to_string(), rest),
        [Key::Delete, ref rest..] => ("<delete>".to_string(), rest),
        [Key::Up, ref rest..] => ("<up>".to_string(), rest),
        [Key::Down, ref rest..] => ("<down>".to_string(), rest),
        [Key::Left, ref rest..] => ("<left>".to_string(), rest),
        [Key::Right, ref rest..] => ("<right>".to_string(), rest),
        [Key::Home, ref rest..] => ("<home>".to_string(), rest),
        [Key::End, ref rest..] => ("<end>".to_string(), rest),
        [Key::Insert, ref rest..] => ("<insert>".to_string(), rest),
        [Key::PageDown, ref rest..] => ("<page down>".to_string(), rest),
        [Key::PageUp, ref rest..] => ("<page down>".to_string(), rest),
        [Key::Esc, k @ Key::Char(_), ref rest..] |
        [Key::Esc, k @ Key::Ctrl(_), ref rest..] => (format!("M-{}", ted_key_seq_to_string(&[k])),
                                                     rest),
        [Key::Esc, ref rest..] => ("<escape>".to_string(), rest),
        [Key::Null, ref rest..] => ("<null>".to_string(), rest),
        [Key::__IsNotComplete, ref rest..] => ("UNSUPPORTED".to_string(), rest),
    };

    if rest.len() > 0 {
        s.push(' ');
        s.push_str(&ted_key_seq_to_string(rest));
    }

    s
}

/// A command to execute on e.g. a keypress
#[derive(Clone)]
enum Cmd {
    /// Set the mark at point
    SetMark,
    /// Insert a character into the buffer at point
    Insert(char),
    /// Insert a tab or spaces
    Tab,
    /// Move point n characters horizontally
    MoveH(isize),
    /// Move point n characters vertically
    MoveV(isize),
    /// Move point upward by near window height
    PageUp,
    /// Move point downward by near window height
    PageDown,
    /// Delete characters forward (positive) or backward (negative)
    DeleteCharsH(isize),
    /// Copy selection and add to clipboard
    Copy,
    /// Cut selection and add to clipboard
    Cut,
    /// Paste latest entry in clipboard
    Paste,
    /// Cycles the active entry to paste of the clipboard forward (newer, positive)
    /// or backward (older, negative).
    CycleClipring(isize),
    /// Insert a newline
    Newline,
    /// Read a number from prompt and go to that line
    GoToLine,
    /// Save file
    Save,
    OpenFile,
    SwitchBuf,
    SplitH,
    SplitV,
    /// Delete the active window
    DeleteWindow,
    /// Delete all windows except active window
    DeleteOtherWindows,
    /// Select the nth window following or preceding the active window cyclically
    OtherWindow(isize),
    /// Submit the topmost prompt by send the input to the associated callback
    PromptSubmit,
    /// Cancel a multi-key stroke or prompting command
    Cancel,
    /// Exit the program
    Exit,
}

type Keymap = SequenceTrie<Key, Cmd>;

#[derive(Clone, Copy, PartialEq, Eq)]
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
        self.col_i = display_width(&buffer.lines[self.line_i][0..self.col_byte_i]);
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.line_i > other.line_i ||
           ((self.line_i == other.line_i) && self.col_i > other.col_i) {
            Ordering::Greater
        } else if (self.line_i == other.line_i) && (self.col_i > other.col_i) {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A buffer is a equivalent to a temporary file in memory.
/// Could be purely temporary data, which does not exist in storage until saved,
/// or simply a normal file loaded to memory for editing
struct Buffer {
    name: String,
    lines: Vec<String>,
    filepath: Option<PathBuf>,
    /// A buffer-local keymap that may override bindings in the global keymap
    local_keymap: Keymap,
    /// Whether the buffer has been modified since open or last save
    modified: bool,
}

impl Buffer {
    fn new<S: Into<String>>(name: S) -> Self {
        Buffer {
            name: name.into(),
            lines: vec![String::new()],
            filepath: None,
            local_keymap: Keymap::new(),
            modified: false,
        }
    }

    fn new_file_buffer(filepath: PathBuf) -> Self {
        let filename = filepath.file_name()
                               .expect(&format!("No filename for filepath \"{}\"",
                                                filepath.display()))
                               .to_string_lossy()
                               .to_string();
        Buffer { filepath: Some(filepath), ..Buffer::new(filename) }
    }

    fn to_string(&self) -> String {
        self.lines.iter().map(String::as_str).intersperse("\n").collect()
    }

    fn save_to_path(&mut self, filepath: &Path) -> io::Result<()> {
        fn write_lines_to_file(f: fs::File, lines: &[String]) -> io::Result<()> {
            let mut bw = io::BufWriter::new(f);

            let (fst, rest) = lines.split_first().unwrap();

            bw.write_all(fst.as_bytes())?;
            for l in rest {
                bw.write_all("\n".as_bytes()).and_then(|_| bw.write_all(l.as_bytes()))?;
            }
            Ok(())
        }

        let r = fs::File::create(filepath).and_then(|f| write_lines_to_file(f, &self.lines));

        if r.is_ok() {
            self.modified = false
        }

        r
    }

    fn copy_selection(&mut self,
                      (start_x, start_y): (usize, usize),
                      (end_x, end_y): (usize, usize))
                      -> Vec<String> {
        if start_y == end_y {
            vec![self.lines[start_y][start_x..end_x].to_string()]
        } else {
            let mut lines = Vec::new();
            lines.push(self.lines[start_y][start_x..].to_string());
            lines.extend(self.lines[(start_y + 1)..end_y].to_vec());
            lines.push(self.lines[end_y][0..end_x].to_string());

            let (prec, succ) = self.lines.split_at_mut(start_y + 1);
            prec[start_y].push_str(&succ[0]);

            lines
        }
    }

    fn cut_selection(&mut self,
                     (start_x, start_y): (usize, usize),
                     (end_x, end_y): (usize, usize))
                     -> Vec<String> {
        self.modified = true;
        if start_y == end_y {
            vec![self.lines[start_y].drain(start_x..end_x).collect()]
        } else {
            let mut lines = Vec::new();
            lines.push(self.lines[start_y].drain(start_x..).collect());
            lines.extend(self.lines.drain((start_y + 1)..end_y));
            lines.push(self.lines[start_y + 1].drain(0..end_x).collect());

            let last_rest = self.lines.remove(start_y + 1);
            self.lines[start_y].push_str(&last_rest);

            lines
        }
    }

    fn paste(&mut self, x: usize, y: usize, lines: &[String]) {
        if let Some((last, init)) = lines.split_last() {
            self.modified = true;

            if let Some((first, mids)) = init.split_first() {
                let rest_of_line = self.lines[y].split_off(x);
                let last_insert_line = last.to_string() + &rest_of_line;

                let succ = self.lines.drain((y + 1)..).collect::<Vec<_>>();

                self.lines[y].push_str(first);
                self.lines.extend(mids.iter().cloned().chain(once(last_insert_line)).chain(succ));
            } else {
                self.lines[y].insert_str(x, last)
            }
        }
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
#[derive(Clone)]
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
    mark: Option<Point>,
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
            mark: None,
            is_active: false,
        }
    }

    fn set_mark_at_point(&mut self) {
        self.mark = Some(self.point)
    }

    /// Move point horizontally by `n` graphemes
    ///
    /// Returns whether move was prevented by beginning/end of buffer
    fn move_point_h(&mut self, n_with_dir: isize) -> MoveRes {
        let buffer = self.buffer.borrow();

        let forward = n_with_dir >= 0;
        let mut n = n_with_dir.abs() as usize;

        let mut line = &buffer.lines[self.point.line_i];

        loop {
            let grapheme_byte_is = if forward {
                let offset = self.point.col_byte_i;

                Box::new(line[self.point.col_byte_i..]
                    .grapheme_indices(true)
                    .map(move |(i, _)| i + offset)
                    .chain(once(line.len()))) as Box<Iterator<Item = _>>
            } else {
                Box::new(line[0..self.point.col_byte_i]
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
                self.point.col_byte_i = line.len();
            }
        }
    }

    /// Move point to an absolute line number
    fn move_point_to_line(&mut self, line_i: isize) -> MoveRes {
        let buffer = self.buffer.borrow();
        let n_lines = buffer.lines.len();

        let move_res = if line_i >= n_lines as isize {
            self.point.line_i = n_lines - 1;
            MoveRes::End
        } else if line_i < 0 {
            self.point.line_i = 0;
            MoveRes::Beg
        } else {
            self.point.line_i = line_i as usize;
            MoveRes::Ok
        };

        let line = &buffer.lines[self.point.line_i];
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

    /// Move point upward (negative) or downward (positive) by `n` lines
    ///
    /// Returns whether move was prevented by beginning/end of buffer
    fn move_point_v(&mut self, n: isize) -> MoveRes {
        let line_i = self.point.line_i as isize + n;

        self.move_point_to_line(line_i)
    }

    /// Move point upward or downward by near full window height
    fn page_v(&mut self, up: bool) -> MoveRes {
        let n = (self.h as f32 * 0.8).ceil() as isize;
        self.move_point_v(n * if up { -1 } else { 1 })
    }

    /// Move point upward by near full window height
    ///
    /// Returns whether move was prevented by beginning of buffer
    fn page_up(&mut self) -> MoveRes {
        self.page_v(true)
    }

    /// Move point downward by near full window height
    ///
    /// Returns whether move was prevented by end of buffer
    fn page_down(&mut self) -> MoveRes {
        self.page_v(false)
    }

    /// Move point to the end of the line
    fn move_point_to_end_of_line(&mut self) {
        let buffer = self.buffer.borrow();
        let line = &buffer.lines[self.point.line_i];
        self.point.col_byte_i = line.len();
        self.point.update_col_i(&buffer)
    }

    /// Move point to the end of the buffer
    fn move_point_to_end_of_buffer(&mut self) {
        {
            let buffer = self.buffer.borrow();
            let n_lines = buffer.lines.len();
            self.point.line_i = n_lines - 1;
        }
        self.move_point_to_end_of_line()
    }

    /// Insert a string at point
    fn insert_str_at_point(&mut self, s: &str) {
        let mut buffer = self.buffer.borrow_mut();
        {
            let line = &mut buffer.lines[self.point.line_i];

            line.insert_str(self.point.col_byte_i, s);
        }

        buffer.modified = true;
        self.mark = None;
        self.point.col_byte_i += s.len();
        self.point.update_col_i(&buffer);
        self.point.prev_col_i = self.point.col_i;
    }

    /// Insert a character at point
    fn insert_char_at_point(&mut self, c: char) {
        let mut s = [0; 4];
        self.insert_str_at_point(c.encode_utf8(&mut s))
    }

    fn insert_new_line(&mut self) {
        let mut buffer = self.buffer.borrow_mut();
        let rest = buffer.lines[self.point.line_i].split_off(self.point.col_byte_i);

        buffer.lines.insert(self.point.line_i + 1, rest);

        buffer.modified = true;
        self.mark = None;
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

    /// Copy text between mark and point. Return `None` if mark is not set
    fn copy_selection(&mut self) -> Option<Vec<String>> {
        self.mark.map(|mark| {
            let (start, end) = (min(self.point, mark), max(self.point, mark));
            let lines = self.buffer.borrow_mut().copy_selection((start.col_byte_i, start.line_i),
                                                                (end.col_byte_i, end.line_i));
            self.mark = None;
            lines
        })
    }

    /// Cut text between mark and point. Return `None` if mark is not set
    fn cut_selection(&mut self) -> Option<Vec<String>> {
        self.mark.map(|mark| {
            let (start, end) = (min(self.point, mark), max(self.point, mark));
            let lines = self.buffer.borrow_mut().cut_selection((start.col_byte_i, start.line_i),
                                                               (end.col_byte_i, end.line_i));
            self.mark = None;
            self.point = start;
            lines
        })
    }

    /// Paste `lines` into associated buffer
    fn paste(&mut self, lines: &[String]) {
        let mut buffer = self.buffer.borrow_mut();

        buffer.paste(self.point.col_byte_i, self.point.line_i, lines);

        let last = lines.last().expect("No lines to paste");
        self.point.line_i += lines.len() - 1;
        if lines.len() > 1 {
            self.point.col_byte_i = last.len();
        } else {
            self.point.col_byte_i += last.len()
        }

        self.point.update_col_i(&buffer)
    }

    /// Delete `n` characters forward (positive) or backward (negative) at point
    ///
    /// Returns whether deletion was prevented by end of buffer
    fn delete_chars_h(&mut self, n: isize) -> MoveRes {
        self.set_mark_at_point();
        let move_res = self.move_point_h(n);
        self.cut_selection();
        move_res
    }

    fn switch_to_buffer(&mut self, buffer: Rc<RefCell<Buffer>>) {
        self.buffer = buffer;
        self.mark = None;
        self.point = Point::new()
    }

    fn cancel(&mut self) {
        self.mark = None;
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
            .map(|l| display_width(&l))
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

    fn is_first_in_frame(&self) -> bool {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.is_first_in_frame()
    }

    fn is_last_in_frame(&self) -> bool {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.is_last_in_frame()
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
    /// in cyclic ordering.
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
                .map(|l| h_cols(&l, self.left, self.w))
                .map(|l| pad_line(l.clone(), self.w))
                .chain(repeat(n_spaces(self.w as usize)))
                .take(self.h as usize - 1)
                .collect()
        }
    }

    fn color_selection(&self, rendering_lines: &mut [String]) {
        fn byte_index_of_col(col_i: usize, line: &str) -> usize {
            let mut w = 0;
            for (i, g) in line.grapheme_indices(true) {
                if col_i == w {
                    return i;
                }
                w += display_width(g);
            }
            line.len()
        }

        if let Some(mark) = self.mark {
            let (start, end) = (min(self.point, mark), max(self.point, mark));

            let (s_x, s_y) = {
                let s_y = start.line_i as isize - self.top as isize;
                if s_y < 0 { (0, 0) } else { (start.col_i - self.left, s_y as usize) }
            };
            let (e_x, e_y) = if end.line_i > self.top + (self.h - 2) as usize {
                (self.w as usize, self.h as usize - 2)
            } else {
                (end.col_i - self.left, end.line_i - self.top)
            };

            let (s_i, e_i) = (byte_index_of_col(s_x, &rendering_lines[s_y]),
                              byte_index_of_col(e_x, &rendering_lines[e_y]));

            let bg_select_s = COLOR_BG_SELECTION.to_string();
            let bg_select_len = bg_select_s.len();
            let bg = COLOR_BG.to_string();

            rendering_lines[s_y].insert_str(s_i, &bg_select_s);

            for y in s_y..e_y {
                let line_len = rendering_lines[y].len();
                rendering_lines[y].insert_str(line_len, &bg);

                rendering_lines[y + 1].insert_str(0, &bg_select_s);
            }

            rendering_lines[e_y].insert_str(e_i + bg_select_len, &bg);
        }
    }

    fn render_modeline(&self) -> String {
        let buffer = self.buffer.borrow();
        let r = self.point.line_i as f32 / max(1, self.buffer.borrow().lines.len() - 1) as f32;
        let s = format!(" {} {}  L{}:{}% C{}",
                        if buffer.modified { "*" } else { "-" },
                        buffer.name,
                        self.point
                            .line_i + 1,
                        (r * 100.0).round() as u8,
                        self.point.col_i);
        let bg = if self.is_active { &*COLOR_BG_MODELINE } else { &*COLOR_BG_MODELINE_INACTIVE };
        format!("{}{}{}", bg, pad_line(s, self.w), *COLOR_BG)
    }

    fn render(&self) -> Vec<String> {
        let mut rendering_lines = self.render_lines();
        self.color_selection(&mut rendering_lines);
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

#[derive(Clone)]
enum _Partition {
    Window(Rc<RefCell<Window>>),
    SplitH(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
    SplitV(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
}

#[derive(Clone)]
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

    fn is_first_in_frame(&self) -> bool {
        if let Some(ref parent_weak) = self.parent {
            let parent = parent_weak.upgrade().unwrap();
            self.is_first_child && parent.borrow().is_first_in_frame()
        } else {
            true
        }
    }

    fn is_last_in_frame(&self) -> bool {
        if let Some(ref parent_weak) = self.parent {
            let parent = parent_weak.upgrade().unwrap();
            !self.is_first_child && parent.borrow().is_last_in_frame()
        } else {
            true
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

    /// Returns the rightmost, bottommost leaf in this partition
    fn last_window(&self) -> Rc<RefCell<Window>> {
        match self.content {
            _Partition::Window(ref window) => window.clone(),
            _Partition::SplitH(_, ref right) => right.borrow().last_window(),
            _Partition::SplitV(_, ref bot) => bot.borrow().last_window(),
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
                .unwrap_or_else(|_| panic!("Sibling was not unique reference"))
                .into_inner();

            sibling_owned.set_size(parent_w, parent_h);

            set_children_parent(&sibling_owned, parent_weak);

            unsafe { ptr::write(&mut parent_b.content, sibling_owned.content) };

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

struct Prompt {
    len: usize,
    window: Rc<RefCell<Window>>,
    subject_window: ActiveWindow,
    callback: Box<for<'t, 'i> FnBox(&'t mut TedTui, &'i str)>,
}

impl Prompt {
    fn input(&self) -> String {
        let w = self.window.borrow();
        let b = w.buffer.borrow();
        b.to_string()[self.len..].to_string()
    }
}

struct Minibuffer {
    w: u16,
    prompt_stack: Vec<Prompt>,
    _echo: Option<String>,
}

impl Minibuffer {
    fn new(w: u16) -> Minibuffer {
        Minibuffer {
            w: w,
            prompt_stack: Vec::new(),
            _echo: None,
        }
    }

    fn set_width(&mut self, w: u16) {
        self.w = w;
        for prompt in &mut self.prompt_stack {
            prompt.window.borrow_mut().set_size(w, 2);
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

    /// Returns the created prompt window
    fn push_new_prompt(&mut self,
                       subject: ActiveWindow,
                       prompt_s: &str,
                       callback: Box<FnBox(&mut TedTui, &str)>)
                       -> Rc<RefCell<Window>> {
        let mut buf = Buffer::new("*minibuffer*");
        buf.lines[0].push_str(prompt_s);
        buf.local_keymap.insert(&[Key::Char('\n')], Cmd::PromptSubmit);

        let mut window = Window::new(self.w, 2, Rc::new(RefCell::new(buf)), Weak::new());
        window.move_point_to_end_of_buffer();

        let window_shared = Rc::new(RefCell::new(window));

        let prompt = Prompt {
            len: prompt_s.len(),
            window: window_shared.clone(),
            subject_window: subject,
            callback: callback,
        };

        self.prompt_stack.push(prompt);

        window_shared
    }

    fn render(&self) -> String {
        match (self._echo.clone(), self.prompt_stack.last()) {
            (Some(s), _) => pad_line(s, self.w),
            (None, Some(p)) => pad_line(p.window
                                         .borrow()
                                         .render_lines()
                                         .first()
                                         .cloned()
                                         .unwrap_or(String::new()),
                                        self.w),
            (None, None) => n_spaces(self.w as usize),
        }
    }
}

#[derive(Clone)]
enum ActiveWindow {
    Prompt(Rc<RefCell<Window>>),
    Window(Rc<RefCell<Window>>),
}

impl ActiveWindow {
    fn window(&self) -> &Rc<RefCell<Window>> {
        match *self {
            ActiveWindow::Prompt(ref w) => &w,
            ActiveWindow::Window(ref w) => &w,
        }
    }
}

/// A frame of windows into buffers
struct Frame {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// Windows into the buffers to show in this frame, including the minibuffer window
    windows: Rc<RefCell<Partition>>,
    /// The index of the active window in `self.windows`
    active_window: ActiveWindow,
    minibuffer: Minibuffer,
}

impl Frame {
    fn new(buffer: Rc<RefCell<Buffer>>) -> Frame {
        let (windows, window) = Partition::new_root_from_buffer(buffer);

        let mut f = Frame {
            w: 0,
            h: 0,
            windows: windows,
            active_window: ActiveWindow::Window(window),
            minibuffer: Minibuffer::new(0),
        };

        f.active_window.window().borrow_mut().is_active = true;
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
            self.active_window.window().borrow_mut().reposition_view();
        }
        changed
    }

    fn render(&mut self) -> Vec<RenderingSection> {
        let mut rendering_sections = self.windows.borrow().render();
        let minibuffer_rendering = RenderingSection {
            x: 0,
            y: self.h - 1,
            lines: vec![self.minibuffer.render()],
        };

        rendering_sections.push(minibuffer_rendering);

        rendering_sections
    }
}

/// Binds the key sequence to a command in `keymap`.
///
/// Will remap all "M-FOO" sequences to "ESC FOO"
fn bind_key(keymap: &mut Keymap, key_seq: &[Key], cmd: Cmd) -> Option<Cmd> {
    let key_seq_remapped = key_seq.iter()
                                  .flat_map(|key| match *key {
                                      Key::Alt(c @ '\u{1}'...'\u{1F}') => {
                                          vec![Key::Esc, Key::Ctrl((c as u8 | 0b11 << 6) as char)]
                                      }
                                      Key::Alt(c) => vec![Key::Esc, Key::Char(c)],
                                      _ => vec![key.clone()],
                                  })
                                  .collect::<Vec<_>>();
    keymap.insert(&key_seq_remapped, cmd)
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

struct TedTui {
    buffers: HashMap<String, Rc<RefCell<Buffer>>>,
    global_keymap: Keymap,
    frame: Frame,
    term: RawTerminal<Stdout>,
    key_seq: Vec<Key>,
    prev_rendering_sections: Vec<RenderingSection>,
    clipring: Clipring,
}

impl TedTui {
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
                            (vec![Key::Alt('p')], Cmd::PageUp),
                            (vec![Key::Alt('n')], Cmd::PageDown),
                            (vec![Key::Ctrl('d')], Cmd::DeleteCharsH(1)),
                            (vec![Key::Delete], Cmd::DeleteCharsH(1)),
                            (vec![Key::Ctrl('h')], Cmd::DeleteCharsH(-1)),
                            (vec![Key::Backspace], Cmd::DeleteCharsH(-1)),
                            (vec![Key::Ctrl('w')], Cmd::Cut),
                            (vec![Key::Alt('w')], Cmd::Copy),
                            (vec![Key::Ctrl('y')], Cmd::Paste),
                            (vec![Key::Alt('y')], Cmd::CycleClipring(-1)),
                            (vec![Key::Alt('Y')], Cmd::CycleClipring(1)),
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

        TedTui {
            buffers: buffers,
            global_keymap: keymap,
            frame: Frame::new(scratch_buf),
            term: stdout().into_raw_mode().expect("Terminal failed to enter raw mode"),
            key_seq: Vec::new(),
            prev_rendering_sections: Vec::new(),
            clipring: Clipring { entries: Vec::new(), active: 0 },
        }
    }

    fn active_window(&self) -> &Rc<RefCell<Window>> {
        self.frame.active_window.window()
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
        let filepath = fs::canonicalize(name).expect("File does not exist");
        let reader = io::BufReader::new(fs::File::open(&filepath)
            .expect("File could not be opened"));
        let file_lines = reader.lines()
                               .map(|result| result)
                               .collect::<Result<Vec<_>, _>>()
                               .expect("File contains invalid utf8 data and cannot be displayed");

        let mut buffer = Buffer::new_file_buffer(filepath);
        buffer.lines = if !file_lines.is_empty() { file_lines } else { vec![String::new()] };

        let bufname = buffer.name.clone();
        let buffer_shared = Rc::new(RefCell::new(buffer));

        self.buffers.insert(bufname.clone(), buffer_shared);
        self.switch_to_buffer_with_name(&bufname)
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

    fn insert_new_line(&self) {
        self.active_window().borrow_mut().insert_new_line()
    }

    fn insert_tab(&self) {
        self.active_window().borrow_mut().insert_tab()
    }

    fn save_buffer(&mut self, buffer: Rc<RefCell<Buffer>>) {
        fn handle_res(ted: &mut TedTui, res: io::Result<()>, filepath: &Path) {
            match res {
                Ok(()) => ted.message(format!("Saved to \"{}\"", filepath.display())),
                Err(e) => ted.message(format!("Error writing file \"{}\", {:?}",
                                              filepath.display(),
                                              e)),
            }
        }

        let filepath = buffer.borrow().filepath.clone();

        match filepath {
            Some(p) => {
                let r = buffer.borrow_mut().save_to_path(&p);
                handle_res(self, r, &p)
            }
            None => self.prompt("Save file: ", move |ted, s| {
                let p = Path::new(&s);
                let r = buffer.borrow_mut().save_to_path(p);
                handle_res(ted, r, p)
            }),
        }
    }

    fn save_active_buffer(&mut self) {
        let buffer = {
            let window_b = self.active_window().borrow();
            window_b.buffer.clone()
        };

        self.save_buffer(buffer)
    }

    fn open_file(&mut self) {
        self.prompt("Open file: ", |ted, s| ted.open_file_from_path(s))
    }

    fn switch_buffer(&mut self) {
        self.prompt("Switch to buffer: ",
                    |ted, s| ted.switch_to_buffer_with_name(s))
    }

    fn split_h(&mut self) {
        match self.frame.active_window {
            ActiveWindow::Window(ref w) => w.borrow_mut().split_h(),
            ActiveWindow::Prompt(_) => self.message("Can't split minibuffer window"),
        }
    }

    fn split_v(&mut self) {
        match self.frame.active_window {
            ActiveWindow::Window(ref w) => w.borrow_mut().split_v(),
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
        match self.frame.active_window {
            ActiveWindow::Prompt(_) => self.message("Can't delete all ordinary windows"),
            ActiveWindow::Window(ref window) => {
                let active_window = window.clone();
                self.frame.windows = Partition::new_root_from_window(active_window);
                window.borrow_mut().set_size(self.frame.w, self.frame.h);
            }
        }
    }

    /// Prompt the user for input, then pass the parsed input to the given callback
    fn prompt<F>(&mut self, p: &str, callback: F)
        where F: FnOnce(&mut TedTui, &str) + 'static
    {
        let prompt_w = self.frame.minibuffer.push_new_prompt(self.frame.active_window.clone(),
                                                             p,
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
        self.prompt("Go to line: ", |ted, s| match s.parse::<isize>() {
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
            Cmd::PageUp => r = self.page_up(),
            Cmd::PageDown => r = self.page_down(),
            Cmd::DeleteCharsH(n) => r = self.delete_chars_h(n),
            Cmd::Copy => self.copy_selection(),
            Cmd::Cut => self.cut_selection(),
            Cmd::Paste => self.paste(),
            Cmd::CycleClipring(n) => self.cycle_clipring(n),
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
    fn message<S: Into<String>>(&mut self, msg: S) {
        let msg = msg.into();
        self.frame.minibuffer.echo(&msg);
        self.message_buffer_mut().lines.push(msg);
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
            let active_buffer = &self.frame.active_window.window().borrow().buffer;
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
                let s = format!("Key {} is undefined", ted_key_seq_to_string(&self.key_seq));
                self.message(s);
                self.key_seq.clear();
            }
        }
    }

    /// Return whether to exit
    fn handle_event(&mut self, event: TuiEvent) {
        match event {
            TuiEvent::Key(k) => {
                self.frame.minibuffer.clear_echo();
                self.handle_key_event(k)
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
    }

    fn cursor_pos(&self) -> (u16, u16) {
        let active = self.frame.active_window.window().borrow();
        let cursor_x_relative_window = (active.point.col_i - active.left) as u16;
        let cursor_y_relative_window = (active.point.line_i - active.top) as u16;

        let (window_x_absolute, window_y_absolute) = match self.frame.active_window {
            ActiveWindow::Prompt(_) => (0, self.frame.h - 1),
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
                (b.name.clone(), b.modified)
            };

            if modified && !ignore_buffers.contains(&name) {
                self.prompt(&format!("Buffer {} has been modified. Save? (yes, ignore, cancel) ",
                                     name),
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
            write!(self.term, "{}", termion::clear::All).ok();
            process::exit(0)
        }
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
        ted.open_file_from_path(filename);
    }

    write!(ted.term, "{}{}", *COLOR_BG, *COLOR_TEXT).expect("Failed to reset colors");

    ted.redraw();

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in TuiEvents::with_update_period(Duration::from_millis(300)) {
        ted.handle_event(event);
    }

    panic!("No more events!")
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
