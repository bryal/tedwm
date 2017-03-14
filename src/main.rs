#[macro_use]
extern crate clap;
extern crate termion;
extern crate unicode_width;
extern crate unicode_segmentation;
extern crate futures;
extern crate tokio_timer;

#[cfg(feature = "profiling")]
extern crate cpuprofiler;

use clap::{Arg, App};

#[cfg(feature = "profiling")]
use cpuprofiler::PROFILER;

use futures::{Sink, Stream};
use futures::sync::mpsc::channel;
use std::{str, thread, fs};
use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::{self, Write, stdout, stdin, Stdout, BufRead};
use std::iter::once;
use std::path::PathBuf;
use std::time::Duration;
use termion::{color, cursor};
use termion::event::{Event, Key, MouseEvent};
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};
use tokio_timer::Timer;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// Helper-macro for getting the current buffer without angering the borrow checker
macro_rules! current_buf {
    ($ted:expr) => {
        $ted.bufs
            .get(&$ted.current_buf_name)
            .expect(&format!("\"current buf\" {} does not exist is map",
                             &$ted.current_buf_name))
    };
}
/// Helper-macro for getting the current buffer without angering the borrow checker
macro_rules! current_buf_mut {
    ($ted:expr) => {
        $ted.bufs
            .get_mut(&$ted.current_buf_name)
            .expect(&format!("\"current buf\" {} does not exist is map",
                             &$ted.current_buf_name))
    };
}

macro_rules! println_err {
    ($fmt:expr $(, $args:expr )*) => {
        writeln!(io::stderr(), $fmt, $($args),*).unwrap()
    };
}

/// Margin when scrolling
const SCROLL_MARGIN: u16 = 10;
/// Whether to insert spaces on TAB press
const TAB_INSERTS_SPACES: bool = false;
/// Tab display width / n.o. spaces to convert to
const TAB_WIDTH: usize = 8;

#[derive(Clone, Copy)]
struct Point {
    /// 0-based index of, approximatively, glyphs
    x: usize,
    /// x-position in glyphs before moving vertically.
    ///
    /// Used to keep track of x-position when moving along lines
    /// that might be shorter than current x-position
    prev_x: usize,
    /// 0-based line index
    y: usize,
    /// Byte index in line of points x-position
    x_byte_i: usize,
}

impl Point {
    fn new() -> Point {
        Point {
            x: 0,
            prev_x: 0,
            y: 0,
            x_byte_i: 0,
        }
    }

    /// Update the grapheme x-coordinate of the position in buffer `buf`
    fn update_x(&mut self, buf: &Buf) {
        self.x = buf.lines[self.y].data[0..self.x_byte_i]
            .graphemes(true)
            .map(|g| if g == "\t" { TAB_WIDTH } else { g.width() })
            .sum();
    }
}

struct Line {
    data: String,
    /// Track whether this line has been modified in buffer.
    ///
    /// Used to determine whether to redraw when rendering
    changed: bool,
    /// Track width in graphemes of `data`
    width: usize,
}

impl Line {
    fn new(s: String) -> Line {
        let w = s.width();
        Line {
            data: s,
            changed: true,
            width: w,
        }
    }

    fn insert_str(&mut self, i: usize, s: &str) {
        self.data.insert_str(i, s);
        self.changed = true;
        self.width = self.data.width();
    }

    fn push_str(&mut self, s: &str) {
        self.data.push_str(s);
        self.changed = true;
        self.width = self.data.width();
    }

    fn remove(&mut self, i: usize) {
        self.data.remove(i);
        self.changed = true;
        self.width = self.data.width();
    }
}

/// A buf is a equivalent to a temporary file in memory.
/// Could be purely temporary data, which does not exist in storage until saved,
/// or simply a normal file loaded to memory for editing
struct Buf {
    lines: Vec<Line>,
    filepath: Option<PathBuf>,
}

impl Buf {
    fn new_buf() -> Self {
        Buf {
            lines: vec![Line::new(String::new())],
            filepath: None,
        }
    }

    fn new_file_buf(filepath: PathBuf) -> Self {
        Buf { filepath: Some(filepath), ..Buf::new_buf() }
    }
}

/// A command to execute on e.g. a keypress
#[derive(Clone)]
enum Cmd {
    /// Insert a character into the buf at point
    Insert(char),
    /// Insert a tab or spaces
    Tab,
    /// Move point n characters forward
    Forward(usize),
    /// Move point n characters backward
    Backward(usize),
    /// Move point n characters downward
    Downward(usize),
    /// Move point n characters upward
    Upward(usize),
    /// Delete a character forwards
    DeleteForward,
    /// Delete a character backwards
    DeleteBackward,
    /// Insert a newline
    Newline,
    /// Exit the program
    Exit,
    /// Go to line n, counting from line 1 at the beginning of the buf
    GoToLine(u64),
    /// Save file
    Save,
}

struct View {
    /// 0-based position of top of view based on drawn lines
    y: usize,
    /// Track whether the view has moved/changed.
    ///
    /// Used to determine whether to redraw everything when rendering
    changed: bool,
}

/// What to draw when redrawing
#[derive(PartialEq, Eq, Clone, Copy)]
enum Redraw {
    /// Redraw everything in view unconditionally
    All,
    /// Redraw the cursor and lines that have been modified if view is unchanged,
    /// or all lines if view has changed
    Changed,
}

struct TedTui {
    bufs: HashMap<String, Buf>,
    keymap: HashMap<Key, Cmd>,
    stdout: RawTerminal<Stdout>,
    point: Point,
    view: View,
    current_buf_name: String,
    term_size: (u16, u16),
}

impl TedTui {
    fn new() -> Self {
        let mut bufs = HashMap::new();
        bufs.insert("*scratch*".to_string(), Buf::new_buf());

        let mut keymap = HashMap::new();
        keymap.insert(Key::Ctrl('f'), Cmd::Forward(1));
        keymap.insert(Key::Ctrl('b'), Cmd::Backward(1));
        keymap.insert(Key::Ctrl('n'), Cmd::Downward(1));
        keymap.insert(Key::Ctrl('p'), Cmd::Upward(1));
        keymap.insert(Key::Ctrl('d'), Cmd::DeleteForward);
        keymap.insert(Key::Delete, Cmd::DeleteForward);
        keymap.insert(Key::Ctrl('h'), Cmd::DeleteBackward);
        keymap.insert(Key::Backspace, Cmd::DeleteBackward);
        keymap.insert(Key::Char('\n'), Cmd::Newline);
        keymap.insert(Key::Char('\t'), Cmd::Tab);
        keymap.insert(Key::Ctrl('s'), Cmd::Save);
        keymap.insert(Key::Esc, Cmd::Exit);

        TedTui {
            bufs: bufs,
            keymap: keymap,
            stdout: stdout().into_raw_mode().unwrap(),
            point: Point::new(),
            view: View {
                y: 0,
                changed: true,
            },
            current_buf_name: "*scratch*".to_string(),
            term_size: (0, 0),
        }
    }

    /// Update the grapheme x coordinate of the pointer based on
    /// the horizontal byte index and the current configuration
    fn update_point_x(&mut self) {
        self.point.update_x(current_buf!(self))
    }

    /// Insert a string at point
    fn insert_str_at_point(&mut self, s: &str) {
        {
            let line = &mut current_buf_mut!(self).lines[self.point.y];
            line.insert_str(self.point.x_byte_i, s);
        }

        self.point.x_byte_i += s.len();
        self.update_point_x();
        self.point.prev_x = self.point.x;
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
        let buf = current_buf!(self);

        'a: loop {
            if let Some(line) = buf.lines.get(self.point.y) {
                let offset = self.point.x_byte_i;
                let is = line.data[self.point.x_byte_i..]
                    .grapheme_indices(true)
                    .map(|(i, _)| i + offset)
                    .chain(once(line.data.len()));
                for i in is {
                    if n == 0 {
                        self.point.x_byte_i = i;
                        self.point.update_x(buf);
                        self.point.prev_x = self.point.x;

                        return false;
                    } else {
                        n -= 1;
                    }
                }
                if self.point.y + 1 == buf.lines.len() {
                    self.point.update_x(buf);
                    self.point.prev_x = self.point.x;

                    return true;
                } else {
                    self.point.y += 1;
                    self.point.x_byte_i = 0;
                }
            } else {
                self.point.update_x(buf);
                self.point.prev_x = self.point.x;

                return true;
            }
        }
    }

    /// Returns true if sucessfully moved backward n characters.
    /// If beginning of buf reached before n = 0, return true
    fn move_point_backward(&mut self, mut n: usize) -> bool {
        let buf = current_buf!(self);

        let mut line = &buf.lines[self.point.y];
        'a: loop {
            let grapheme_indices_rev = if line.data.len() == self.point.x_byte_i {
                Box::new(line.data[0..self.point.x_byte_i]
                    .grapheme_indices(true)
                    .map(|(i, _)| i)
                    .chain(once(self.point.x_byte_i))
                    .rev()) as Box<Iterator<Item = usize>>
            } else {
                Box::new(line.data[0..(self.point.x_byte_i + 1)]
                    .grapheme_indices(true)
                    .map(|(i, _)| i)
                    .rev()) as Box<Iterator<Item = usize>>
            };
            for i in grapheme_indices_rev {
                let beginning_reached = self.point.y == 0 && i == 0;
                if n == 0 || beginning_reached {
                    self.point.x_byte_i = i;
                    self.point.update_x(buf);
                    self.point.prev_x = self.point.x;

                    return beginning_reached;
                } else {
                    n -= 1;
                }
            }
            self.point.y -= 1;

            if let Some(prev_line) = buf.lines.get(self.point.y) {
                line = prev_line;
                self.point.x_byte_i = line.data.len();
            } else {
                self.point.update_x(buf);
                self.point.prev_x = self.point.x;

                return true;
            }
        }
    }

    /// Move point upward (negative) or downward (positive)
    ///
    /// Return whether end/beginning of buffer reached
    fn move_point_v(&mut self, n: isize) -> bool {
        let buf = current_buf!(self);
        let n_lines = buf.lines.len();

        let end_of_buf = if self.point.y as isize + n >= n_lines as isize {
            self.point.y = n_lines - 1;
            false
        } else if (self.point.y as isize) + n < 0 {
            self.point.y = 0;
            false
        } else {
            self.point.y = (self.point.y as isize + n) as usize;
            true
        };

        let line = &buf.lines[self.point.y].data;
        self.point.x = 0;
        self.point.x_byte_i = 0;
        for (i, g) in line.grapheme_indices(true) {
            let w = g.width();
            if self.point.x + w > self.point.prev_x {
                self.point.x_byte_i = i;
                return end_of_buf;
            }
            self.point.x += w;
        }
        self.point.x_byte_i = line.len();
        end_of_buf
    }

    /// Return true if at end of buffer
    fn delete_forward_at_point(&mut self) -> bool {
        let buf = current_buf_mut!(self);
        let n_lines = buf.lines.len();

        self.point.prev_x = self.point.x;

        if self.point.x_byte_i < buf.lines[self.point.y].data.len() {
            buf.lines[self.point.y].remove(self.point.x_byte_i);

            false
        } else if self.point.y < n_lines - 1 {
            let next_line = buf.lines.remove(self.point.y + 1);
            buf.lines[self.point.y].push_str(&next_line.data);

            for changed_or_moved_line in &mut buf.lines[self.point.y..] {
                changed_or_moved_line.changed = true;
            }
            false
        } else {
            true
        }
    }

    /// Return true if at beginning of buffer
    fn delete_backward_at_point(&mut self) -> bool {
        let buf = current_buf_mut!(self);

        if self.point.x > 0 {
            let i = {
                let line = &mut buf.lines[self.point.y];
                let (i, _) =
                    line.data[0..self.point.x_byte_i].grapheme_indices(true).rev().next().unwrap();

                line.remove(i);
                i
            };
            self.point.x_byte_i = i;
            self.point.update_x(buf);
            self.point.prev_x = self.point.x;

            false
        } else if self.point.y > 0 {
            let line = buf.lines.remove(self.point.y);

            self.point.x_byte_i = buf.lines[self.point.y - 1].data.len();
            self.point.y -= 1;
            self.point.update_x(buf);
            self.point.prev_x = self.point.x;

            buf.lines[self.point.y].push_str(&line.data);

            for changed_or_moved_line in &mut buf.lines[self.point.y..] {
                changed_or_moved_line.changed = true;
            }
            false
        } else {
            self.point.prev_x = self.point.x;

            true
        }
    }

    fn newline(&mut self) {
        let buf = current_buf_mut!(self);
        let rest = buf.lines[self.point.y].data.split_off(self.point.x_byte_i);
        buf.lines[self.point.y].width = buf.lines[self.point.y].data.width();

        buf.lines.insert(self.point.y + 1, Line::new(rest));

        for changed_or_moved_line in &mut buf.lines[self.point.y..] {
            changed_or_moved_line.changed = true;
        }

        self.point.y += 1;
        self.point.x_byte_i = 0;
        self.point.x = 0;
        self.point.prev_x = 0;
    }

    /// Insert tab or spaces
    fn tab(&mut self) {
        if TAB_INSERTS_SPACES {
            let spaces = vec![' ' as u8; TAB_WIDTH];
            self.insert_str_at_point(str::from_utf8(&spaces).unwrap());
        } else {
            self.insert_char_at_point('\t');
        }
    }

    fn open_file(&mut self, name: &str) {
        let filepath = fs::canonicalize(name).expect("File does not exist");
        let reader = io::BufReader::new(fs::File::open(&filepath)
            .expect("File could not be opened"));
        let file_lines = reader.lines()
                               .map(|result| result.map(|s| Line::new(s)))
                               .collect::<Result<Vec<_>, _>>()
                               .expect("File contains invalid utf8 data and cannot be displayed");

        let mut buf = Buf::new_file_buf(filepath);
        buf.lines =
            if !file_lines.is_empty() { file_lines } else { vec![Line::new(String::new())] };

        self.bufs.insert(name.to_string(), buf);
        self.current_buf_name = name.to_string();
        self.point = Point::new();
    }

    fn save_file(&mut self) {
        fn write_lines_to_file(f: fs::File, lines: &[Line]) -> io::Result<()> {
            let mut bw = io::BufWriter::new(f);

            let (fst, rest) = lines.split_first().unwrap();

            bw.write_all(fst.data.as_bytes())?;
            for l in rest {
                bw.write_all("\n".as_bytes()).and_then(|_| bw.write_all(l.data.as_bytes()))?;
            }
            Ok(())
        }

        let buf = current_buf!(self);

        match buf.filepath {
            Some(ref p) => fs::File::create(p)
                .and_then(|f| write_lines_to_file(f, &buf.lines))
                .expect("Failed to write buffer to file"),
            None => unimplemented!(),
        }
    }

    /// Returns true if exit
    fn eval(&mut self, cmd: Cmd) -> bool {
        enum MoveRes {
            Beg,
            End,
            Ok,
        }

        let mut r = MoveRes::Ok;
        match cmd {
            Cmd::Insert(c) => {
                self.insert_char_at_point(c);
            }
            Cmd::Forward(n) => if self.move_point_forward(n) {
                r = MoveRes::End
            },
            Cmd::Backward(n) => if self.move_point_backward(n) {
                r = MoveRes::Beg
            },
            Cmd::Downward(n) => if self.move_point_v(n as isize) {
                r = MoveRes::End
            },
            Cmd::Upward(n) => if self.move_point_v(-(n as isize)) {
                r = MoveRes::Beg
            },
            Cmd::DeleteForward => if self.delete_forward_at_point() {
                r = MoveRes::End
            },
            Cmd::DeleteBackward => if self.delete_backward_at_point() {
                r = MoveRes::Beg
            },
            Cmd::Newline => self.newline(),
            Cmd::Tab => self.tab(),
            Cmd::Exit => {
                return true;
            }
            Cmd::GoToLine(_) => unimplemented!(),
            Cmd::Save => self.save_file(),
        }
        match r {
            MoveRes::Beg => self.message("Beginning of buffer"),
            MoveRes::End => self.message("End of buffer"),
            MoveRes::Ok => (),
        }
        self.reposition_view();
        false
    }

    /// Position the view such that the cursor is visible.
    ///
    /// How exactly the view is positioned depends on the config value `SCROLL_MARGIN`
    fn reposition_view(&mut self) {
        let h = self.term_size.1;
        let point_draw_y = self.point_draw_pos().1;
        let top_margin = self.view.y as i32 + SCROLL_MARGIN as i32;
        let bot_margin = self.view.y as i32 + h as i32 - SCROLL_MARGIN as i32;
        let move_relative_up = min(point_draw_y as i32 - top_margin, 0);
        let move_relative_down = max(point_draw_y as i32 - bot_margin, 0);

        let bot = current_buf!(self).lines.len();

        let view_y = min(bot,
                         max(0,
                             self.view.y as i32 + move_relative_up +
                             move_relative_down) as usize);
        if view_y != self.view.y {
            self.view.changed = true
        }
        println_err!("h: {}, point_draw_y: {}, top_m: {}, bot_m: {}, up: {}, down: {}, bot: {}, \
                      view_y: {}, view.y: {}",
                     h,
                     point_draw_y,
                     top_margin,
                     bot_margin,
                     move_relative_up,
                     move_relative_down,
                     bot,
                     view_y,
                     self.view.y);
        self.view.y = view_y;
    }

    /// Get the message buf
    fn message_buf(&mut self) -> &mut Buf {
        self.bufs.entry("*messages*".to_string()).or_insert(Buf::new_buf())
    }

    /// Write a message to the *message* buf
    fn message<S: Into<String>>(&mut self, msg: S) {
        let msg = msg.into();

        self.message_buf().lines.push(Line::new(msg));
    }

    /// Returns whether terminal size has changed
    fn update_term_size(&mut self) -> bool {
        let size = termion::terminal_size().unwrap();
        let changed = self.term_size != size;
        self.term_size = size;

        self.reposition_view();

        changed
    }

    fn _redraw(&mut self, redraw: Redraw) -> Result<(), io::Error> {
        let tab_spaces = String::from_utf8(vec![' ' as u8; TAB_WIDTH]).unwrap();
        let c_bg = color::Bg(color::Rgb(0x18, 0x18, 0x10));
        let c_text = color::Fg(color::Rgb(0xF0, 0xE6, 0xD6));
        let c_dim_text = color::Fg(color::Rgb(0x60, 0x56, 0x50));
        let term_size_changed = self.update_term_size();
        let (w, h) = self.term_size;
        let redraw_all = redraw == Redraw::All ||
                         (redraw == Redraw::Changed && (self.view.changed || term_size_changed));

        write!(self.stdout,
               "{}{}{}{}",
               cursor::Goto(1, 1),
               c_bg,
               c_text,
               cursor::Hide)?;

        let linum_width = 7;
        /// Index of which row in terminal to draw on
        let mut term_line_n = 1u16;
        let mut i = 0;
        while current_buf!(self).lines.get(i).is_some() {
            let line_draw_y = self.n_draw_lines_before(i);
            let line = &mut current_buf_mut!(self).lines[i];
            let line_in_view = line_draw_y >= self.view.y &&
                               line_draw_y <= self.view.y + h as usize;
            let line_after_view = line_draw_y > self.view.y + h as usize;

            if line_after_view {
                break;
            } else if line_in_view {
                let should_redraw_line = redraw_all || (redraw == Redraw::Changed && line.changed);

                println_err!("line {} in view", i);
                if should_redraw_line {
                    println_err!("Redrawing line {}", i);
                    write!(self.stdout, "{}", cursor::Goto(1, term_line_n))?;
                    write!(self.stdout, "{}", termion::clear::CurrentLine)?;
                    write!(self.stdout, "{}{:>5}. {}", c_dim_text, i + 1, c_text)?;

                    // Width of the line displayed in the terminal
                    let mut draw_line_width = linum_width;
                    for g in line.data.graphemes(true) {
                        let s = if g == "\t" { &tab_spaces } else { g };
                        let s_w = s.width();

                        if draw_line_width + s_w <= w as usize {
                            write!(self.stdout, "{}", s)?;
                            draw_line_width += s_w;
                        } else {
                            term_line_n += 1;
                            write!(self.stdout, "{}{}", cursor::Goto(1, term_line_n), s)?;
                            draw_line_width = s_w;
                        }
                    }
                    line.changed = false;
                } else {
                    term_line_n += ((linum_width + line.width) / w as usize) as u16;
                }
                term_line_n += 1;
            }
            i += 1;
        }
        write!(self.stdout,
               "{}{}",
               cursor::Goto(1, term_line_n),
               termion::clear::AfterCursor)?;

        let (point_draw_x, point_draw_y) = self.point_draw_pos();
        let (cursor_x, cursor_y) = (point_draw_x + 1, (point_draw_y - self.view.y) as u16 + 1);

        write!(self.stdout,
               "{}{}",
               cursor::Goto(cursor_x, cursor_y),
               cursor::Show)?;

        self.term_size = (w, h);
        self.view.changed = false;
        self.stdout.flush()
    }

    fn redraw(&mut self, redraw: Redraw) {
        self._redraw(redraw).expect("Redraw failed")
    }

    fn n_draw_lines_before(&self, line_y: usize) -> usize {
        let w = self.term_size.0;
        current_buf!(self).lines[0..line_y]
            .iter()
            .map(|l| l.width / w as usize + 1)
            .sum::<usize>()
    }

    /// Position of point when taking line wrap into account
    ///
    /// E.g., consider a single line that spans 100 lines in terminal when drawn, due to line wrap.
    /// In line-coordinates, y is still 0, because it's the first line.
    /// In drawn-line-coordinates, y might be anywhere from 1 to 100,
    /// depending on where the cursor is.
    fn point_draw_pos(&self) -> (u16, usize) {
        let w = self.term_size.0;
        let linum_width = 7;
        let x = ((self.point.x + linum_width) % w as usize) as u16;
        let preceding_n_draw_lines = self.n_draw_lines_before(self.point.y);
        let y = preceding_n_draw_lines + (self.point.x + linum_width) / w as usize;
        (x, y)
    }
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
        let update_event_stream = timer.interval(update_period)
                                       .map(|_| Some(Ok(TuiEvent::Update)))
                                       .map_err(|_| ());

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
fn start_tui_mode(opt_filename: Option<&str>) {
    let mut ted = TedTui::new();

    if let Some(filename) = opt_filename {
        ted.open_file(filename);
    }

    ted.redraw(Redraw::All);

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in TuiEvents::new(5) {
        match event {
            TuiEvent::Key(k) => {
                let exit = if let Some(cmd) = ted.keymap.get(&k).cloned() {
                    ted.eval(cmd)
                } else if let Key::Char(c) = k {
                    ted.eval(Cmd::Insert(c))
                } else {
                    ted.message(format!("Key {} is undefined", ted_key_to_string(k)));
                    false
                };

                if exit {
                    println_err!("Exiting...");
                    break;
                }

                ted.redraw(Redraw::Changed);
            }
            TuiEvent::Update => {
                ted.redraw(Redraw::Changed);
            }
            TuiEvent::Mouse(_) => {
                ted.message(format!("Mousevent is undefined"));
                ted.redraw(Redraw::Changed);
            }
            TuiEvent::Unsupported(u) => {
                ted.message(format!("Unsupported event {:?}", u));
            }
        }
    }

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
        start_tui_mode(matches.value_of("file"));
    } else {
        // Start GUI mode. Vulkan?
        unimplemented!();
    }
}
