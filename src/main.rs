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
use std::{str, thread};
use std::collections::HashMap;
use std::io::{self, Write, stdout, stdin, Stdout};
use std::iter::once;
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
    ($self:expr) => {
        $self.bufs
            .get(&$self.current_buf_name)
            .expect(&format!("\"current buf\" {} does not exist is map",
                             &$self.current_buf_name))
    };
}
/// Helper-macro for getting the current buffer without angering the borrow checker
macro_rules! current_buf_mut {
    ($self:expr) => {
        $self.bufs
            .get_mut(&$self.current_buf_name)
            .expect(&format!("\"current buf\" {} does not exist is map",
                             &$self.current_buf_name))
    };
}

macro_rules! println_err {
    ($fmt:expr, $( $args:expr ),*) => {
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
}

/// A buf is a equivalent to a temporary file in memory.
/// Could be purely temporary data, which does not exist in storage until saved,
/// or simply a normal file loaded to memory for editing
struct Buf {
    lines: Vec<Line>,
}

impl Buf {
    fn new() -> Self {
        Buf { lines: vec![Line { data: String::new() }] }
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
}

struct TedTui {
    bufs: HashMap<String, Buf>,
    keymap: HashMap<Key, Cmd>,
    stdout: RawTerminal<Stdout>,
    point: Point,
    view_pos: (usize, usize),
    current_buf_name: String,
    term_size: (u16, u16),
}

impl TedTui {
    fn new() -> Self {
        let mut bufs = HashMap::new();
        bufs.insert("*scratch*".to_string(), Buf::new());

        let mut keymap = HashMap::new();
        keymap.insert(Key::Ctrl('f'), Cmd::Forward(1));
        keymap.insert(Key::Ctrl('b'), Cmd::Backward(1));
        keymap.insert(Key::Ctrl('n'), Cmd::Downward(1));
        keymap.insert(Key::Ctrl('p'), Cmd::Upward(1));
        keymap.insert(Key::Ctrl('d'), Cmd::DeleteForward);
        keymap.insert(Key::Ctrl('h'), Cmd::DeleteBackward);
        keymap.insert(Key::Char('\n'), Cmd::Newline);
        keymap.insert(Key::Char('\t'), Cmd::Tab);
        keymap.insert(Key::Esc, Cmd::Exit);

        TedTui {
            bufs: bufs,
            keymap: keymap,
            stdout: stdout().into_raw_mode().unwrap(),
            point: Point {
                x: 0,
                prev_x: 0,
                y: 0,
                x_byte_i: 0,
            },
            view_pos: (0, 0),
            current_buf_name: "*scratch*".to_string(),
            term_size: termion::terminal_size().unwrap(),
        }
    }

    /// Update the grapheme x coordinate of the pointer based on
    /// the horizontal byte index and the current configuration
    fn update_point_x(&mut self) {
        self.point.update_x(current_buf!(self))
    }

    /// Insert a string at point
    fn insert_str_at_point(&mut self, s: &str) {
        current_buf_mut!(self).lines[self.point.y].data.insert_str(self.point.x_byte_i, s);

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
                let s_ahead = &line.data[self.point.x_byte_i..];
                for i in s_ahead.grapheme_indices(true)
                                .map(|(i, _)| i + offset)
                                .chain(once(line.data.len())) {
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
    fn move_point_v(&mut self, n: isize) -> bool {
        let buf = current_buf!(self);
        let n_lines = buf.lines.len();

        let end_of_buf = if self.point.y as isize + n >= n_lines as isize {
            self.point.y = n_lines - 1;
            true
        } else if (self.point.y as isize) + n < 0 {
            self.point.y = 0;
            true
        } else {
            self.point.y = (self.point.y as isize + n) as usize;
            false
        };

        self.point.x = 0;
        self.point.x_byte_i = 0;
        for (i, g) in buf.lines[self.point.y].data.grapheme_indices(true) {
            let w = g.width();
            if self.point.x + w > self.point.prev_x {
                self.point.x_byte_i = i;
                break;
            }
            self.point.x += w;
        }
        end_of_buf
    }

    fn delete_forward_at_point(&mut self) -> bool {
        let buf = current_buf_mut!(self);
        let n_lines = buf.lines.len();

        self.point.prev_x = self.point.x;

        if self.point.x_byte_i < buf.lines[self.point.y].data.len() {
            buf.lines[self.point.y].data.remove(self.point.x_byte_i);
            true
        } else if self.point.y < n_lines {
            let next_line = buf.lines.remove(self.point.y + 1);
            buf.lines[self.point.y].data.push_str(&next_line.data);
            true
        } else {
            false
        }
    }

    fn delete_backward_at_point(&mut self) -> bool {
        let buf = current_buf_mut!(self);

        if self.point.x > 0 {
            let i = {
                let line = &mut buf.lines[self.point.y];

                let (i, _) =
                    line.data[0..self.point.x_byte_i].grapheme_indices(true).rev().next().unwrap();

                line.data.remove(i);
                i
            };
            self.point.x_byte_i = i;
            self.point.update_x(buf);
            self.point.prev_x = self.point.x;

            true
        } else if self.point.y > 0 {
            let line = buf.lines.remove(self.point.y);

            {
                let prev = &buf.lines[self.point.y - 1].data;
                self.point.x_byte_i = prev.len();
            }
            self.point.y -= 1;
            self.point.update_x(buf);
            self.point.prev_x = self.point.x;

            let prev = &mut buf.lines[self.point.y].data;
            prev.push_str(&line.data);

            true
        } else {
            self.point.prev_x = self.point.x;

            false
        }
    }

    fn newline(&mut self) {
        let buf = current_buf_mut!(self);
        let rest = buf.lines[self.point.y].data.split_off(self.point.x_byte_i);

        buf.lines.insert(self.point.y + 1, Line { data: rest });

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

    /// Returns false if exit
    fn eval(&mut self, cmd: Cmd) -> bool {
        match cmd {
            Cmd::Insert(c) => {
                self.insert_char_at_point(c);
            }
            Cmd::Forward(n) => {
                self.move_point_forward(n);
            }
            Cmd::Backward(n) => {
                self.move_point_backward(n);
            }
            Cmd::Downward(n) => {
                self.move_point_v(n as isize);
            }
            Cmd::Upward(n) => {
                self.move_point_v(-(n as isize));
            }
            Cmd::DeleteForward => {
                self.delete_forward_at_point();
            }
            Cmd::DeleteBackward => {
                self.delete_backward_at_point();
            }
            Cmd::Newline => self.newline(),
            Cmd::Tab => self.tab(),
            Cmd::Exit => {
                return false;
            }
            Cmd::GoToLine(_) => unimplemented!(),
        }
        true
    }

    /// Get the message buf
    fn message_buf(&mut self) -> &mut Buf {
        self.bufs.entry("*messages*".to_string()).or_insert(Buf::new())
    }

    /// Write a message to the *message* buf
    fn message<S: Into<String>>(&mut self, msg: S) {
        let msg = msg.into();

        self.message_buf().lines.push(Line { data: msg });
    }

    fn render_if_resized(&mut self) -> Result<(), io::Error> {
        let size = termion::terminal_size().unwrap();
        if self.term_size != size {
            self.term_size = size;

            self._render()
        } else {
            Ok(())
        }
    }

    fn _render(&mut self) -> Result<(), io::Error> {
        let tab_spaces = String::from_utf8(vec![' ' as u8; TAB_WIDTH]).unwrap();
        let (w, h) = self.term_size;
        let c_bg = color::Bg(color::Rgb(0x18, 0x18, 0x10));
        let c_text = color::Fg(color::Rgb(0xF0, 0xE6, 0xD6));
        let c_dim_text = color::Fg(color::Rgb(0x60, 0x56, 0x50));

        write!(self.stdout,
               "{}{}{}{}{}",
               cursor::Goto(1, 1),
               c_bg,
               c_text,
               termion::clear::AfterCursor,
               cursor::Hide)?;

        for i in 0..h {
            if let Some(line) = current_buf!(self).lines.get(i as usize) {
                write!(self.stdout,
                       "{}{}{:05}. {}",
                       cursor::Goto(1, i + 1),
                       c_dim_text,
                       i + 1,
                       c_text)?;

                let mut line_len = 7;

                for g in line.data.graphemes(true) {
                    let s = if g == "\t" { &tab_spaces } else { g };
                    let s_w = s.width();

                    if line_len + s_w < w as usize {
                        write!(self.stdout, "{}", s)?;
                        line_len += s_w;
                    }
                }
            }
        }

        write!(self.stdout,
               "{}{}",
               cursor::Goto(self.point.x as u16 + 1 + 7, self.point.y as u16 + 1),
               cursor::Show)?;

        self.stdout.flush()
    }

    /// Render unconditionally
    fn render(&mut self) -> Result<(), io::Error> {
        self.term_size = termion::terminal_size().unwrap();

        self._render()
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
fn start_tui_mode() {
    let mut ted = TedTui::new();

    ted.render().expect("Rendering failed");

    #[cfg(feature = "profiling")]
    PROFILER.lock().unwrap().start("./prof.profile").expect("Failed to start profiler");

    for event in TuiEvents::new(5) {
        match event {
            TuiEvent::Key(k) => {
                if let Some(cmd) = ted.keymap.get(&k).cloned() {
                    if !ted.eval(cmd) {
                        break;
                    }
                } else if let Key::Char(c) = k {
                    if !ted.eval(Cmd::Insert(c)) {
                        break;
                    }
                } else {
                    ted.message(format!("Key {} is undefined", ted_key_to_string(k)))
                }
                ted.render().expect("Rendering failed");
            }
            TuiEvent::Update => {
                ted.render_if_resized().expect("Rendering failed");
            }
            TuiEvent::Mouse(_) => {
                ted.message(format!("Mousevent is undefined"));
                ted.render().expect("Rendering failed");
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
        .get_matches();

    if matches.is_present("tui-mode") {
        // Start in terminal mode
        start_tui_mode();
    } else {
        // Start GUI mode. Vulkan?
        unimplemented!();
    }
}
