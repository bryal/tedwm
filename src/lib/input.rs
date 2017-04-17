use futures::{Sink, Stream};
use futures::sync::mpsc::channel;
use std::io::stdin;
use std::thread;
use std::time::Duration;
pub use termion::event::{Key, MouseEvent};
use termion::input::TermRead;
use tokio_timer::Timer;

/// Formatting of key events
pub fn key_seq_to_string(ks: &[Key]) -> String {
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
        [Key::Esc, k @ Key::Ctrl(_), ref rest..] => (format!("M-{}", key_seq_to_string(&[k])),
                                                     rest),
        [Key::Esc, ref rest..] => ("<escape>".to_string(), rest),
        [Key::Null, ref rest..] => ("<null>".to_string(), rest),
        [Key::__IsNotComplete, ref rest..] => ("UNSUPPORTED".to_string(), rest),
    };

    if rest.len() > 0 {
        s.push(' ');
        s.push_str(&key_seq_to_string(rest));
    }

    s
}

/// A command to execute on e.g. a keypress
#[derive(Clone)]
pub enum Cmd {
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

pub type Keymap = ::sequence_trie::SequenceTrie<Key, Cmd>;

/// Binds the key sequence to a command in `keymap`.
///
/// Will remap all "M-FOO" sequences to "ESC FOO"
pub fn bind_key(keymap: &mut Keymap, key_seq: &[Key], cmd: Cmd) -> Option<Cmd> {
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

pub enum Event {
    Update,
    Key(Key),
    Mouse(MouseEvent),
    Unsupported(Vec<u8>),
}

impl From<::termion::event::Event> for Event {
    fn from(e: ::termion::event::Event) -> Event {
        match e {
            ::termion::event::Event::Key(k) => Event::Key(k),
            ::termion::event::Event::Mouse(m) => Event::Mouse(m),
            ::termion::event::Event::Unsupported(u) => Event::Unsupported(u),
        }
    }
}

/// Iterator for event loop of terminal events
pub struct Events {
    event_it: Box<Iterator<Item = Event>>,
}

impl Events {
    pub fn with_update_period(update_period: Duration) -> Self {
        let (tx, term_read_event_rx) = channel(0);

        thread::spawn(|| {
            let mut tx = tx.wait();

            for read_event_result in stdin().events() {
                tx.send(Event::from(read_event_result.expect("Failed to read terminal event")))
                  .expect("Failed to send on channel");
            }
            panic!("No more events on stdin");
        });

        let timer = Timer::default();
        let update_event_stream =
            timer.interval(update_period).map(|_| Event::Update).map_err(|_| ());

        let event_it = term_read_event_rx.select(update_event_stream).wait().map(|r| r.unwrap());

        Events { event_it: Box::new(event_it) as Box<Iterator<Item = _>> }
    }
}

impl Iterator for Events {
    type Item = Event;

    fn next(&mut self) -> Option<Self::Item> {
        self.event_it.next()
    }
}
