use itertools::Itertools;
use lib::*;
use lib::input::Keymap;
use std::cmp::Ordering;
use std::fs;
use std::io::{self, Write, BufRead};
use std::iter::once;
use std::path::{PathBuf, Path};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Point {
    /// 0-based index of, hopefully, column in terminal
    ///
    /// E.g. a grapheme that consists of 8 bytes, but should be rendered as
    /// a half-width glyph / single monospace, will count as 1 column.
    /// Because of the nature of fonts and the unicode standard, this might
    /// not always be the case, which means that this value might be incorrect.
    /// This should, however, only affect the rendered appearance of the cursor
    /// in relation to the text, and it should not happen often.
    pub col_i: usize,
    /// The byte-index corresponding to `self.col` in the current line string
    pub col_byte_i: usize,
    /// Column index before moving vertically.
    ///
    /// Used to keep track of column when moving along lines
    /// that might be shorter than current line
    pub prev_col_i: usize,
    /// 0-based index of line in memory
    pub line_i: usize,
}

impl Point {
    pub fn new() -> Point {
        Point {
            col_i: 0,
            col_byte_i: 0,
            prev_col_i: 0,
            line_i: 0,
        }
    }

    /// Update the column-position of the point in buffer `buffer`
    pub fn update_col_i(&mut self, buffer: &Buffer) {
        self.col_i = display_width(&buffer.lines()[self.line_i][0..self.col_byte_i]);
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

#[derive(Clone)]
pub enum Change {
    Insert((usize, usize), Rc<Vec<String>>),
    Remove((usize, usize), Rc<Vec<String>>),
}

pub struct Changes {
    changes: Vec<Change>,
    i: usize,
}

impl Changes {
    fn new() -> Self {
        Changes { changes: Vec::new(), i: 0 }
    }

    fn try_append_insert(&mut self, (x, y): (usize, usize), text: &[String]) -> bool {
        if let Some(&mut Change::Insert((p_x, p_y), ref mut p_text)) = self.changes.last_mut() {
            let one_char = text.len() == 1 && text[0].graphemes(true).count() == 1;
            let correct_pos = p_y == y && p_text.len() == 1 && p_x + p_text[0].len() == x;

            if one_char && correct_pos {
                match text[0].graphemes(true).next().unwrap() {
                    " " | "\t" => false,
                    c => {
                        let mut upd = (**p_text).clone();
                        upd[0].push_str(c);
                        *p_text = Rc::new(upd);
                        true
                    }
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    fn try_append_remove(&mut self, (x, y): (usize, usize), text: &[String]) -> bool {
        if let Some(&mut Change::Remove(ref mut p, ref mut p_text)) = self.changes.last_mut() {
            let (p_x, p_y) = *p;
            let one_char = text.len() == 1 && text[0].graphemes(true).count() == 1;
            let correct_pos = p_y == y && p_text.len() == 1 && x + text[0].len() == p_x;

            if one_char && correct_pos {
                match text[0].graphemes(true).next().unwrap() {
                    " " | "\t" => false,
                    c => {
                        let mut upd = (**p_text).clone();
                        upd[0].insert_str(0, c);
                        *p_text = Rc::new(upd);
                        *p = (x, y);
                        true
                    }
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn add_insert(&mut self, pos: (usize, usize), text: Vec<String>) {
        if !self.try_append_insert(pos, &text) {
            self.changes.push(Change::Insert(pos, Rc::new(text)));
        }
        self.break_undo_sequence()
    }

    pub fn add_remove(&mut self, pos: (usize, usize), text: Vec<String>) {
        if !self.try_append_remove(pos, &text) {
            self.changes.push(Change::Remove(pos, Rc::new(text)));
        }

        self.break_undo_sequence()
    }

    pub fn break_undo_sequence(&mut self) {
        self.i = self.changes.len();
    }

    pub fn undo(&mut self) -> Option<Change> {
        if self.i == 0 {
            None
        } else {
            self.i -= 1;

            Some(self.changes[self.i].clone())
        }
    }
}

/// A buffer is a equivalent to a temporary file in memory.
/// Could be purely temporary data, which does not exist in storage until saved,
/// or simply a normal file loaded to memory for editing
pub struct Buffer {
    name: String,
    lines: Vec<String>,
    filepath: Option<PathBuf>,
    /// A buffer-local keymap that may override bindings in the global keymap
    pub local_keymap: Keymap,
    /// Whether the buffer has been modified since open or last save
    modified: bool,
    pub changes: Changes,
}

impl Buffer {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Buffer {
            name: name.into(),
            lines: vec![String::new()],
            filepath: None,
            local_keymap: Keymap::new(),
            modified: false,
            changes: Changes::new(),
        }
    }

    pub fn from_file(filepath: PathBuf) -> Self {
        let reader = io::BufReader::new(fs::File::open(&filepath)
            .expect("File could not be opened"));
        let file_lines = reader.lines()
                               .map(|result| result)
                               .collect::<Result<Vec<_>, _>>()
                               .expect("File contains invalid utf8 data and cannot be displayed");

        let lines = if !file_lines.is_empty() { file_lines } else { vec![String::new()] };

        let filename = filepath.file_name()
                               .expect(&format!("No filename for filepath \"{}\"",
                                                filepath.display()))
                               .to_string_lossy()
                               .to_string();

        Buffer {
            lines: lines,
            filepath: Some(filepath),
            ..Buffer::new(filename)
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn lines(&self) -> &[String] {
        &self.lines
    }

    pub fn filepath(&self) -> Option<&Path> {
        self.filepath.as_ref().map(PathBuf::as_path)
    }

    pub fn is_modified(&self) -> bool {
        self.modified
    }

    pub fn to_string(&self) -> String {
        self.lines.iter().map(String::as_str).intersperse("\n").collect()
    }

    pub fn save_to_path(&mut self, filepath: &Path) -> io::Result<()> {
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

    pub fn insert_str(&mut self, x: usize, y: usize, s: &str) {
        self.paste((x, y), &[s]);
    }

    pub fn insert_new_line(&mut self, x: usize, y: usize) {
        self.paste((x, y), &["", ""])
    }

    pub fn push_line(&mut self, line: &str) {
        let y = self.lines.len() - 1;
        let x = self.lines[y].len();
        self.paste((x, y), &["", line]);
    }

    pub fn copy_selection(&mut self,
                          (start_x, start_y): (usize, usize),
                          (end_x, end_y): (usize, usize))
                          -> Vec<String> {
        self.changes.break_undo_sequence();

        if start_y == end_y {
            vec![self.lines[start_y][start_x..end_x].to_string()]
        } else {
            let mut lines = Vec::new();
            lines.push(self.lines[start_y][start_x..].to_string());
            lines.extend(self.lines[(start_y + 1)..end_y].to_vec());
            lines.push(self.lines[end_y][0..end_x].to_string());
            lines
        }
    }

    fn _cut_selection_no_change(&mut self,
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

    pub fn cut_selection(&mut self, start: (usize, usize), end: (usize, usize)) -> Vec<String> {
        let removed = self._cut_selection_no_change(start, end);

        self.changes.add_remove(start, removed.clone());

        removed
    }

    fn _paste_no_change<S: AsRef<str>>(&mut self, (x, y): (usize, usize), lines: &[S]) {
        if let Some((last, init)) = lines.split_last() {
            self.modified = true;

            if let Some((first, mids)) = init.split_first() {
                let rest_of_line = self.lines[y].split_off(x);
                let last_insert_line = last.as_ref().to_string() + &rest_of_line;

                let succ = self.lines.drain((y + 1)..).collect::<Vec<_>>();

                self.lines[y].push_str(first.as_ref());
                self.lines.extend(mids.iter()
                                      .map(|s| s.as_ref().to_string())
                                      .chain(once(last_insert_line))
                                      .chain(succ));
            } else {
                self.lines[y].insert_str(x, last.as_ref())
            }
        }
    }

    pub fn paste<S: AsRef<str>>(&mut self, pos: (usize, usize), lines: &[S]) {
        self._paste_no_change(pos, lines);

        if !lines.is_empty() {
            self.changes.add_insert(pos, lines.iter().map(|s| s.as_ref().to_string()).collect())
        }
    }

    /// If there was something to undo, return position where undoing happened
    pub fn undo(&mut self) -> Option<(usize, usize)> {
        self.changes.undo().map(|change| match change {
            Change::Insert(pos, lines) => {
                let (start_x, start_y) = pos;
                let end = if lines.len() == 1 {
                    (start_x + lines[0].len(), start_y)
                } else {
                    (lines[lines.len() - 1].len(), start_y + lines.len() - 1)
                };

                self._cut_selection_no_change(pos, end);

                self.changes.changes.push(Change::Remove(pos, lines.clone()));
                pos
            }
            Change::Remove(pos, lines) => {
                self._paste_no_change(pos, &lines);

                self.changes.changes.push(Change::Insert(pos, lines.clone()));
                pos
            }
        })
    }
}
