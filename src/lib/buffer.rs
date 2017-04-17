use itertools::Itertools;
use lib::input::Keymap;
use std::fs;
use std::io::{self, Write, BufRead};
use std::iter::once;
use std::path::{PathBuf, Path};

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
}

impl Buffer {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Buffer {
            name: name.into(),
            lines: vec![String::new()],
            filepath: None,
            local_keymap: Keymap::new(),
            modified: false,
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
        &mut self.lines[y].insert_str(x, s);
        self.modified = true;
    }

    pub fn insert_new_line(&mut self, x: usize, y: usize) {
        let rest = self.lines[y].split_off(x);
        self.lines.insert(y + 1, rest);
        self.modified = true;
    }

    pub fn push_line(&mut self, line: String) {
        self.lines.push(line)
    }

    pub fn copy_selection(&mut self,
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
            lines
        }
    }

    pub fn cut_selection(&mut self,
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

    pub fn paste(&mut self, x: usize, y: usize, lines: &[String]) {
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
