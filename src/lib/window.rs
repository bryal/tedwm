use lib::*;
use lib::buffer::Buffer;
use lib::frame::Partition;
use std::cell::RefCell;
use std::cmp::{min, max, Ordering};
use std::iter::{once, repeat};
use std::rc::{Weak, Rc};
use std::str;
use unicode_segmentation::UnicodeSegmentation;

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

/// Describes whether a move was prevented by the point being at the
/// beginning or end of buffer
pub enum MoveRes {
    Beg,
    End,
    Ok,
}

/// A window into a buffer
#[derive(Clone)]
pub struct Window {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// The index of the leftmost column of the buffer visible in the window
    left: usize,
    /// The index of the topmost line of the buffer visible in the window
    top: usize,
    pub parent_partition: Weak<RefCell<Partition>>,
    buffer: Rc<RefCell<Buffer>>,
    point: Point,
    mark: Option<Point>,
    /// Whether this is the active window
    pub is_active: bool,
}

impl Window {
    pub fn new(w: u16,
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

    pub fn buffer(&self) -> &Rc<RefCell<Buffer>> {
        &self.buffer
    }

    pub fn point(&self) -> &Point {
        &self.point
    }

    pub fn size(&self) -> (u16, u16) {
        (self.w, self.h)
    }

    pub fn set_size(&mut self, w: u16, h: u16) {
        self.w = w;
        self.h = h;
    }

    pub fn pos_in_buffer(&self) -> (usize, usize) {
        (self.left, self.top)
    }

    /// Get the absolute position in terminal cells of this window in the frame
    pub fn pos_in_frame(&self) -> (u16, u16) {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.pos_in_frame()
    }

    pub fn is_first_in_frame(&self) -> bool {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.is_first_in_frame()
    }

    pub fn is_last_in_frame(&self) -> bool {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.is_last_in_frame()
    }

    pub fn set_mark_at_point(&mut self) {
        self.mark = Some(self.point)
    }

    /// Move point horizontally by `n` graphemes
    ///
    /// Returns whether move was prevented by beginning/end of buffer
    pub fn move_point_h(&mut self, n_with_dir: isize) -> MoveRes {
        let buffer = self.buffer.borrow();

        let forward = n_with_dir >= 0;
        let mut n = n_with_dir.abs() as usize;

        let mut line = &buffer.lines()[self.point.line_i];

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
                if self.point.line_i + 1 >= buffer.lines().len() {
                    return MoveRes::End;
                }
                self.point.line_i += 1;
                line = &buffer.lines()[self.point.line_i];
                self.point.col_byte_i = 0;
            } else {
                if self.point.line_i == 0 {
                    return MoveRes::Beg;
                }
                self.point.line_i -= 1;
                line = &buffer.lines()[self.point.line_i];
                self.point.col_byte_i = line.len();
            }
        }
    }

    /// Move point to an absolute line number
    pub fn move_point_to_line(&mut self, line_i: isize) -> MoveRes {
        let buffer = self.buffer.borrow();
        let n_lines = buffer.lines().len();

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

        let line = &buffer.lines()[self.point.line_i];
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
    pub fn move_point_v(&mut self, n: isize) -> MoveRes {
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
    pub fn page_up(&mut self) -> MoveRes {
        self.page_v(true)
    }

    /// Move point downward by near full window height
    ///
    /// Returns whether move was prevented by end of buffer
    pub fn page_down(&mut self) -> MoveRes {
        self.page_v(false)
    }

    /// Move point to the end of the line
    pub fn move_point_to_end_of_line(&mut self) {
        let buffer = self.buffer.borrow();
        let line = &buffer.lines()[self.point.line_i];
        self.point.col_byte_i = line.len();
        self.point.update_col_i(&buffer)
    }

    /// Move point to the end of the buffer
    pub fn move_point_to_end_of_buffer(&mut self) {
        {
            let buffer = self.buffer.borrow();
            let n_lines = buffer.lines().len();
            self.point.line_i = n_lines - 1;
        }
        self.move_point_to_end_of_line()
    }

    /// Returns whether the search succeded
    pub fn search_forward(&mut self, query: &str) -> bool {
        let buffer = self.buffer.clone();
        let buffer_b = buffer.borrow();
        let lines = buffer_b.lines();

        let search_line_is = once((self.point.col_byte_i, self.point.line_i))
            .chain(((self.point.line_i + 1)..lines.len())
                .chain(0..self.point.line_i)
                .map(|y| (0, y)));

        for (x_offset, y) in search_line_is {
            let line = &lines[y];
            if let Some(i) = line[x_offset..].find(query).map(|i| i + x_offset) {
                self.point.line_i = y;
                self.point.col_byte_i = i;
                self.point.update_col_i(&buffer_b);
                self.set_mark_at_point();
                self.point.col_byte_i += query.len();
                self.point.update_col_i(&buffer_b);
                return true;
            }
        }
        false
    }

    /// Returns whether the search succeded
    pub fn search_backward(&mut self, query: &str) -> bool {
        let buffer = self.buffer.clone();
        let buffer_b = buffer.borrow();
        let lines = buffer_b.lines();

        let same_line = if self.point.col_byte_i > 0 {
            Some((Some(self.point.col_byte_i - 1), self.point.line_i))
        } else {
            None
        };

        let search_line_is = same_line.into_iter()
                                      .chain((0..self.point.line_i)
                                          .rev()
                                          .chain(((self.point.line_i + 1)..lines.len()).rev())
                                          .map(|y| (None, y)));

        for (lim, y) in search_line_is {
            let line = &lines[y];
            let lim = lim.unwrap_or(line.len());
            if let Some(i) = line[0..lim].rfind(query) {
                self.point.line_i = y;
                self.point.col_byte_i = i;
                self.point.update_col_i(&buffer_b);
                self.set_mark_at_point();
                self.point.col_byte_i += query.len();
                self.point.update_col_i(&buffer_b);
                return true;
            }
        }
        false
    }

    /// Insert a string at point
    pub fn insert_str_at_point(&mut self, s: &str) {
        let mut buffer = self.buffer.borrow_mut();
        buffer.insert_str(self.point.col_byte_i, self.point.line_i, s);

        self.mark = None;
        self.point.col_byte_i += s.len();
        self.point.update_col_i(&buffer);
        self.point.prev_col_i = self.point.col_i;
    }

    /// Insert a character at point
    pub fn insert_char_at_point(&mut self, c: char) {
        let mut s = [0; 4];
        self.insert_str_at_point(c.encode_utf8(&mut s))
    }

    pub fn insert_new_line(&mut self) {
        self.buffer.borrow_mut().insert_new_line(self.point.col_byte_i, self.point.line_i);
        self.mark = None;
        self.point.line_i += 1;
        self.point.col_byte_i = 0;
        self.point.col_i = 0;
        self.point.prev_col_i = 0;
    }

    /// Insert tab or spaces
    pub fn insert_tab(&mut self) {
        if TAB_INSERTS_SPACES {
            let spaces = vec![' ' as u8; TAB_WIDTH];
            self.insert_str_at_point(str::from_utf8(&spaces).unwrap());
        } else {
            self.insert_char_at_point('\t');
        }
    }

    /// Copy text between mark and point. Return `None` if mark is not set
    pub fn copy_selection(&mut self) -> Option<Vec<String>> {
        self.mark.map(|mark| {
            let (start, end) = (min(self.point, mark), max(self.point, mark));
            let lines = self.buffer.borrow_mut().copy_selection((start.col_byte_i, start.line_i),
                                                                (end.col_byte_i, end.line_i));
            self.mark = None;
            lines
        })
    }

    /// Cut text between mark and point. Return `None` if mark is not set
    pub fn cut_selection(&mut self) -> Option<Vec<String>> {
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
    pub fn paste(&mut self, lines: &[String]) {
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
    pub fn delete_chars_h(&mut self, n: isize) -> MoveRes {
        self.set_mark_at_point();
        let move_res = self.move_point_h(n);
        self.cut_selection();
        move_res
    }

    pub fn switch_to_buffer(&mut self, buffer: Rc<RefCell<Buffer>>) {
        self.buffer = buffer;
        self.mark = None;
        self.point = Point::new()
    }

    pub fn cancel(&mut self) {
        self.mark = None;
    }

    /// Position the view of the window such that the pointer is in view.
    pub fn reposition_view(&mut self) {
        let buf = self.buffer.borrow();
        let ver_margin = max(1, (self.h as f32 * SCROLL_MARGIN_V) as i32);
        let margin_top = self.top as i32 + ver_margin;
        let margin_bot = self.top as i32 + self.h as i32 - ver_margin;
        let move_relative_up = min(self.point.line_i as i32 - margin_top, 0);
        let move_relative_down = max(self.point.line_i as i32 - margin_bot, 0) + move_relative_up;
        let buf_bot = buf.lines().len();

        let new_view_top = min(buf_bot,
                               max(0, self.top as i32 + move_relative_down) as usize);
        self.top = new_view_top;

        let hor_margin = max(1, (self.w as f32 * SCROLL_MARGIN_H) as i32);
        let margin_left = self.left as i32 + hor_margin;
        let margin_right = self.left as i32 + self.w as i32 - hor_margin;
        let move_relative_left = min(self.point.col_i as i32 - margin_left, 0);
        let move_relative_right = max(self.point.col_i as i32 - margin_right, 0) +
                                  move_relative_left;
        let rightmost_col = buf.lines()[self.top..]
            .iter()
            .take(self.h as usize)
            .map(|l| display_width(&l))
            .max()
            .unwrap_or(0);

        let new_view_left = min(rightmost_col,
                                max(0, self.left as i32 + move_relative_right) as usize);
        self.left = new_view_left;
    }

    pub fn split_h(this: Rc<RefCell<Window>>) {
        let (parent, buffer) = {
            let this_b = this.borrow();
            (this_b.parent_partition.upgrade().expect("Failed to upgrade parent"),
             this_b.buffer.clone())
        };
        Partition::split_new_window_h(parent, buffer)
    }

    pub fn split_v(this: Rc<RefCell<Window>>) {
        let (parent, buffer) = {
            let this_b = this.borrow();
            (this_b.parent_partition.upgrade().expect("Failed to upgrade parent"),
             this_b.buffer.clone())
        };
        Partition::split_new_window_v(parent, buffer)
    }

    /// Delete this window from parent and unsplit the partition
    ///
    /// If this window was the only child, do not delete and return `None`,
    /// otherwise, return the first sibling
    pub fn delete(&self) -> Option<Rc<RefCell<Window>>> {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let parent_b = parent.borrow();
        parent_b.delete().map(|part| part.borrow().first_window())
    }

    /// Select the `n`th window following or preceding this window
    /// in cyclic ordering.
    pub fn other_window(&self, n: isize) -> Rc<RefCell<Window>> {
        let parent = self.parent_partition.upgrade().expect("Failed to upgrade parent");
        let other_leaf_partition = Partition::cycle(parent, n);
        let other_leaf_partition = other_leaf_partition.borrow();
        other_leaf_partition.window().expect("Other partition was not leaf partition")
    }

    pub fn render_lines(&self) -> Vec<String> {
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

        if buffer.lines().len() < self.top {
            vec![n_spaces(self.w as usize); self.h as usize]
        } else {
            buffer.lines()[self.top..]
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
        let r = self.point.line_i as f32 / max(1, self.buffer.borrow().lines().len() - 1) as f32;
        let s = format!(" {} {}  L{}:{}% C{}",
                        if buffer.is_modified() { "*" } else { "-" },
                        buffer.name(),
                        self.point
                            .line_i + 1,
                        (r * 100.0).round() as u8,
                        self.point.col_i);
        let bg = if self.is_active { &*COLOR_BG_MODELINE } else { &*COLOR_BG_MODELINE_INACTIVE };
        format!("{}{}{}", bg, pad_line(s, self.w), *COLOR_BG)
    }

    pub fn render(&self) -> Vec<String> {
        let mut rendering_lines = self.render_lines();
        self.color_selection(&mut rendering_lines);
        rendering_lines.push(self.render_modeline());
        rendering_lines
    }
}
