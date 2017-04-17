pub mod seq_set;
pub mod buffer;
pub mod window;
pub mod frame;
pub mod tui;
pub mod input;

use termion::color;
use std::cmp::max;
use unicode_segmentation::UnicodeSegmentation;

lazy_static! {
    pub static ref COLOR_BG: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x18, 0x18, 0x10));
    pub static ref COLOR_TEXT: color::Fg<color::Rgb> = color::Fg(color::Rgb(0xF0, 0xE6, 0xD6));
    pub static ref COLOR_BG_SELECTION: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x80, 0x00, 0x38));
    pub static ref COLOR_DIM_TEXT: color::Fg<color::Rgb> = color::Fg(color::Rgb(0x60, 0x56, 0x50));
    pub static ref COLOR_BG_MODELINE: color::Bg<color::Rgb> = color::Bg(color::Rgb(0x4B, 0x4B, 0x40));
    pub static ref COLOR_BG_MODELINE_INACTIVE: color::Bg<color::Rgb> =
        color::Bg(color::Rgb(0x29, 0x29, 0x22));
}

/// Vertical scroll margin as a normalized percentage of window height
pub const SCROLL_MARGIN_V: f32 = 0.3;
/// Horizontal scroll margin as a normalized percentage of window width
pub const SCROLL_MARGIN_H: f32 = 0.02;
/// Whether to insert spaces on TAB press
pub const TAB_INSERTS_SPACES: bool = true;
/// Tab display width / n.o. spaces to convert to
pub const TAB_WIDTH: usize = 4;

/// The width in columns of a string if it is displayed in a terminal.
///
/// Takes into account the current configuration of tab-width
pub fn display_width(s: &str) -> usize {
    use unicode_width::UnicodeWidthStr;
    s.graphemes(true).map(|g| if g == "\t" { TAB_WIDTH } else { g.width() }).sum()
}

/// Pad a line in the editor with spaces so that the number of symbols when printed will equal `len`
pub fn pad_line(mut s: String, len: u16) -> String {
    let padding = n_spaces(max(0, len as isize - display_width(&s) as isize) as usize);
    s.push_str(&padding);
    s
}

pub fn n_spaces(n: usize) -> String {
    (0..n).map(|_| ' ').collect::<String>()
}
