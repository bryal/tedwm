#![feature(slice_patterns, fnbox, inclusive_range_syntax)]

extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate sequence_trie;
extern crate termion;
extern crate unicode_segmentation;
extern crate unicode_width;
extern crate futures;
extern crate tokio_timer;
#[macro_use]
extern crate clap;
#[cfg(feature = "profiling")]
extern crate cpuprofiler;

mod lib;

use clap::{Arg, App};
#[cfg(feature = "profiling")]
use cpuprofiler::PROFILER;
use lib::tui::start_tui;

macro_rules! println_err {
    ($fmt:expr $(, $args:expr )*) => {
        write!(io::stderr(), "{}\n\r", format!($fmt, $($args),*))
            .expect("Failed to write to stderr")
    };
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
