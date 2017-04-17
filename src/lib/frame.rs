use lib::*;
use lib::buffer::Buffer;
use lib::input::{Key, Cmd};
use lib::tui::Tui;
use lib::window::Window;
use std::{mem, ptr};
use std::boxed::FnBox;
use std::cell::RefCell;
use std::iter::{repeat, once};
use std::rc::{Rc, Weak};
use unicode_segmentation::UnicodeSegmentation;

#[derive(PartialEq, Eq, Debug)]
pub struct RenderingSection {
    pub x: u16,
    pub y: u16,
    pub lines: Vec<String>,
}

#[derive(Clone)]
enum _Partition {
    Window(Rc<RefCell<Window>>),
    SplitH(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
    SplitV(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>),
}

#[derive(Clone)]
pub struct Partition {
    content: _Partition,
    parent: Option<Weak<RefCell<Partition>>>,
    is_first_child: bool,
}

impl Partition {
    pub fn new_from_window(window: Rc<RefCell<Window>>,
                           first_child_of_parent: Option<(bool, Weak<RefCell<Partition>>)>)
                           -> Rc<RefCell<Partition>> {
        let (first_child, parent) = match first_child_of_parent {
            Some((first_child, parent)) => (first_child, Some(parent)),
            None => (true, None),
        };

        let part = Rc::new(RefCell::new(Partition {
            content: _Partition::Window(window.clone()),
            parent: parent,
            is_first_child: first_child,
        }));

        window.borrow_mut().parent_partition = Rc::downgrade(&part);

        part
    }

    pub fn new_root_from_window(window: Rc<RefCell<Window>>) -> Rc<RefCell<Partition>> {
        Partition::new_from_window(window, None)
    }

    pub fn new_root_from_buffer(w: u16,
                                h: u16,
                                buffer: Rc<RefCell<Buffer>>)
                                -> (Rc<RefCell<Partition>>, Rc<RefCell<Window>>) {
        let window = Rc::new(RefCell::new(Window::new(w, h, buffer, Weak::new())));
        (Partition::new_root_from_window(window.clone()), window)
    }

    pub fn new_window(w: u16,
                      h: u16,
                      buffer: Rc<RefCell<Buffer>>,
                      is_first_child: bool,
                      parent: Option<Weak<RefCell<Partition>>>)
                      -> (Rc<RefCell<Partition>>, Rc<RefCell<Window>>) {
        let window = Rc::new(RefCell::new(Window::new(w, h, buffer, Weak::new())));
        (Partition::new_from_window(window.clone(), parent.map(|p| (is_first_child, p))), window)
    }

    pub fn size(&self) -> (u16, u16) {
        match self.content {
            _Partition::Window(ref window) => {
                window.borrow().size()
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

    fn set_children_parent(&mut self, new_parent: Weak<RefCell<Partition>>) {
        match self.content {
            _Partition::Window(ref mut window) => window.borrow_mut().parent_partition = new_parent,
            _Partition::SplitH(ref first, ref second) |
            _Partition::SplitV(ref first, ref second) => {
                first.borrow_mut().parent = Some(new_parent.clone());
                second.borrow_mut().parent = Some(new_parent);
            }
        };
    }

    fn split_new_window<F: FnOnce(Rc<RefCell<Partition>>, Rc<RefCell<Partition>>) -> _Partition,
                        G: FnOnce(u16, u16) -> ((u16, u16), (u16, u16))>
        (this: Rc<RefCell<Partition>>,
         split_dir: F,
         new_sizes: G,
         buffer: Rc<RefCell<Buffer>>) {

        let (first_partition, (second_w, second_h)) = {
            let mut this_b = this.borrow_mut();
            let (w, h) = this_b.size();
            let ((first_w, first_h), second) = new_sizes(w, h);

            this_b.set_size(first_w, first_h);

            let first_partition = Rc::new(RefCell::new(this_b.clone()));

            {
                let mut first_b = first_partition.borrow_mut();
                first_b.parent = Some(Rc::downgrade(&this));
                first_b.is_first_child = true;
            }

            this_b.set_children_parent(Rc::downgrade(&first_partition));

            (first_partition, second)
        };

        let (second_partition, _) = Partition::new_window(second_w,
                                                          second_h,
                                                          buffer,
                                                          false,
                                                          Some(Rc::downgrade(&this)));

        this.borrow_mut().content = split_dir(first_partition, second_partition);
    }

    pub fn split_new_window_h(this: Rc<RefCell<Partition>>, buffer: Rc<RefCell<Buffer>>) {
        Partition::split_new_window(this,
                                    _Partition::SplitH,
                                    |w, h| ((w / 2, h), ((w - 1) / 2, h)),
                                    buffer)
    }

    pub fn split_new_window_v(this: Rc<RefCell<Partition>>, buffer: Rc<RefCell<Buffer>>) {
        Partition::split_new_window(this,
                                    _Partition::SplitV,
                                    |w, h| ((w, (h + 1) / 2), (w, h / 2)),
                                    buffer)
    }

    /// Get the absolute position in terminal cells of this window partition in the frame
    pub fn pos_in_frame(&self) -> (u16, u16) {
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

    pub fn is_first_in_frame(&self) -> bool {
        if let Some(ref parent_weak) = self.parent {
            let parent = parent_weak.upgrade().unwrap();
            self.is_first_child && parent.borrow().is_first_in_frame()
        } else {
            true
        }
    }

    pub fn is_last_in_frame(&self) -> bool {
        if let Some(ref parent_weak) = self.parent {
            let parent = parent_weak.upgrade().unwrap();
            !self.is_first_child && parent.borrow().is_last_in_frame()
        } else {
            true
        }
    }

    /// Returns the `n`th leaf partition following or preceding this partition
    pub fn cycle(this: Rc<RefCell<Partition>>, n: isize) -> Rc<RefCell<Partition>> {
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
    pub fn first_leaf(this: Rc<RefCell<Partition>>) -> Rc<RefCell<Partition>> {
        let this_b = this.borrow();
        match this_b.content {
            _Partition::Window(_) => this.clone(),
            _Partition::SplitH(ref left, _) => Partition::first_leaf(left.clone()),
            _Partition::SplitV(ref top, _) => Partition::first_leaf(top.clone()),
        }
    }

    /// Returns the leftmost, topmost leaf in this partition
    pub fn first_window(&self) -> Rc<RefCell<Window>> {
        match self.content {
            _Partition::Window(ref window) => window.clone(),
            _Partition::SplitH(ref left, _) => left.borrow().first_window(),
            _Partition::SplitV(ref top, _) => top.borrow().first_window(),
        }
    }

    /// Returns the rightmost, bottommost leaf in this partition
    pub fn last_window(&self) -> Rc<RefCell<Window>> {
        match self.content {
            _Partition::Window(ref window) => window.clone(),
            _Partition::SplitH(_, ref right) => right.borrow().last_window(),
            _Partition::SplitV(_, ref bot) => bot.borrow().last_window(),
        }
    }

    pub fn window(&self) -> Option<Rc<RefCell<Window>>> {
        match self.content {
            _Partition::Window(ref window) => Some(window.clone()),
            _ => None,
        }
    }

    /// Delete this partition from parent and unsplit the partition
    ///
    /// If this partition was the only child, do not delete and return `None`,
    /// otherwise, return the sibling
    pub fn delete(&self) -> Option<Rc<RefCell<Partition>>> {
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

            let mut sibling_owned = Rc::try_unwrap(sibling)
                .unwrap_or_else(|_| panic!("Sibling was not unique reference"))
                .into_inner();

            sibling_owned.set_size(parent_w, parent_h);

            sibling_owned.set_children_parent(parent_weak);

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

pub struct Prompt {
    len: usize,
    pub window: Rc<RefCell<Window>>,
    pub subject_window: ActiveWindow,
    pub callback: Box<for<'t, 'i> FnBox(&'t mut Tui, &'i str)>,
}

impl Prompt {
    pub fn input(&self) -> String {
        let w = self.window.borrow();
        let b = w.buffer().borrow();
        b.to_string()[self.len..].to_string()
    }
}

pub struct Minibuffer {
    w: u16,
    pub prompt_stack: Vec<Prompt>,
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

    pub fn echo(&mut self, s: &str) {
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

    pub fn clear_echo(&mut self) {
        self._echo = None;
    }

    /// Returns the created prompt window
    pub fn push_new_prompt(&mut self,
                           subject: ActiveWindow,
                           prompt_s: &str,
                           default: &str,
                           callback: Box<FnBox(&mut Tui, &str)>)
                           -> Rc<RefCell<Window>> {
        let mut buf = Buffer::new("*minibuffer*");
        buf.insert_str(0, 0, prompt_s);
        buf.insert_str(prompt_s.len(), 0, default);
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
pub enum ActiveWindow {
    Prompt(Rc<RefCell<Window>>),
    Window(Rc<RefCell<Window>>),
}

impl ActiveWindow {
    pub fn window(&self) -> &Rc<RefCell<Window>> {
        match *self {
            ActiveWindow::Prompt(ref w) => &w,
            ActiveWindow::Window(ref w) => &w,
        }
    }
}

/// A frame of windows into buffers
pub struct Frame {
    /// Width in terminal columns
    w: u16,
    /// Height in terminal lines
    h: u16,
    /// Windows into the buffers to show in this frame, including the minibuffer window
    pub windows: Rc<RefCell<Partition>>,
    /// The index of the active window in `self.windows`
    pub active_window: ActiveWindow,
    pub minibuffer: Minibuffer,
}

impl Frame {
    pub fn new(w: u16, h: u16, buffer: Rc<RefCell<Buffer>>) -> Frame {
        let (windows, window) = Partition::new_root_from_buffer(w, h - 1, buffer);

        let f = Frame {
            w: w,
            h: h,
            windows: windows,
            active_window: ActiveWindow::Window(window),
            minibuffer: Minibuffer::new(w),
        };

        f.active_window.window().borrow_mut().is_active = true;

        f
    }

    pub fn size(&self) -> (u16, u16) {
        (self.w, self.h)
    }

    pub fn resize(&mut self, w: u16, h: u16) {
        let changed = self.w != w || self.h != h;

        if changed {
            self.w = w;
            self.h = h;
            self.windows.borrow().set_size(w, h - 1);
            self.minibuffer.set_width(w);
            self.active_window.window().borrow_mut().reposition_view();
        }
    }

    pub fn render(&mut self) -> Vec<RenderingSection> {
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
