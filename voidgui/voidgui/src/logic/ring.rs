use std::{
  cell::{RefCell, RefMut},
  rc::Rc,
};

use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::{
  render::{painter::Painter, Point},
  widgets::traits::{
    CallbackResult, ClickSink, Drawable, InputEvent, InputSink, Parent,
    Transient, WidgetError,
  },
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mark {
  #[cfg(test)]
  Test,
  None,
  Window,
  Spreadsheet,
  SpreadsheetInputField,
  Toolbar,
  ToolbarDropdown,
}

type Wrap<T> = Rc<RefCell<T>>;

pub trait RingMember {
  fn push_to_ring(&self, ring: &mut Ring);
}

// TODO: add pull cache?
pub struct Ring {
  widgets: Vec<(Wrap<dyn Drawable>, Mark, Mark, usize)>,
  parents: Vec<(Wrap<dyn Parent>, Mark)>,
  click_sinks: Vec<(Wrap<dyn ClickSink>, Mark)>,
  input_sinks: Vec<(Wrap<dyn InputSink>, Mark)>,
  transient: Option<(Wrap<dyn Transient>, Mark)>,
}

impl Ring {
  pub fn new() -> Self {
    Self {
      widgets: vec![],
      click_sinks: vec![],
      parents: vec![],
      input_sinks: vec![],
      transient: None,
    }
  }

  pub fn push(
    &mut self,
    w: Wrap<dyn Drawable>,
    m: Mark,
    parent: Mark,
    n: usize,
  ) {
    assert!(m != parent);
    self.widgets.push((w, m, parent, n));
  }

  pub fn push_parent(&mut self, w: Wrap<dyn Parent>, m: Mark) {
    self.parents.push((w, m));
  }

  pub fn push_click_sink(&mut self, w: Wrap<dyn ClickSink>, m: Mark) {
    self.click_sinks.push((w, m));
  }

  pub fn push_input_sink(&mut self, w: Wrap<dyn InputSink>, m: Mark) {
    self.input_sinks.push((w, m));
  }

  pub fn replace_transient(&mut self, w: Wrap<dyn Transient>, m: Mark) {
    self.transient = Some((w, m));
  }

  pub fn pull(&self, mark: &Mark) -> Option<Wrap<dyn Drawable>> {
    self
      .widgets
      .iter()
      .position(|(_, m, _, _)| *m == *mark)
      .map(|i| self.widgets.get(i).unwrap().0.clone())
  }

  pub fn pull_parent(&self, mark: &Mark) -> Option<Wrap<dyn Parent>> {
    self
      .parents
      .iter()
      .position(|(_, m)| *m == *mark)
      .map(|i| self.parents.get(i).unwrap().0.clone())
  }

  pub fn for_each<F>(&mut self, mut f: F)
  where
    F: FnMut(RefMut<dyn Drawable>) -> (),
  {
    for (w, _, _, _) in self.widgets.iter_mut() {
      f(w.borrow_mut());
    }
  }

  pub fn delete(&mut self, m: Mark) -> bool {
    let len = self.widgets.len();

    self.widgets.retain(|(_, _m, _, _)| *_m != m);
    self.parents.retain(|(_, _m)| *_m != m);
    self.click_sinks.retain(|(_, _m)| *_m != m);
    self.input_sinks.retain(|(_, _m)| *_m != m);
    if let Some((_, _m)) = &self.transient {
      if *_m == m {
        self.transient = None;
      }
    }

    len != self.widgets.len()
  }

  /// Draw all owned widgets, plotting if needed.
  pub fn draw(&mut self, painter: &Painter) -> Vec<WidgetError> {
    let p = self.widgets.iter().try_for_each(|(w, _m, p, n)| {
      let mut w = w.borrow_mut();
      if !w.plotted() {
        if *p != Mark::None {
          let o = self
            .pull_parent(p)
            .ok_or(WidgetError::Unspecified("Invalid parent mark".to_owned()))?
            .borrow()
            .nth_child(*n)
            .ok_or(WidgetError::Unspecified(format!(
              "Child out of bounds: {}",
              n
            )))?;
          w.set_origin(&o);
        }
        unsafe { w.plot(painter) }
      } else {
        Ok(())
      }
    });
    if let Err(e) = p {
      return vec![e];
    }

    self
      .widgets
      .iter()
      .map(|(w, _, _, _)| {
        let w = w.borrow_mut();
        w.draw(painter)
      })
      .filter_map(|r| r.err())
      .collect()
  }

  /// Push click event to all click sinks from last to first until one of them
  /// handles it.
  pub fn catch_click(&mut self, painter: &Painter, p: Point) {
    let mut r = CallbackResult::Skip;
    let mut m = Mark::None;

    for (w, _m) in self.click_sinks.iter().rev() {
      match w.borrow().handle_click(painter, p) {
        CallbackResult::Skip => continue,
        res => {
          r = res;
          m = *_m;
          break;
        }
      }
    }

    self.hanle_callback_result(painter, r, "Click", m);
  }

  /// Act on callback result.
  /// Returns `false` if `r` is [CallbackResult::Skip], `true otherwise`.
  /// `what` and `who` are used to describe callback in case of error:
  /// `what` describes the type of callback, `who` - marked object, sourcing
  /// the callback.
  fn hanle_callback_result(
    &mut self,
    p: &Painter,
    r: CallbackResult,
    what: &str,
    who: Mark,
  ) -> bool {
    if r.is_skip() {
      return false;
    }

    match r {
      CallbackResult::Error(e) => {
        println!("{} callback for {:?} failed: {}", what, who, e);
      }
      CallbackResult::Push(x) => x.push_to_ring(self),
      CallbackResult::Modify(m, f) => f(self.pull(&m), p),

      _ => {}
    }
    return true;
  }

  /// Push char to last input sink.
  pub fn catch_input_event(&self, p: &Painter, e: InputEvent) {
    self.input_sinks.last().iter().for_each(|(w, _)| {
      match w.borrow_mut().handle_event(p, &e) {
        CallbackResult::Error(e) => {
          println!("Input callback failed: {}", e);
        }
        _ => {}
      }
    });
  }

  /// Checks event for being a control event: `cancel` (Esc, click off, etc.)
  /// or `accept` (Enter, "ok" button, etc.) and passes it to last transient
  /// if it is.
  /// Returns `true` if event was consumed, `false` otherwise.
  pub fn handle_transient_control_event(
    &mut self,
    p: &Painter,
    e: &WindowEvent,
  ) -> bool {
    if !self.transient.is_some() {
      return false;
    }
    let (t, m) = self.transient.as_ref().unwrap();
    let m = m.clone();

    match e {
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        let r = t.borrow().handle_cancel(p);
        self.delete(m);
        self.hanle_callback_result(p, r, "Cancel", m);
        true
      }
      WindowEvent::Key(Key::Enter | Key::KpEnter, _, Action::Press, mods)
        if !mods.contains(Modifiers::Control) =>
      {
        let r = t.borrow().handle_accept(p);
        self.delete(m);
        self.hanle_callback_result(p, r, "Accept", m);
        true
      }
      _ => false,
    }
  }
}

#[cfg(test)]
mod tests {
  use std::{cell::RefCell, rc::Rc};

  use crate::{
    render::painter::Painter,
    widgets::traits::{widget::WidgetError, Drawable, Widget},
  };

  use super::Ring;

  struct W {
    fail_plot: bool,
    fail_draw: bool,
    plotted: bool,
  }

  impl W {
    fn new(fail_plot: bool, fail_draw: bool, plotted: bool) -> Self {
      Self {
        fail_plot,
        fail_draw,
        plotted,
      }
    }

    pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
      let rc = Rc::new(RefCell::new(self));
      ring.push(
        rc,
        crate::logic::ring::Mark::Test,
        crate::logic::ring::Mark::None,
        0,
      );
    }
  }

  impl Widget for W {
    fn plotted(&self) -> bool {
      self.plotted
    }

    fn set_origin(&mut self, _: &crate::render::Origin) {
      todo!()
    }

    fn request_plot(&mut self) {
      todo!()
    }
  }

  impl Drawable for W {
    unsafe fn plot(&mut self, _: &Painter) -> Result<(), WidgetError> {
      if self.fail_plot {
        Err(WidgetError::Unspecified("plot failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn draw(&self, _: &Painter) -> Result<(), WidgetError> {
      if self.fail_draw {
        Err(WidgetError::Unspecified("draw failed".to_owned()))
      } else {
        Ok(())
      }
    }
  }

  #[test]
  fn add_rem() {
    let mut r = Ring::new();
    let w = W::new(false, false, false);
    w.push_to_ring(&mut r);

    assert_eq!(r.delete(super::Mark::Test), true);
    assert_eq!(r.delete(super::Mark::Test), false);
  }

  #[test]
  fn error_handling() {
    let p = Painter::new(0, 0);
    let mut r = Ring::new();

    // all good
    let norm = W::new(false, false, false);
    norm.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 0);

    // drawing failed
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("draw failed".to_owned())
    );

    // drawing doesn't fail fast
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 2);

    // plotting failed, drawing never tried
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      WidgetError::Unspecified("plot failed".to_owned())
    );

    // plotting fails fast
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 1);
  }

  #[test]
  fn checks_for_plotting() {
    let p = Painter::new(0, 0);
    let mut r = Ring::new();
    let norm = W::new(false, false, false);
    let plotted = W::new(true, false, true);
    norm.push_to_ring(&mut r);
    plotted.push_to_ring(&mut r);
    assert_eq!(r.draw(&p).len(), 0);
  }
}
