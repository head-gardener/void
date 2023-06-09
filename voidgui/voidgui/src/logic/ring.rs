use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use glfw::{Action, Key, Modifiers, WindowEvent};

use rayon::prelude::*;

use crate::{
  render::{
    painter::{Drone, DroneFeed},
    Point,
  },
  widgets::{
    self,
    traits::{
      ClickSink, Drawable, InputEvent, InputSink, KeySink, Parent, Transient,
    },
    Spreadsheet,
  },
  Description,
};

use super::CallbackResult;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mark {
  _Test1,
  _Test2,
  None,
  Window,
  Spreadsheet,
  InputFloat,
  Toolbar,
  ToolbarDropdown,
  DamageTracker,
  Status(u64),
  StatusBox,
  File(u32),
}

impl Into<usize> for Mark {
  fn into(self) -> usize {
    match self {
      Mark::_Test1 => 0,
      Mark::_Test2 => 1,
      _ => todo!(),
    }
  }
}

pub type Wrap<T> = Arc<RwLock<T>>;
pub type Unwrapped<'a, T> = RwLockWriteGuard<'a, T>;

pub fn wrap<T: Send + Sync>(x: T) -> Wrap<T> {
  Arc::new(RwLock::new(x))
}

pub trait RingElement {
  fn push_to_ring(&self, ring: RwLockWriteGuard<Ring>);
}

pub type WidgetElement = (Wrap<dyn Drawable>, Mark, Mark, usize);
pub type Element<T> = (Wrap<T>, Mark);

pub struct Ring {
  widgets: Vec<WidgetElement>,
  parents: Vec<Element<dyn Parent>>,
  transients: Vec<Element<dyn Transient>>,

  click_sinks: Vec<Element<dyn ClickSink>>,
  input_sinks: Vec<Element<dyn InputSink>>,
  key_sinks: Vec<Element<dyn KeySink>>,
}

impl Ring {
  pub fn new() -> Self {
    Self {
      widgets: vec![],
      parents: vec![],
      transients: vec![],
      click_sinks: vec![],
      input_sinks: vec![],
      key_sinks: vec![],
    }
  }

  /// Same as [Ring::push_dynamic], but the child n in known ahead of time - i.e.
  /// parent is static
  pub fn push_static(
    &mut self,
    w: Wrap<dyn Drawable>,
    m: Mark,
    parent: Mark,
    n: usize,
  ) {
    self.widgets.push((w, m, parent, n));
  }

  /// Same as [Ring::push_static], but the child n in polled from parent.
  pub fn push_dynamic(&mut self, w: Wrap<dyn Drawable>, m: Mark, parent: Mark) {
    let s = w.write().unwrap().size();
    let n = self
      .pull_parent(&parent)
      .unwrap()
      .write()
      .unwrap()
      .add_child(s);
    self.widgets.push((w, m, parent, n));
  }

  pub fn push_parent(&mut self, w: Wrap<dyn Parent>, m: Mark) {
    self.parents.push((w, m));
  }

  pub fn push_transient(
    &mut self,
    w: Wrap<dyn Transient>,
    m: Mark,
    unique: bool,
  ) {
    if unique {
      self.delete(m);
    }
    self.transients.push((w, m));
  }

  pub fn push_click_sink(&mut self, w: Wrap<dyn ClickSink>, m: Mark) {
    self.click_sinks.push((w, m));
  }

  pub fn push_input_sink(&mut self, w: Wrap<dyn InputSink>, m: Mark) {
    self.input_sinks.push((w, m));
  }

  pub fn push_key_sink(&mut self, w: Wrap<dyn KeySink>, m: Mark) {
    self.key_sinks.push((w, m));
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

  pub fn delete(&mut self, m: Mark) -> bool {
    let len = self.widgets.len();

    self.widgets.iter().for_each(|(_, _m, p, n)| {
      if *_m == m {
        self
          .pull_parent(&p)
          .map(|p| p.write().unwrap().pop_child(*n));
      }
    });

    self.widgets.retain(|(_, _m, p, _)| *_m != m && *p != m);
    self.parents.retain(|(_, _m)| *_m != m);

    self.transients.retain(|(_, _m)| *_m != m);
    self.click_sinks.retain(|(_, _m)| *_m != m);
    self.input_sinks.retain(|(_, _m)| *_m != m);
    self.key_sinks.retain(|(_, _m)| *_m != m);

    len != self.widgets.len()
  }

  /// Draw all owned widgets, plotting if needed.
  pub fn draw(
    &mut self,
    desc: Wrap<Description>,
    drone: &Drone,
  ) -> Vec<widgets::Error> {
    let parents = &self.parents;
    let p: Vec<widgets::Error> = self
      .widgets
      .iter()
      .zip(
        self
          .widgets
          .iter()
          .map(|_| drone.new_feed())
          .collect::<Vec<DroneFeed>>()
          .into_iter(),
      )
      .par_bridge()
      .map(|((w, _m, p, n), feed)| {
        let mut w = w.write().unwrap();
        if *p != Mark::None {
          let o = parents
            .iter()
            .position(|(_, m)| *m == *p)
            .map(|i| self.parents.get(i).unwrap().0.clone())
            .ok_or(widgets::Error::Unspecified(
              "Invalid parent mark".to_owned(),
            ))?
            .read()
            .unwrap()
            .nth_child(*n)
            .ok_or(widgets::Error::Unspecified(format!(
              "Child out of bounds: {}",
              n
            )))?;
          w.set_origin(&o);
        }
        if !w.plotted() {
          w.plot(desc.read().unwrap(), feed)
        } else {
          Ok(())
        }
      })
      .filter_map(|r| r.err())
      .collect();
    if p.len() != 0 {
      return p;
    }

    let res = self
      .widgets
      .iter()
      .map(|(w, _, _, _)| {
        let mut w = w.write().unwrap();
        unsafe { w.draw(drone.new_feed()) }
      })
      .filter_map(|r| r.err())
      .collect();
    res
  }

  /// Push click event to all click sinks from last to first until one of them
  /// handles it.
  pub fn catch_click(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    mods: &Modifiers,
    p: Point,
  ) -> (CallbackResult, Mark) {
    for (w, m) in self.click_sinks.iter().rev() {
      match w.write().unwrap().handle_click(desc, drone, p, mods) {
        CallbackResult::Pass => continue,
        res => return (res, *m),
      }
    }
    (CallbackResult::Pass, Mark::None)
  }

  /// Push char to last input sink.
  pub fn catch_input_event(
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: InputEvent,
  ) {
    self.input_sinks.last().iter().for_each(|(w, _)| {
      match w.write().unwrap().handle_event(desc, drone, &e) {
        CallbackResult::Error(e) => {
          println!("Input callback failed: {}", e);
        }
        CallbackResult::None | CallbackResult::Pass => {}

        r => panic!("Unexpected callback result: {:?}", r),
      }
    });
  }

  /// Checks event for being a control event: `cancel` (Esc, click off, etc.)
  /// or `accept` (Enter, "ok" button, etc.) and passes it to last transient
  /// if it is.
  /// Return [CallbackResult::Pass] if event wasn't consumed.
  pub fn catch_transient_control_event(
    &mut self,
    _: &RwLockReadGuard<Description>,
    feed: &DroneFeed,
    e: &WindowEvent,
  ) -> (CallbackResult, Mark) {
    let mut r = CallbackResult::Pass;
    let mut m = Mark::None;
    let mut caught = false;

    for (trans, _m) in self.transients.iter().rev() {
      match e {
        WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
          r = trans.write().unwrap().handle_cancel(feed);
          m = *_m;
          caught = true;
          break;
        }
        WindowEvent::Key(Key::Enter | Key::KpEnter, _, Action::Press, mods)
          if !mods.contains(Modifiers::Control) =>
        {
          r = trans.write().unwrap().handle_accept(feed);
          m = *_m;
          caught = true;
          break;
        }
        _ => {}
      }
    }

    if caught {
      self.delete(m);
    }
    (r, m)
  }

  pub fn catch_key(
    &mut self,
    desc: &Description,
    drone: &Drone,
    e: &WindowEvent,
  ) -> (CallbackResult, Mark) {
    for (w, m) in self.key_sinks.iter().rev() {
      match w.write().unwrap().handle_key(desc, drone, e) {
        CallbackResult::Pass => continue,
        res => return (res, *m),
      }
    }
    (CallbackResult::Pass, Mark::None)
  }

  pub fn kill_transient(
    &mut self,
    drone: &DroneFeed,
    m: Mark,
  ) -> CallbackResult {
    let mut caught = false;
    let mut res = CallbackResult::Pass;
    for (w, _m) in self.transients.iter() {
      if *_m == m {
        res = w.write().unwrap().handle_cancel(drone);
        caught = true;
        break;
      }
    }
    if caught {
      self.delete(m);
    }
    res
  }
}

impl<'a> IntoIterator for &'a mut Ring {
  type Item = &'a mut WidgetElement;
  type IntoIter = std::slice::IterMut<'a, WidgetElement>;

  fn into_iter(self) -> Self::IntoIter {
    self.widgets.iter_mut()
  }
}

impl<'a> IntoParallelIterator for &'a mut Ring {
  type Item = &'a mut WidgetElement;
  type Iter = rayon::slice::IterMut<'a, WidgetElement>;

  fn into_par_iter(self) -> Self::Iter {
    self.widgets.par_iter_mut()
  }
}

pub fn with_spreadsheet(
  f: impl FnOnce(&Description, &Drone, &mut Spreadsheet) + 'static,
) -> Box<dyn FnOnce(Option<Wrap<dyn Drawable>>, &Description, &Drone)> {
  Box::new(move |s, desc, drone| {
    s.expect("Spreadsheet should always be in the ring")
      .write()
      .unwrap()
      .downcast_mut()
      .map(|s| f(desc, drone, s));
  })
}

#[cfg(test)]
mod tests {
  use std::sync::RwLockReadGuard;

  use crate::{
    backend::Backend,
    logic::ring,
    render::{painter::DroneFeed, Size},
    widgets::{
      self,
      traits::{Drawable, Widget},
    },
    Description,
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
      let rc = ring::wrap(self);
      ring.push_static(
        rc,
        crate::logic::ring::Mark::_Test1,
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
    fn plot(
      &mut self,
      _: RwLockReadGuard<Description>,
      _: DroneFeed,
    ) -> Result<(), widgets::Error> {
      if self.fail_plot {
        Err(widgets::Error::Unspecified("plot failed".to_owned()))
      } else {
        Ok(())
      }
    }

    unsafe fn draw(&mut self, _: DroneFeed) -> Result<(), widgets::Error> {
      if self.fail_draw {
        Err(widgets::Error::Unspecified("draw failed".to_owned()))
      } else {
        Ok(())
      }
    }

    fn size(&mut self) -> crate::render::Size {
      Size::new(0, 0)
    }
  }

  #[test]
  fn add_rem() {
    let mut r = Ring::new();
    let w = W::new(false, false, false);
    w.push_to_ring(&mut r);

    assert_eq!(r.delete(super::Mark::_Test1), true);
    assert_eq!(r.delete(super::Mark::_Test1), false);
  }

  #[test]
  fn error_handling() {
    let b = Backend::mock(200, 200);
    let mut r = Ring::new();

    // all good
    let norm = W::new(false, false, false);
    norm.push_to_ring(&mut r);
    let errors = r.draw(b.desc.clone(), &b.drone);
    assert_eq!(errors.len(), 0);

    // drawing failed
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(b.desc.clone(), &b.drone);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      widgets::Error::Unspecified("draw failed".to_owned())
    );

    // drawing doesn't fail fast
    let fail_draw = W::new(false, true, false);
    fail_draw.push_to_ring(&mut r);
    let errors = r.draw(b.desc.clone(), &b.drone);
    assert_eq!(errors.len(), 2);

    // plotting failed, drawing never tried
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(b.desc.clone(), &b.drone);
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      widgets::Error::Unspecified("plot failed".to_owned())
    );

    // plotting doesn't fail fast
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(b.desc.clone(), &b.drone);
    assert_eq!(errors.len(), 2);
  }

  #[test]
  fn checks_for_plotting() {
    let b = Backend::mock(200, 200);
    let mut r = Ring::new();
    let norm = W::new(false, false, false);
    let plotted = W::new(true, false, true);
    norm.push_to_ring(&mut r);
    plotted.push_to_ring(&mut r);
    assert_eq!(r.draw(b.desc.clone(), &b.drone).len(), 0);
  }
}
