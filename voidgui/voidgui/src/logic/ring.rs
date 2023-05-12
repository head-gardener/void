use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use glfw::{Action, Key, Modifiers, WindowEvent};

use rayon::prelude::*;

use crate::{
  render::{
    painter::{Description, Drone, DroneFeed},
    Point,
  },
  widgets::{
    self,
    traits::{
      ClickSink, Drawable, InputEvent, InputSink, KeySink, Parent, Transient,
    },
  },
};

use super::DamageTracker;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mark {
  _Test1,
  _Test2,
  None,
  Window,
  Spreadsheet,
  SpreadsheetInputField,
  Toolbar,
  ToolbarDropdown,
  DamageTracker,
}

pub enum CallbackResult {
  /// Event was dropped.
  Pass,

  /// No side effects.
  None,

  /// Callback failed.
  Error(widgets::Error),

  /// Push a widget to the ring.
  Push(Box<dyn RingElement>),

  /// Modify a widget by mark.
  Modify(
    Mark,
    Box<
      dyn FnOnce(
        Option<Wrap<dyn Drawable>>,
        &RwLockReadGuard<Description>,
        &Drone,
      ),
    >,
  ),

  /// Access damage tracker.
  Damage(Box<dyn FnOnce(&mut DamageTracker)>),
}

impl std::fmt::Debug for CallbackResult {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CallbackResult::Pass => write!(f, "Pass"),
      CallbackResult::None => write!(f, "None"),
      CallbackResult::Error(e) => write!(f, "Error({})", e),
      CallbackResult::Push(_) => write!(f, "Push(_)"),
      CallbackResult::Modify(m, _) => write!(f, "Modify({:?}, _)", m),
      CallbackResult::Damage(_) => write!(f, "Damage(_)"),
    }
  }
}

impl CallbackResult {
  /// Returns `true` if the callback result is [`Pass`].
  ///
  /// [`Pass`]: CallbackResult::Pass
  #[must_use]
  pub fn is_pass(&self) -> bool {
    matches!(self, Self::Pass)
  }
}

pub type Wrap<T> = Arc<RwLock<T>>;
pub type Unwrapped<'a, T> = RwLockWriteGuard<'a, T>;

pub fn wrap<T: Send + Sync>(x: T) -> Wrap<T> {
  Arc::new(RwLock::new(x))
}

pub trait RingElement {
  fn push_to_ring(&self, ring: &mut Ring);
}

pub type WidgetElement = (Wrap<dyn Drawable>, Mark, Mark, usize);
pub type Element<T> = (Wrap<T>, Mark);

pub struct Ring {
  widgets: Vec<WidgetElement>,
  parents: Vec<Element<dyn Parent>>,
  transient: Option<Element<dyn Transient>>,

  click_sinks: Vec<Element<dyn ClickSink>>,
  input_sinks: Vec<Element<dyn InputSink>>,
  key_sinks: Vec<Element<dyn KeySink>>,

  damage_tracker: Wrap<DamageTracker>,
}

impl Ring {
  pub fn new() -> Self {
    let damage_tracker = wrap(DamageTracker::new());
    Self {
      widgets: vec![],
      parents: vec![],
      transient: None,
      click_sinks: vec![],
      input_sinks: vec![],
      key_sinks: vec![(damage_tracker.clone(), Mark::DamageTracker)],
      damage_tracker,
    }
  }

  pub fn push(
    &mut self,
    w: Wrap<dyn Drawable>,
    m: Mark,
    parent: Mark,
    n: usize,
  ) {
    self.widgets.push((w, m, parent, n));
  }

  pub fn push_parent(&mut self, w: Wrap<dyn Parent>, m: Mark) {
    self.parents.push((w, m));
  }

  pub fn replace_transient(&mut self, w: Wrap<dyn Transient>, m: Mark) {
    if let Some((_, m)) = &self.transient {
      self.delete(*m);
    }
    self.transient = Some((w, m));
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

    self.widgets.retain(|(_, _m, _, _)| *_m != m);
    self.parents.retain(|(_, _m)| *_m != m);
    if let Some((_, _m)) = &self.transient {
      if *_m == m {
        self.transient = None;
      }
    }

    self.click_sinks.retain(|(_, _m)| *_m != m);
    self.input_sinks.retain(|(_, _m)| *_m != m);
    self.key_sinks.retain(|(_, _m)| *_m != m);

    len != self.widgets.len()
  }

  /// Draw all owned widgets, plotting if needed.
  pub fn draw(
    &mut self,
    desc: Arc<RwLock<Description>>,
    drone: &mut Drone,
  ) -> Vec<widgets::Error> {
    let parents = &self.parents;
    let p: Vec<widgets::Error> = self
      .widgets
      .iter()
      .zip(
        self
          .widgets
          .iter()
          .map(|w| drone.new_feed())
          .collect::<Vec<DroneFeed>>()
          .into_iter(),
      )
      .par_bridge()
      .map(|((w, _m, p, n), feed)| {
        let mut w = w.write().unwrap();
        if !w.plotted() {
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
    p: Point,
  ) {
    let mut r = CallbackResult::Pass;
    let mut m = Mark::None;

    for (w, _m) in self.click_sinks.iter().rev() {
      match w
        .write()
        .unwrap()
        .handle_click(desc, drone, p)
      {
        CallbackResult::Pass => continue,
        res => {
          r = res;
          m = *_m;
          break;
        }
      }
    }

    self.handle_callback_result_mut(desc, drone, r, "Click", m);
  }

  /// Act on callback result.
  ///
  /// # Alternatives
  ///
  /// See [Ring::handle_callback_result] for immutable version.
  ///
  /// # Return
  ///
  /// Returns `false` if `r` is [CallbackResult::Pass], `true otherwise`.
  /// `what` and `who` are used to describe callback in case of error:
  /// `what` describes the type of callback, `who` - marked object, sourcing
  /// the callback.
  fn handle_callback_result_mut(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    r: CallbackResult,
    what: &str,
    who: Mark,
  ) -> bool {
    if r.is_pass() {
      return false;
    }

    match r {
      CallbackResult::Error(e) => {
        println!("{} callback by {:?} failed: {}", what, who, e);
      }
      CallbackResult::Push(x) => {
        println!("push");
        x.push_to_ring(self)
      }
      CallbackResult::Modify(m, f) => f(self.pull(&m), desc, drone),
      CallbackResult::Damage(f) => f(&mut self.damage_tracker.write().unwrap()),

      _ => {}
    }
    return true;
  }

  /// Same as [Ring::handle_callback_result_mut], but [CallbackResult::Push]
  /// and [CallbackResult::Damage] are considered errors.
  fn handle_callback_result(
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    r: CallbackResult,
    what: &str,
    who: Mark,
  ) -> bool {
    if r.is_pass() {
      return false;
    }

    match r {
      CallbackResult::Error(e) => {
        println!("{} callback by {:?} failed: {}", what, who, e);
      }
      CallbackResult::Damage(_) => {
        println!("{} immutable callback by {:?} returned Damage", what, who);
      }
      CallbackResult::Push(_) => {
        println!("{} immutable callback by {:?} returned Push", what, who);
      }
      CallbackResult::Modify(m, f) => f(self.pull(&m), desc, drone),

      _ => {}
    }
    return true;
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
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: &WindowEvent,
  ) -> bool {
    if !self.transient.is_some() {
      return false;
    }
    let (trans, m) = self.transient.as_ref().unwrap();
    let m = m.clone();

    match e {
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        let r = trans.write().unwrap().handle_cancel(drone);
        self.delete(m);
        self.handle_callback_result_mut(desc, drone, r, "Cancel", m);
        true
      }
      WindowEvent::Key(Key::Enter | Key::KpEnter, _, Action::Press, mods)
        if !mods.contains(Modifiers::Control) =>
      {
        let r = trans.write().unwrap().handle_accept(drone);
        self.delete(m);
        self.handle_callback_result_mut(desc, drone, r, "Accept", m);
        true
      }
      _ => false,
    }
  }

  pub fn handle_key(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: &WindowEvent,
  ) -> bool {
    let mut r = CallbackResult::Pass;
    let mut m = Mark::None;

    for (w, _m) in self.key_sinks.iter().rev() {
      match w.write().unwrap().handle_key(drone, e) {
        CallbackResult::Pass => continue,
        res => {
          r = res;
          m = *_m;
          break;
        }
      }
    }

    self.handle_callback_result_mut(desc, drone, r, "Click", m)
  }

  pub fn drain_damage_tracker(
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
  ) {
    let mut t = self.damage_tracker.write().unwrap();
    t.drain().for_each(|r| {
      self.handle_callback_result(desc, drone, r, "Damage", Mark::DamageTracker);
    });
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

#[cfg(test)]
mod tests {
  use crate::{
    logic::ring,
    render::painter::Drone,
    widgets::{
      self,
      traits::{Drawable, Widget},
    },
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
      ring.push(
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
    fn plot(&mut self, _: Drone) -> Result<(), widgets::Error> {
      if self.fail_plot {
        Err(widgets::Error::Unspecified("plot failed".to_owned()))
      } else {
        Ok(())
      }
    }

    unsafe fn draw(&mut self, _: &Drone) -> Result<(), widgets::Error> {
      if self.fail_draw {
        Err(widgets::Error::Unspecified("draw failed".to_owned()))
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

    assert_eq!(r.delete(super::Mark::_Test1), true);
    assert_eq!(r.delete(super::Mark::_Test1), false);
  }

  #[test]
  fn error_handling() {
    let p = Drone::new(0, 0);
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
      widgets::Error::Unspecified("draw failed".to_owned())
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
      widgets::Error::Unspecified("plot failed".to_owned())
    );

    // plotting doesn't fail fast
    let fail_plot = W::new(true, false, false);
    fail_plot.push_to_ring(&mut r);
    let errors = r.draw(&p);
    assert_eq!(errors.len(), 2);
  }

  #[test]
  fn checks_for_plotting() {
    let p = Drone::new(0, 0);
    let mut r = Ring::new();
    let norm = W::new(false, false, false);
    let plotted = W::new(true, false, true);
    norm.push_to_ring(&mut r);
    plotted.push_to_ring(&mut r);
    assert_eq!(r.draw(&p).len(), 0);
  }
}
