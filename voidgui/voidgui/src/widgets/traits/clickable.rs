use std::sync::RwLockReadGuard;

use glfw::Modifiers;

use crate::{
  logic::CallbackResult,
  render::{painter::Drone, Area, Point},
  Description,
};

/// `Clickable` is an object, that has an area catching clicks.
pub trait Clickable {
  fn click_area(&self) -> Option<Area>;
}

/// `Click sink` is an object, that can catch and handle clicks.
///
/// # Properties
///
/// If a click sink rejects a click, it's passed to the previous (earlier) sink.
pub trait ClickSink: Send + Sync + Clickable {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
    mods: &Modifiers,
  ) -> CallbackResult;

  fn handle_click(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
    mods: &Modifiers,
  ) -> CallbackResult {
    self.click_area().map_or(CallbackResult::Pass, |a| {
      if p.contained(&a) {
        self.onclick(desc, drone, p, mods)
      } else {
        CallbackResult::Pass
      }
    })
  }
}
