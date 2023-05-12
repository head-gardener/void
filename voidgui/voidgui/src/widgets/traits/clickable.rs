use std::sync::RwLockReadGuard;

use crate::{
  logic::CallbackResult,
  render::{painter::{Drone, Description}, Area, Point},
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
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult;

  fn handle_click(
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult {
    self.click_area().map_or(CallbackResult::Pass, |a| {
      if p.contained(&a) {
        self.onclick(desc, drone, p)
      } else {
        CallbackResult::Pass
      }
    })
  }
}
