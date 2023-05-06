use crate::render::{Area, Point, painter::Painter};

use super::CallbackResult;

/// `Clickable` is an object, that has an area catching clicks.
pub trait Clickable {
  fn click_area(&self) -> Option<Area>;
}

/// `Click sink` is an object, that can catch and handle clicks.
///
/// # Properties
///
/// If a click sink rejects a click, it's passed to the previous (earlier) sink.
pub trait ClickSink: Clickable {
  fn onclick(&self, painter: &Painter, p: Point) -> CallbackResult;

  fn handle_click(&self, painter: &Painter, p: Point) -> CallbackResult {
    self.click_area().map_or(CallbackResult::Pass, |a| {
      if p.contained(&a) {
        self.onclick(painter, p)
      } else {
        CallbackResult::Pass
      }
    })
  }
}
