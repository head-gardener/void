use crate::render::{Area, Point, painter::Painter};

use super::CallbackResult;

pub trait Clickable {
  fn click_area(&self) -> Option<Area>;
}

pub trait ClickSink: Clickable {
  fn onclick(&self, painter: &Painter, p: Point) -> CallbackResult;

  fn handle_click(&self, painter: &Painter, p: Point) -> CallbackResult {
    self.click_area().map_or(CallbackResult::Skip, |a| {
      if p.contained(&a) {
        self.onclick(painter, p)
      } else {
        CallbackResult::Skip
      }
    })
  }
}
