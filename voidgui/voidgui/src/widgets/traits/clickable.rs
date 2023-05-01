use crate::render::{Area, Point};

use super::Widget;

pub trait Clickable {
  fn click_area(&self) -> Option<Area>;
}

pub trait ClickableWidget: Widget + Clickable {
  fn onclick(&self, p: Point);

  fn handle_click(&self, p: Point) -> bool {
    self.click_area().map_or(false, |a| {
      if p.contained(&a) {
        self.onclick(p);
        return true;
      }

      return false;
    })
  }
}
