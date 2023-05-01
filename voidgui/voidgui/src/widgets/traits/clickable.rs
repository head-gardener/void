use crate::render::Area;

use super::Widget;

pub trait Clickable {
  fn click_area(&self) -> Option<Area>;
}

pub trait ClickableWidget: Widget + Clickable {}
