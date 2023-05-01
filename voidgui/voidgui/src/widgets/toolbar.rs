use crate::render::{painter::Painter, Point, TextTable};
use std::{cell::RefCell, rc::Rc};

use super::{
  menu::Orientation,
  traits::{widget::WidgetError, Clickable, ClickableWidget, Widget},
};

use voidmacro::Menu;

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];

#[derive(Menu)]
pub struct Toolbar {
  table: TextTable,
}

impl Toolbar {
  pub unsafe fn new(painter: &Painter) -> Result<Self, WidgetError> {
    Toolbar::make_table(
      painter,
      Orientation::Vertical,
      &TOOLBAR_ITEMS,
    )
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = Rc::new(RefCell::new(self));
    ring.push_clickable(rc.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push(rc, crate::logic::ring::Mark::Toolbar);
  }
}
