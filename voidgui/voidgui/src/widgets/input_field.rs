use crate::render::{
  painter::Painter, text_table::Orientation, Origin, TextTable,
};
use std::{cell::RefCell, rc::Rc};

use super::traits::{widget::WidgetError, Widget};

use voidmacro::Menu;

#[derive(Menu)]
pub struct InputField {
  table: TextTable,
}

impl InputField {
  pub unsafe fn new(painter: &Painter, s: String) -> Result<Self, WidgetError> {
    Ok(Self {
      table: TextTable::make_static(painter, Orientation::Vertical, &[&s])?,
    })
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = Rc::new(RefCell::new(self));
    ring.push(
      rc,
      crate::logic::ring::Mark::Toolbar,
      crate::logic::ring::Mark::Window,
      2,
    );
  }
}
