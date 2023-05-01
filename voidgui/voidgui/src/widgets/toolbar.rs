use crate::render::{
  painter::Painter, text_table::Orientation, Point, TextTable,
};
use std::{cell::RefCell, rc::Rc};

use super::traits::{widget::WidgetError, Clickable, ClickableWidget, Widget};

use voidmacro::Menu;

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];

#[derive(Menu)]
pub struct Toolbar {
  table: TextTable,
}

impl Toolbar {
  pub unsafe fn new(painter: &Painter) -> Result<Self, WidgetError> {
    Ok(Self {
      table: TextTable::make_static(
        painter,
        Orientation::Vertical,
        &TOOLBAR_ITEMS,
      )?,
    })
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = Rc::new(RefCell::new(self));
    ring.push_clickable(rc.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push(rc, crate::logic::ring::Mark::Toolbar);
  }
}

impl ClickableWidget for Toolbar {
  fn onclick(&self, p: Point) {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 => println!("Table"),
      1 => println!("Tools"),
      _ => { panic!("unexpected ind: {}", i) }
    }
  }
}
