use std::sync::{RwLockReadGuard, RwLockWriteGuard};

use crate::{
  logic::ring::{self, Mark, RingElement},
  render::{
    painter::{Drone, DroneFeed},
    text_table::{CellStyle, Orientation},
    Area, Origin, Point, Size, TextTable,
  },
  widgets, Description,
};

use super::traits::{
  widget::Error, ClickSink, Clickable, Drawable, Transient, Widget,
};
use voidmacro::{ClickableMenu, DrawableMenu, Menu};

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Picker<C> {
  table: TextTable,
  mark: Mark,
  parent: Mark,
  child_n: usize,
  closure: C,
}

impl<C> Picker<C> {
  pub unsafe fn new(
    desc: &Description,
    drone: &Drone,
    items: &[&str],
    mark: Mark,
    parent: Mark,
    child_n: usize,
    style: CellStyle,
    closure: C,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        desc,
        drone,
        Orientation::Vertical,
        style,
        items,
      )?,
      mark,
      parent,
      child_n,
      closure,
    })
  }

  pub fn catch_point(&self, p: &Point) -> Option<usize> {
    self.table.catch_point(p)
  }

  pub fn closure(&self) -> &C {
    &self.closure
  }
}

impl<C> RingElement for ring::Wrap<Picker<C>>
where
  C: Send + Sync + 'static,
  Picker<C>: ClickSink,
{
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    let s = self.read().unwrap();
    ring.push_transient(self.clone(), s.mark, true);
    ring.push_click_sink(self.clone(), s.mark);
    ring.push_static(self.clone(), s.mark, s.parent, s.child_n);
  }
}

impl<C: Send + Sync> Transient for Picker<C> {}
