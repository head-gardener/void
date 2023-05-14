use std::sync::RwLockReadGuard;

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  logic::{
    ring::{self, Mark, RingElement},
    CallbackResult,
  },
  render::{
    painter::{Description, Drone, DroneFeed},
    Area, Origin, Point, TextTable,
  },
  widgets::InputField,
};

use super::traits::{ClickSink, Clickable, Drawable, Transient, Widget};
use crate::widgets;

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  records: Vec<Box<String>>,
}

impl Spreadsheet {
  pub unsafe fn new(
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
  ) -> Result<Self, widgets::Error> {
    let n = "Name".to_owned();
    let p = "Phone".to_owned();

    let mut table = TextTable::new(drone, 0, 2)?;
    table.add_row(
      desc,
      drone,
      [&n, &p].iter(),
      crate::render::text_table::CellStyle::Lit,
    )?;

    Ok(Self {
      table,
      records: vec![],
    })
  }

  pub fn push(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
    name: &str,
    phone: &str,
  ) -> Result<(), widgets::Error> {
    let n = name.to_owned();
    let p = phone.to_owned();

    unsafe {
      self.table.add_row(
        desc,
        drone,
        [&n, &p].iter(),
        crate::render::text_table::CellStyle::Normal,
      )?;
    };

    self.records.push(Box::new(n));
    self.records.push(Box::new(p));

    Ok(())
  }

  pub fn drop(&mut self) {
    self.records.clear();

    self.table.truncate(2);
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push(rc, Mark::Spreadsheet, Mark::Window, 0);
  }

  pub fn update_record(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    n: usize,
    s: &str,
  ) -> Result<(), widgets::Error> {
    let s = s.to_string();
    self.table.update_cell(desc, drone, n + 2, &s)?;
    *self.records[n] = s;
    Ok(())
  }
}

impl ClickSink for Spreadsheet {
  fn onclick(
    &self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 | 1 => CallbackResult::Pass,
      _ => {
        let s = &self.records[i - 2];
        match unsafe { InputField::new(desc, drone, s, (i - 2, s.clone())) } {
          Ok(f) => CallbackResult::Push(Box::new(ring::wrap(f))),
          Err(e) => CallbackResult::Error(e),
        }
      }
    }
  }
}

type SpreadsheetIF = (usize, Box<String>);

impl Transient for InputField<SpreadsheetIF> {
  fn handle_accept(&self, _: &Drone) -> CallbackResult {
    let (c, from) = self.closure().clone();
    let to = self.to_string();
    CallbackResult::Damage(Box::new(move |t| {
      t.push(crate::logic::Damage::Update(c, *from, to))
    }))
  }
}

impl RingElement for ring::Wrap<InputField<SpreadsheetIF>> {
  fn push_to_ring(&self, ring: &mut crate::logic::Ring) {
    ring.push_input_sink(self.clone(), Mark::SpreadsheetInputField);
    ring.replace_transient(self.clone(), Mark::SpreadsheetInputField);
    ring.push(self.clone(), Mark::SpreadsheetInputField, Mark::Window, 2);
  }
}
