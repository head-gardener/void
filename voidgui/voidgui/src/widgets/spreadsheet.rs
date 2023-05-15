use std::sync::{RwLockReadGuard, RwLockWriteGuard};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  logic::{
    ring::{self, Mark, RingElement},
    CallbackResult,
  },
  render::{
    painter::{Description, Drone, DroneFeed},
    Area, Origin, Point, Size, TextTable,
  },
  widgets::InputField,
};

use super::traits::{ClickSink, Clickable, Drawable, Transient, Widget};
use crate::widgets;

/// An entry in a spreadsheet. Implement with `Entry` derive macro.
/// When doing so, prefix all data field with `d_` and give them `&'a str` type.
/// Those fields will be used when displaying data.
pub trait Entry<'a>: Default + Send + Sync {
  const N_FIELDS: usize;

  fn fields(self) -> Vec<&'a str>;
  fn uuid(&self) -> &u64;
  // fn ntn_mut(&mut self) -> &mut String;
}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  records: Vec<Box<String>>,
  uuids: Vec<u64>,
}

impl Spreadsheet {
  pub unsafe fn new<'a, E: Entry<'a>>(
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
  ) -> Result<Self, widgets::Error> {
    let mut table = TextTable::new(drone, 0, E::N_FIELDS)?;
    table.add_row(
      desc,
      drone,
      E::default().fields().into_iter(),
      crate::render::text_table::CellStyle::Lit,
    )?;

    Ok(Self {
      table,
      records: vec![],
      uuids: vec![],
    })
  }

  pub fn push<'a, E: Entry<'a>>(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
    e: E,
  ) -> Result<(), widgets::Error> {
    self.uuids.push(*e.uuid());
    let fs = e.fields();
    fs.iter().for_each(|f| {
      self.records.push(Box::new(f.to_string()));
    });

    unsafe {
      self.table.add_row(
        desc,
        drone,
        fs.into_iter(),
        crate::render::text_table::CellStyle::Normal,
      )?;
    };

    Ok(())
  }

  pub fn drop(&mut self) {
    self.records.clear();
    self.uuids.clear();

    self.table.truncate(self.table.columns());
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_static(rc, Mark::Spreadsheet, Mark::Window, 0);
  }

  pub fn update_record(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    uuid: u64,
    n: usize,
    s: &str,
  ) -> Result<(), widgets::Error> {
    let s = s.to_string();
    let n = n
      + self.uuids.iter().position(|i| *i == uuid).ok_or(
        widgets::Error::Unspecified(format!(
          "UUID {} not found in spreadsheet",
          uuid,
        )),
      )? * self.table.columns();
    self
      .table
      .update_cell(desc, drone, n + self.table.columns(), &s)?;
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
        print!("{} in ", self.uuids[i / self.table.columns() - 1]);
        println!("self.uuids = {:?}", self.uuids);
        let s = &self.records[i - self.table.columns()];
        match unsafe {
          InputField::new(
            desc,
            drone,
            s,
            (
              self.uuids[i / self.table.columns() - 1],
              i % self.table.columns(),
              s.clone(),
            ),
          )
        } {
          Ok(f) => CallbackResult::Push(Box::new(ring::wrap(f))),
          Err(e) => CallbackResult::Error(e),
        }
      }
    }
  }
}

type SpreadsheetIF = (u64, usize, Box<String>);

impl Transient for InputField<SpreadsheetIF> {
  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    let (uuid, c, from) = self.closure().clone();
    let to = self.to_string();
    CallbackResult::Damage(Box::new(move |t| {
      t.push(crate::logic::Damage::Update(uuid, c, *from, to))
    }))
  }
}

impl RingElement for ring::Wrap<InputField<SpreadsheetIF>> {
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_input_sink(self.clone(), Mark::SpreadsheetInputField);
    ring.push_transient(self.clone(), Mark::SpreadsheetInputField);
    ring.push_static(self.clone(), Mark::SpreadsheetInputField, Mark::Window, 2);
  }
}
