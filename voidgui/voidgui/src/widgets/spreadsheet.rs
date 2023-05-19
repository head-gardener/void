use std::{
  str::FromStr,
  sync::{RwLockReadGuard, RwLockWriteGuard},
};

use glfw::{Action, Key, Modifiers, WindowEvent};
use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  logic::{
    ring::{self, Mark, RingElement},
    CallbackResult, Dataset,
  },
  render::{
    painter::{Description, Drone, DroneFeed},
    Area, Origin, Point, Size, TextTable,
  },
  widgets::InputField,
};

use super::traits::{
  ClickSink, Clickable, Drawable, KeySink, Transient, Widget,
};
use crate::widgets;

/// An entry in a spreadsheet. Implement with `Entry` derive macro.
/// When doing so, prefix all data field with `d_` and give them `&'a str` type.
/// Those fields will be used when displaying data.
pub trait Entry: Header + Send + Sync {
  const N_FIELDS: usize;

  fn fields<'a>(&'a self) -> Vec<&'a dyn Data>;
  fn datatypes() -> Vec<Datatype>;
  fn uuid(&self) -> &u64;
}

pub trait Header {
  fn header() -> Vec<&'static str>;
}

pub enum Datatype {
  String,
  Integer,
}

pub trait Data: ToString {}
impl Data for String {}
impl Data for u64 {}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  data: Dataset,
  datatypes: Vec<Datatype>,
  uuids: Vec<u64>,
  scroll: i32,
}

impl Spreadsheet {
  pub unsafe fn new<'a, E: Entry>(
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
  ) -> Result<Self, widgets::Error> {
    let mut table = TextTable::new(drone, 0, E::N_FIELDS)?;
    table.add_row(
      desc,
      drone,
      E::header().into_iter(),
      crate::render::text_table::CellStyle::Lit,
    )?;

    Ok(Self {
      table,
      data: Dataset::new(),
      datatypes: E::datatypes(),
      uuids: vec![],
      scroll: 0,
    })
  }

  pub fn push<'a, E: Entry>(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
    e: E,
  ) -> Result<(), widgets::Error> {
    // PERF: too much allocating

    self.uuids.push(*e.uuid());
    let fs: Vec<String> = e.fields().iter().map(|f| f.to_string()).collect();
    fs.iter().for_each(|f| {
      self.data.push(&f);
    });

    unsafe {
      self.table.add_row(
        desc,
        drone,
        fs.iter().map(|f| f.as_str()),
        crate::render::text_table::CellStyle::Normal,
      )?;
    };

    Ok(())
  }

  pub fn drop(&mut self) {
    self.data.clear();
    self.uuids.clear();

    self.table.truncate(self.table.columns());
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_key_sink(rc.clone(), Mark::Spreadsheet);
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
    let n = n
      + self.uuids.iter().position(|i| *i == uuid).ok_or(
        widgets::Error::Unspecified(format!(
          "UUID {} not found in spreadsheet",
          uuid,
        )),
      )? * self.table.columns();
    self
      .table
      .update_cell(desc, drone, n + self.table.columns(), s)?;
    self.data.set(n, s);
    Ok(())
  }

  pub fn scroll(&mut self, offset: i32) {
    self.scroll += offset;
    self.table.request_plot();
  }

  pub fn new_search(&mut self, drone: &Drone, value: &str) {
    self.data.new_search(value);
    self.find(drone, true);
  }

  fn find(&mut self, drone: &Drone, next: bool) {
    if next {
      self.data.find_next()
    } else {
      self.data.find_prev()
    }
    .map(|i| {
      self
        .table
        .set_highlight(drone, i / self.table.columns() + 1);
      self.scroll = self
        .table
        .cell_sizes()
        .chunks(self.table.columns())
        .map(|xs| xs[0].height)
        .take(i / self.table.columns() + 1)
        .sum::<i32>()
        * -1;
      self.request_plot();
    });
  }

  fn clear_search(&mut self) -> bool {
    self.request_plot();
    self.data.clear_search()
  }
}

impl ClickSink for Spreadsheet {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 | 1 => CallbackResult::Pass,
      _ => {
        let s = self.data.get(i - self.table.columns());
        self.table.set_highlight(drone, i / self.table.columns());
        let closure = (
          self.uuids[i / self.table.columns() - 1],
          i % self.table.columns(),
          s.to_string(),
        );
        match self.datatypes[i % self.table.columns()] {
          Datatype::String => unsafe {
            InputField::<SpreadsheetIF, String>::new(desc, drone, s, closure)
              .map(|f| Box::new(ring::wrap(f)))
              .map_or_else(
                |e| CallbackResult::Error(e),
                |f| CallbackResult::Push(f),
              )
          },
          Datatype::Integer => unsafe {
            InputField::<SpreadsheetIF, i64>::new(desc, drone, s, closure)
              .map(|f| Box::new(ring::wrap(f)))
              .map_or_else(
                |e| CallbackResult::Error(e),
                |f| CallbackResult::Push(f),
              )
          },
        }
      }
    }
  }
}

impl KeySink for Spreadsheet {
  fn handle_key(
    &mut self,
    drone: &Drone,
    e: &glfw::WindowEvent,
  ) -> CallbackResult {
    match e {
      WindowEvent::Key(Key::N, _, Action::Press, m) if m.is_empty() => {
        self.find(drone, true);
        CallbackResult::None
      }
      WindowEvent::Key(Key::N, _, Action::Press, Modifiers::Shift) => {
        self.find(drone, false);
        CallbackResult::None
      }
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        if self.clear_search() {
          CallbackResult::None
        } else {
          CallbackResult::Pass
        }
      }
      _ => CallbackResult::Pass,
    }
  }
}

type SpreadsheetIF = (u64, usize, String);

impl<T> Transient for InputField<SpreadsheetIF, T>
where
  T: Send + Sync + std::fmt::Display + Default + FromStr + Clone,
  <T>::Err: std::fmt::Debug,
{
  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    let (uuid, c, from) = self.closure().clone();
    let to = self.to_string();
    CallbackResult::Damage(Box::new(move |t| {
      t.push(crate::logic::Damage::Update(uuid, c, from, to))
    }))
  }
}

impl<T> RingElement for ring::Wrap<InputField<SpreadsheetIF, T>>
where
  T: Send + Sync + std::fmt::Display + Default + FromStr + Clone + 'static,
  <T>::Err: std::fmt::Debug,
{
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_transient(self.clone(), Mark::InputFloat, true);
    ring.push_input_sink(self.clone(), Mark::InputFloat);
    ring.push_static(self.clone(), Mark::InputFloat, Mark::Window, 2);
  }
}
