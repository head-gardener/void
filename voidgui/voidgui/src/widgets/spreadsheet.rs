use std::{
  str::FromStr,
  sync::{RwLockReadGuard, RwLockWriteGuard},
};

use glfw::{Action, Key, Modifiers, WindowEvent};
use serde::Serialize;
use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  logic::{
    ring::{self, Mark, RingElement},
    CallbackResult, Damage, Dataset,
  },
  render::{
    painter::{Description, Drone, DroneFeed, Mode},
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
pub trait Record:
  Serialize + Header + Send + Sync + From<(i64, Vec<String>)>
{
  const N_FIELDS: usize;

  fn fields<'a>(&'a self) -> Vec<&'a dyn Data>;
  fn datatypes() -> Vec<Datatype>;
  fn uid(&self) -> &i64;
}

pub trait Header {
  fn header() -> Vec<&'static str>;
}

pub enum Datatype {
  String,
  Integer,
}

static DEF_STR: String = String::new();
static DEF_INT: i64 = 0;

impl Datatype {
  fn default(&self) -> &'static dyn Data {
    match self {
      Datatype::String => &DEF_STR,
      Datatype::Integer => &DEF_INT,
    }
  }
}

pub trait Data: ToString {}
impl Data for String {}
impl Data for i64 {}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  data: Dataset,
  datatypes: Vec<Datatype>,
  uids: Vec<i64>,
  scroll: i32,
}

impl Spreadsheet {
  pub unsafe fn new<'a, E: Record>(
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
      uids: vec![],
      scroll: 0,
    })
  }

  pub fn push<'a, E: Record>(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: E,
  ) -> Result<(), widgets::Error> {
    // PERF: too much allocating

    self.uids.push(*e.uid());
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

  pub fn add_record(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
  ) -> Result<(), widgets::Error> {
    let fs: Vec<String> = self
      .datatypes
      .iter()
      .map(|f| f.default().to_string())
      .collect();

    self.uids.push(uid);
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

  pub fn rem_record(
    &mut self,
    drone: &Drone,
    uid: i64,
  ) -> Result<(), widgets::Error> {
    let pos = self.uids.iter().position(|u| *u == uid).ok_or(
      widgets::Error::Unspecified(format!("invalid uid passed to rem: {uid}")),
    )?;

    // TODO: memory
    self.uids.remove(pos);
    self
      .data
      .cut(pos * self.table.columns(), self.table.columns());
    self.table.rem_row(drone, pos + 1);

    Ok(())
  }

  pub fn update_record(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    n: usize,
    s: &str,
  ) -> Result<(), widgets::Error> {
    let n = n
      + self.uids.iter().position(|i| *i == uid).ok_or(
        widgets::Error::Unspecified(format!(
          "uid {} not found in spreadsheet",
          uid,
        )),
      )? * self.table.columns();
    self
      .table
      .update_cell(desc, drone, n + self.table.columns(), s)?;
    self.data.set(n, s);
    Ok(())
  }

  pub fn get_record<R: Record>(&self, uid: i64) -> Option<R> {
    let pos = self.uids.iter().position(|u| *u == uid)?;
    let ss = self
      .data
      .range(pos * self.table.columns(), self.table.columns())
      .iter()
      .map(|s| s.to_string())
      .collect();
    Some(R::from((uid, ss)))
  }

  pub fn drop(&mut self) {
    self.data.clear();
    self.uids.clear();

    self.table.truncate(self.table.columns());
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_key_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_static(rc, Mark::Spreadsheet, Mark::Window, 0);
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

  pub fn scale(&mut self, scale: f32) {
    self.table.scale(scale)
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
      _ => match desc.mode() {
        Mode::Normal => {
          let s = self.data.get(i - self.table.columns());
          self.table.set_highlight(drone, i / self.table.columns());
          let closure = (
            self.uids[i / self.table.columns() - 1],
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
        Mode::Delete => {
          let uid = self.uids[i / self.table.columns() - 1];
          CallbackResult::Damage(Box::new(move |t| {
            t.push(Damage::Remove(uid));
          }))
        }
      },
    }
  }
}

impl KeySink for Spreadsheet {
  fn handle_key(
    &mut self,
    desc: &Description,
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
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => match desc.mode() {
        Mode::Delete => CallbackResult::Mode(Mode::Normal),
        _ => {
          if self.clear_search() {
            CallbackResult::None
          } else {
            CallbackResult::Pass
          }
        }
      },
      _ => CallbackResult::Pass,
    }
  }
}

type SpreadsheetIF = (i64, usize, String);

impl<T> Transient for InputField<SpreadsheetIF, T>
where
  T: Send + Sync + std::fmt::Display + Default + FromStr + Clone,
  <T>::Err: std::fmt::Debug,
{
  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    let (uid, c, from) = self.closure().clone();
    let to = self.to_string();
    CallbackResult::Damage(Box::new(move |t| {
      t.push(crate::logic::Damage::Update(uid, c, from, to))
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
