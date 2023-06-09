use std::{
  str::FromStr,
  sync::{RwLockReadGuard, RwLockWriteGuard},
};

use glfw::{Action, Key, WindowEvent};
use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  data::{Data, Datatype, FileSub, GenericFile, Record, Tag},
  description::*,
  logic::{
    ring::{self, Mark, RingElement, Wrap},
    CallbackResult, Damage,
  },
  render::{
    painter::{Drone, DroneFeed},
    Area, Origin, Point, Size, TextTable,
  },
  widgets::InputField,
};

use super::{
  picker::Picker,
  traits::{
    ClickSink, Clickable, Drawable, KeySink, Parent, Transient, Widget,
  },
  Error, Status,
};
use crate::widgets;

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  target: Tag,
  uids: Vec<i64>,
  scroll: i32,
}

impl Spreadsheet {
  pub unsafe fn new<'a, R: Record>(
    desc: &Description,
    drone: &mut Drone,
    target: Tag,
  ) -> Result<Self, widgets::Error> {
    let header = R::header();
    assert_eq!(header.len(), R::N_FIELDS);
    let mut table = TextTable::new(drone, 0, R::N_FIELDS)?;
    table.add_row(
      desc,
      drone,
      R::header().into_iter(),
      crate::render::text_table::CellStyle::Lit,
    )?;

    Ok(Self {
      table,
      uids: vec![],
      scroll: 0,
      target,
    })
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) -> Wrap<Self> {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_key_sink(rc.clone(), Mark::Spreadsheet);
    ring.push_parent(rc.clone(), Mark::Spreadsheet);
    ring.push_static(rc.clone(), Mark::Spreadsheet, Mark::Window, 0);
    rc
  }

  pub fn scroll(&mut self, offset: i32) {
    self.scroll += offset;
    self.table.request_plot();
  }

  pub fn scale(&mut self, scale: f32) {
    self.table.scale(scale)
  }

  fn on_invalid_target(&self) {
    panic!("unexpected tgt {:?}", self.target);
  }

  pub fn set_target(&mut self, target: Tag) {
    self.target = target;
  }
}

impl FileSub for Spreadsheet {
  fn on_drop(&mut self, _: &Description, _: &Drone) {
    self.uids.clear();

    self.table.truncate(self.table.columns());
  }

  fn on_put(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    row: &[&str],
  ) {
    self.uids.push(uid);

    unsafe {
      self
        .table
        .add_row(
          desc,
          drone,
          row.into_iter().map(|s| *s),
          crate::render::text_table::CellStyle::Normal,
        )
        .unwrap();
    };
  }

  fn on_set(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    field: usize,
    s: &str,
  ) {
    let pos = self.uids.iter().position(|i| *i == uid);
    if pos.is_none() {
      return;
    }
    let pos = pos.unwrap();

    let n = field + pos * self.table.columns();
    self
      .table
      .update_cell(desc, drone, n + self.table.columns(), s)
      .unwrap();
  }

  fn on_rem(&mut self, _: &Description, drone: &Drone, uid: i64) {
    let pos = self.uids.iter().position(|u| *u == uid);
    if pos.is_none() {
      return;
    }
    let pos = pos.unwrap();

    self.uids.remove(pos);
    self.table.rem_row(drone, pos + 1);
  }

  fn on_drop_search(&mut self, _: &Description, _: &Drone) {
    // drop highlight
    self.table.request_plot();
  }

  fn on_search_update(
    &mut self,
    _: &Description,
    drone: &Drone,
    res: &Option<(i64, usize)>,
  ) {
    res
      .and_then(|(uid, _)| self.uids.iter().position(|i| *i == uid))
      .map(|pos| {
        self.table.set_highlight(drone, pos + 1);
        self.scroll = self
          .table
          .cell_sizes()
          .chunks(self.table.columns())
          .map(|xs| xs[0].height)
          .take(pos + 1)
          .sum::<i32>()
          * -1;
        self.request_plot();
      });
  }
}

impl ClickSink for Spreadsheet {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    _: &Drone,
    p: Point,
    mods: &glfw::Modifiers,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    let mods = mods.clone();

    match i {
      n if n < self.table.columns() => CallbackResult::Pass,
      _ => match desc.mode() {
        Mode::Normal => CallbackResult::Read(
          self.target,
          Box::new(
            move |w: Option<Wrap<dyn Drawable>>,
                  f: Option<Wrap<dyn GenericFile>>,
                  desc: &Description,
                  drone: &Drone| {
              on_click(w, f, desc, drone, i, mods)
            },
          ),
        ),
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

impl Parent for Spreadsheet {
  fn nth_child(&self, n: usize) -> Option<Origin> {
    let a = self.table.cells()?.get(n)?;
    Some(Origin {
      x: a.x,
      y: a.y + a.height,
      pole: crate::render::OriginPole::TopLeft,
    })
  }
}

// what follows is comically complex and i'm sorry for whoever is going to
// be digging through that.
fn on_click(
  w: Option<Wrap<dyn Drawable>>,
  f: Option<Wrap<dyn GenericFile>>,
  desc: &Description,
  drone: &Drone,
  i: usize,
  mods: glfw::Modifiers,
) -> CallbackResult {
  if w.is_none() {
    return "widget gone after read".into();
  }
  let w = w.unwrap();
  let w = w.try_write().ok();
  if w.is_none() {
    return "can't lock widget".into();
  }
  let mut w = w.unwrap();
  let ss: &mut Spreadsheet = w.downcast_mut().unwrap();
  let column = i % ss.table.columns();
  let row = i / ss.table.columns() - 1;

  if f.is_none() {
    ss.on_invalid_target();
  }
  let f = f.unwrap();
  let f = f.read().unwrap();
  let f = f.read();

  let uid = ss.uids[row];
  let sub = f.get(uid, column);
  if sub.is_none() {
    return "couldn't get str".into();
  }
  let sub = sub.unwrap();
  ss.table.set_highlight(drone, row + 1);
  let closure = (uid, column, sub.to_string());
  let edit_target = ss.target;

  match f.datatypes()[i % ss.table.columns()] {
    Datatype::String => unsafe {
      InputField::<SpreadsheetIF, String>::new(desc, drone, sub, closure)
        .map(|f| Box::new(ring::wrap(f)))
        .map_or_else(|e| CallbackResult::Error(e), |f| CallbackResult::Push(f))
    },
    Datatype::Integer => unsafe {
      InputField::<SpreadsheetIF, i64>::new(desc, drone, sub, closure)
        .map(|f| Box::new(ring::wrap(f)))
        .map_or_else(|e| CallbackResult::Error(e), |f| CallbackResult::Push(f))
    },
    Datatype::FKey(_, _) => unsafe {
      let fkey = f.raw(uid, column).unwrap();
      let fkey = match fkey {
        crate::data::Data::FKey(None) => return CallbackResult::None,
        crate::data::Data::FKey(Some(k)) => k,
        _ => panic!(),
      };
      let tgt = match f.datatypes().iter().nth(column).unwrap() {
        Datatype::FKey(tgt, tgt_f) => (*tgt, *tgt_f),
        _ => panic!(),
      };

      CallbackResult::Read(
        tgt.0,
        Box::new(
          move |_: Option<Wrap<dyn Drawable>>,
                f: Option<Wrap<dyn GenericFile>>,
                desc: &Description,
                drone: &Drone| {
            if f.is_none() {
              return CallbackResult::None;
            }
            let f = f.unwrap();
            let f = f.read().unwrap();
            let f = f.read();

            if mods != glfw::Modifiers::Control {
              let (uids, data): (Vec<i64>, Vec<&str>) =
                f.column(tgt.1).unwrap().into_iter().unzip();
              return FKeyPicker::new(
                desc,
                drone,
                &data.as_slice(),
                Mark::InputFloat,
                Mark::Spreadsheet,
                i,
                crate::render::text_table::CellStyle::Lighter,
                (edit_target, uid, column, uids),
              )
              .map_or(CallbackResult::Error(Error::InitFailure), |m| {
                CallbackResult::Push(Box::new(ring::wrap(m)))
              });
            }

            let msg = f.row(fkey).unwrap().join(", ");

            Status::create(msg, desc, drone)
          },
        ),
      )
    },
  }
}

impl KeySink for Spreadsheet {
  fn handle_key(
    &mut self,
    desc: &Description,
    _: &Drone,
    e: &glfw::WindowEvent,
  ) -> CallbackResult {
    match e {
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => match desc.mode() {
        Mode::Delete => CallbackResult::Mode(Mode::Normal),
        _ => CallbackResult::Pass,
      },
      _ => CallbackResult::Pass,
    }
  }
}

// Picker

type FKeyPicker = Picker<(Tag, i64, usize, Vec<i64>)>;

impl ClickSink for FKeyPicker {
  fn onclick(
    &mut self,
    _: &RwLockReadGuard<Description>,
    _: &Drone,
    p: Point,
    _: &glfw::Modifiers,
  ) -> CallbackResult {
    let i = self.catch_point(&p).unwrap();
    let closure = self.closure().clone();
    CallbackResult::File(
      closure.0,
      Box::new(move |f, desc, drone| {
        let mut f = f.write().unwrap();
        f.set_raw(
          desc,
          drone,
          closure.1,
          closure.2,
          Data::FKey(Some(closure.3[i])),
        );
      }),
    )
    .join(CallbackResult::Read(
      closure.0,
      Box::new(
        move |_: Option<Wrap<dyn Drawable>>,
              f: Option<Wrap<dyn GenericFile>>,
              _: &Description,
              _: &Drone| {
          f.unwrap().read().unwrap().join(closure.0, closure.1)
        },
      ),
    ))
    .join(CallbackResult::Die)
  }
}

// InputField

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
