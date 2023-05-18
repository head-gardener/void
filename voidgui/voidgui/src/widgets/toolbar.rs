use std::sync::{RwLockReadGuard, RwLockWriteGuard};

use crate::{
  logic::{
    ring::{self, Mark, RingElement},
    CallbackResult,
  },
  render::{
    painter::{Description, Drone, DroneFeed},
    text_table::Orientation,
    Area, Origin, Point, Size, TextTable,
  },
  widgets,
};

use super::{
  traits::{
    widget::Error, ClickSink, Clickable, Drawable, Parent, Transient, Widget,
  },
  InputField, Spreadsheet,
};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];
static TOOLBAR_TABLE_ITEMS: [&str; 2] = ["Pull", "Push"];
static TOOLBAR_TOOLS_ITEMS: [&str; 1] = ["Search..."];

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Toolbar {
  table: TextTable,
}

impl Toolbar {
  pub unsafe fn new(
    desc: &RwLockReadGuard<Description>,
    drone: &mut Drone,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        desc,
        drone,
        Orientation::Horizontal,
        crate::render::text_table::CellStyle::Normal,
        &TOOLBAR_ITEMS,
      )?,
    })
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_click_sink(rc.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push_parent(rc.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push_static(
      rc,
      crate::logic::ring::Mark::Toolbar,
      crate::logic::ring::Mark::Window,
      1,
    );
  }
}

impl ClickSink for Toolbar {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 => unsafe { ToolbarTable::new(desc, drone) }
        .map_or(CallbackResult::Error(Error::InitFailure), |m| {
          CallbackResult::Push(Box::new(ring::wrap(m)))
        }),
      1 => unsafe { ToolbarTools::new(desc, drone) }
        .map_or(CallbackResult::Error(Error::InitFailure), |m| {
          CallbackResult::Push(Box::new(ring::wrap(m)))
        }),
      _ => {
        panic!("unexpected ind: {}", i);
      }
    }
  }
}

impl Parent for Toolbar {
  fn nth_child(&self, n: usize) -> Option<Origin> {
    let a = self.table.cells()?.get(n)?;
    Some(Origin {
      x: a.x + a.width,
      y: a.y + a.height,
      pole: crate::render::OriginPole::TopRight,
    })
  }
}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct ToolbarTable {
  table: TextTable,
}

impl ToolbarTable {
  pub unsafe fn new(
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        desc,
        drone,
        Orientation::Vertical,
        crate::render::text_table::CellStyle::Normal,
        &TOOLBAR_TABLE_ITEMS,
      )?,
    })
  }
}

impl RingElement for ring::Wrap<ToolbarTable> {
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_transient(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
      true,
    );
    ring.push_click_sink(self.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push_static(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
      crate::logic::ring::Mark::Toolbar,
      0,
    );
  }
}

impl Transient for ToolbarTable {}

impl ClickSink for ToolbarTable {
  fn onclick(
    &mut self,
    _: &RwLockReadGuard<Description>,
    _: &Drone,
    p: Point,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 => CallbackResult::ExitCode(2),
      1 => CallbackResult::ExitCode(3),
      _ => {
        panic!("unexpected ind: {}", i);
      }
    }
  }
}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct ToolbarTools {
  table: TextTable,
}

impl ToolbarTools {
  pub unsafe fn new(
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        desc,
        drone,
        Orientation::Vertical,
        crate::render::text_table::CellStyle::Normal,
        &TOOLBAR_TOOLS_ITEMS,
      )?,
    })
  }
}

impl RingElement for ring::Wrap<ToolbarTools> {
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_transient(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
      true,
    );
    ring.push_click_sink(self.clone(), crate::logic::ring::Mark::Toolbar);
    ring.push_static(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
      crate::logic::ring::Mark::Toolbar,
      1,
    );
  }
}

impl Transient for ToolbarTools {}

impl ClickSink for ToolbarTools {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 => match unsafe { InputField::new(desc, drone, "", ()) } {
        Ok(f) => CallbackResult::Push(Box::new(ring::wrap(f))),
        Err(e) => CallbackResult::Error(e),
      },
      _ => {
        panic!("unexpected ind: {}", i);
      }
    }
  }
}

type SearchIF = ();

impl Transient for InputField<SearchIF> {
  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    let st = self.to_string();
    CallbackResult::Modify(
      Mark::Spreadsheet,
      ring::with_spreadsheet(
        move |_: &RwLockReadGuard<Description>,
              drone: &Drone,
              s: &mut Spreadsheet| {
          s.new_search(drone, &st);
        },
      ),
    )
  }
}

impl RingElement for ring::Wrap<InputField<SearchIF>> {
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_transient(self.clone(), Mark::InputFloat, true);
    ring.push_input_sink(self.clone(), Mark::InputFloat);
    ring.push_static(self.clone(), Mark::InputFloat, Mark::Window, 2);
  }
}
