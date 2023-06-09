use std::sync::{RwLockReadGuard, RwLockWriteGuard};

use crate::{
  data::GenericFile,
  logic::{
    ring::{self, Mark, RingElement, Wrap},
    CallbackResult, Damage,
  },
  render::{
    painter::{Drone, DroneFeed},
    text_table::Orientation,
    Area, Origin, Point, Size, TextTable,
  },
  widgets, Description,
};

use super::{
  picker::Picker,
  traits::{
    widget::Error, ClickSink, Clickable, Drawable, Parent, Transient, Widget,
  },
  InputField,
};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];
static TOOLBAR_TABLE_ITEMS: [&str; 2] = ["Pull", "Push"];
static TOOLBAR_TOOLS_ITEMS: [&str; 3] = ["New", "Remove", "Search..."];

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
    _: &glfw::Modifiers,
  ) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 => unsafe {
        TableDropdown::new(
          desc,
          drone,
          &TOOLBAR_TABLE_ITEMS,
          Mark::ToolbarDropdown,
          Mark::Toolbar,
          0,
          crate::render::text_table::CellStyle::Normal,
          (),
        )
      }
      .map_or(CallbackResult::Error(Error::InitFailure), |m| {
        CallbackResult::Push(Box::new(ring::wrap(m)))
      }),
      1 => unsafe {
        ToolsDropdown::new(
          desc,
          drone,
          &TOOLBAR_TOOLS_ITEMS,
          Mark::ToolbarDropdown,
          Mark::Toolbar,
          1,
          crate::render::text_table::CellStyle::Normal,
          0,
        )
      }
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

// Dropdowns

type TableDropdown = Picker<()>;

impl ClickSink for TableDropdown {
  fn onclick(
    &mut self,
    _: &RwLockReadGuard<Description>,
    _: &Drone,
    p: Point,
    _: &glfw::Modifiers,
  ) -> CallbackResult {
    let i = self.catch_point(&p).unwrap();
    match i {
      0 => CallbackResult::ExitCode(2),
      1 => CallbackResult::ExitCode(3),
      _ => {
        panic!("unexpected ind: {}", i);
      }
    }
  }
}

type ToolsDropdown = Picker<u8>;

impl ClickSink for ToolsDropdown {
  fn onclick(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    p: Point,
    _: &glfw::Modifiers,
  ) -> CallbackResult {
    let i = self.catch_point(&p).unwrap();
    match i {
      0 => CallbackResult::Damage(Box::new(|t| {
        t.push(Damage::Add(rand::random()))
      })),
      1 => CallbackResult::Mode(crate::description::Mode::Delete),
      2 => match unsafe { InputField::new(desc, drone, "", ()) } {
        Ok(f) => CallbackResult::Push(Box::new(ring::wrap(f))),
        Err(e) => CallbackResult::Error(e),
      },
      _ => {
        panic!("unexpected ind: {}", i);
      }
    }
  }
}

// SEARCH INPUT FIELD

type SearchIF = ();

impl Transient for InputField<SearchIF, String> {
  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    let st = self.to_string();
    CallbackResult::File(
      // TODO: how do you now?
      0,
      Box::new(
        move |f: Wrap<dyn GenericFile>, desc: &Description, drone: &Drone| {
          f.write().unwrap().new_search(desc, drone, &st);
        },
      ),
    )
  }
}

impl RingElement for ring::Wrap<InputField<SearchIF, String>> {
  fn push_to_ring(&self, mut ring: RwLockWriteGuard<crate::logic::Ring>) {
    ring.push_transient(self.clone(), Mark::InputFloat, true);
    ring.push_input_sink(self.clone(), Mark::InputFloat);
    ring.push_static(self.clone(), Mark::InputFloat, Mark::Window, 2);
  }
}
