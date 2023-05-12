use std::sync::RwLockReadGuard;

use crate::{
  logic::{
    ring::{self, RingElement},
    CallbackResult,
  },
  render::{
    painter::{Drone, DroneFeed, Painter},
    text_table::Orientation,
    Area, Origin, Point, TextTable,
  },
  widgets,
};

use super::traits::{
  widget::Error, ClickSink, Clickable, Drawable, Parent, Transient, Widget,
};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];
static TOOLBAR_TABLE_ITEMS: [&str; 2] = ["Pull", "Push"];
// static TOOLBAR_TOOLS_ITEMS: [&str; 1] = [":)"];

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Toolbar {
  table: TextTable,
}

impl Toolbar {
  pub unsafe fn new(
    painter: &RwLockReadGuard<Painter>,
    drone: &mut Drone,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        drone,
        painter,
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
    ring.push(
      rc,
      crate::logic::ring::Mark::Toolbar,
      crate::logic::ring::Mark::Window,
      1,
    );
  }
}

impl ClickSink for Toolbar {
  fn onclick(&self, drone: &Drone, p: Point) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      // 0 => unsafe { ToolbarTable::new(drone) }.map_or(
      //   CallbackResult::Error(Error::InitFailure("ToolbarTable")),
      //   |m| CallbackResult::Push(Box::new(ring::wrap(m))),
      // ),
      1 => {
        println!("Tools");
        CallbackResult::Pass
      }
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
      x: a.x,
      y: a.y + a.height,
      pole: crate::render::OriginPole::TopLeft,
    })
  }
}

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct ToolbarTable {
  table: TextTable,
}

impl ToolbarTable {
  pub unsafe fn new(
    painter: &RwLockReadGuard<Painter>,
    drone: &mut Drone,
  ) -> Result<Self, Error> {
    Ok(Self {
      table: TextTable::make_static(
        drone,
        painter,
        Orientation::Vertical,
        crate::render::text_table::CellStyle::Normal,
        &TOOLBAR_TABLE_ITEMS,
      )?,
    })
  }
}

impl RingElement for ring::Wrap<ToolbarTable> {
  fn push_to_ring(&self, ring: &mut crate::logic::Ring) {
    // ring.push_clickable(rc.clone(), crate::logic::ring::Mark::Toolbar);
    ring.replace_transient(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
    );
    ring.push(
      self.clone(),
      crate::logic::ring::Mark::ToolbarDropdown,
      crate::logic::ring::Mark::Toolbar,
      0,
    );
  }
}

impl Transient for ToolbarTable {}

// impl ClickableWidget for ToolbarTable {
//   fn onclick(&self, painter: &Painter, p: Point) -> CallbackResult {
//     let i = self.table.catch_point(&p).unwrap();
//     match i {
//       0 => println!("Table"),
//       1 => println!("Tools"),
//       _ => { panic!("unexpected ind: {}", i) }
//     }
//   }
// }

// #[derive(Menu, ClickableMenu)]
// pub struct ToolbarTools {
//   table: TextTable,
// }

// impl ToolbarTools {
//   pub unsafe fn new(painter: &Painter) -> Result<Self, WidgetError> {
//     Ok(Self {
//       table: TextTable::make_static(
//         painter,
//         Orientation::Vertical,
//         &TOOLBAR_TOOLS_ITEMS,
//       )?,
//     })
//   }

//   pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
//     let rc = Rc::new(RefCell::new(self));
//     ring.push_clickable(rc.clone(), crate::logic::ring::Mark::Toolbar);
//     ring.push(rc, crate::logic::ring::Mark::Toolbar);
//   }
// }

// impl ClickableWidget for ToolbarTools {
//   fn onclick(&self, p: Point) {
//     let i = self.table.catch_point(&p).unwrap();
//     match i {
//       0 => println!(":)"),
//       _ => { panic!("unexpected ind: {}", i) }
//     }
//   }
// }
