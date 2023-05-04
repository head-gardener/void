use std::{cell::RefCell, rc::Rc};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  render::{painter::Painter, Origin, Point, TextTable},
  widgets::InputField,
};

use super::traits::{
  widget::WidgetError, CallbackResult, ClickSink, Clickable, Drawable, Widget,
};

#[derive(Menu, DrawableMenu, ClickableMenu)]
pub struct Spreadsheet {
  table: TextTable,
  records: Vec<Box<String>>,
}

impl Spreadsheet {
  pub unsafe fn new(painter: &Painter) -> Result<Self, WidgetError> {
    let n = "Name".to_owned();
    let p = "Phone".to_owned();

    let mut table = TextTable::new(0, 2);
    table.add_row(
      painter,
      [&n, &p].iter(),
      crate::render::text_table::CellColor::Darker,
    )?;

    Ok(Self {
      table,
      records: vec![Box::new(n), Box::new(p)],
    })
  }

  pub fn push(
    &mut self,
    painter: &Painter,
    name: &str,
    phone: &str,
  ) -> Result<(), WidgetError> {
    let n = name.to_owned();
    let p = phone.to_owned();

    unsafe {
      self.table.add_row(
        painter,
        [&n, &p].iter(),
        crate::render::text_table::CellColor::Normal,
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
    let rc = Rc::new(RefCell::new(self));
    ring.push_click_sink(rc.clone(), crate::logic::ring::Mark::Spreadsheet);
    ring.push(
      rc,
      crate::logic::ring::Mark::Spreadsheet,
      crate::logic::ring::Mark::Window,
      0,
    );
  }
}

impl ClickSink for Spreadsheet {
  fn onclick(&self, painter: &Painter, p: Point) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 | 1 => CallbackResult::Skip,
      _ => {
        let s = self.records.get(i - 2).unwrap();
        match unsafe { InputField::new(painter, s) } {
          Ok(f) => CallbackResult::Push(Box::new(f.wrap(
            crate::logic::ring::Mark::SpreadsheetInputField,
            crate::logic::ring::Mark::Window,
            2,
          ))),
          Err(e) => CallbackResult::Error(e),
        }
      }
    }
  }
}
