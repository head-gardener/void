use std::{cell::RefCell, rc::Rc};

use voidmacro::{ClickableMenu, DrawableMenu, Menu};

use crate::{
  logic::ring::{Mark, RingMember},
  render::{painter::Painter, Origin, Point, TextTable},
  widgets::InputField,
};

use super::traits::{
  widget::WidgetError, CallbackResult, ClickSink, Clickable, Drawable,
  Transient, Widget,
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
    ring.push_click_sink(rc.clone(), Mark::Spreadsheet);
    ring.push(rc, Mark::Spreadsheet, Mark::Window, 0);
  }

  pub fn update_record(
    &mut self,
    p: &Painter,
    n: usize,
    s: &str,
  ) -> Result<(), WidgetError> {
    let s = s.to_string();
    self.table.update_cell(p, n + 2, &s)?;
    *self.records[n] = s;
    Ok(())
  }
}

impl ClickSink for Spreadsheet {
  fn onclick(&self, painter: &Painter, p: Point) -> CallbackResult {
    let i = self.table.catch_point(&p).unwrap();
    match i {
      0 | 1 => CallbackResult::Skip,
      _ => {
        let s = self.records.get(i - 2).unwrap();
        match unsafe { InputField::new(painter, s, i - 2) } {
          Ok(f) => CallbackResult::Push(Box::new(Rc::new(RefCell::new(f)))),
          Err(e) => CallbackResult::Error(e),
        }
      }
    }
  }
}

type SpreadsheetIF = usize;

impl Transient for InputField<SpreadsheetIF> {
  fn handle_cancel(&self, _: &Painter) -> CallbackResult {
    CallbackResult::None
  }

  fn handle_accept(&self, _: &Painter) -> CallbackResult {
    let c = *self.closure();
    let d = self.to_string();
    CallbackResult::Modify(Mark::Spreadsheet, Box::new(move |s, p| {
      s.expect("Spreadsheet should always be in the ring")
        .borrow_mut()
        .downcast_mut()
        .map(|s: &mut Spreadsheet| {
          s.update_record(p, c, &d).unwrap();
        });
    }))
  }
}

impl RingMember for Rc<RefCell<InputField<SpreadsheetIF>>> {
  fn push_to_ring(&self, ring: &mut crate::logic::Ring) {
    ring.push_input_sink(self.clone(), Mark::SpreadsheetInputField);
    ring.replace_transient(self.clone(), Mark::SpreadsheetInputField);
    ring.push(self.clone(), Mark::SpreadsheetInputField, Mark::Window, 2);
  }
}
