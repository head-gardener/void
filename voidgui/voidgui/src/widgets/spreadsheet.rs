use std::{cell::RefCell, rc::Rc};

use voidmacro::{Menu, ClickableMenu};

use crate::render::{painter::Painter, Point, TextTable, Origin};

use super::traits::{
  widget::WidgetError, CallbackResult, Clickable, ClickSink, Widget,
};

#[derive(Menu, ClickableMenu)]
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

    table.commit();
    Ok(Self {
      table,
      records: vec![Box::new(n), Box::new(p)],
    })
  }

  fn push(
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

  pub fn transaction<F>(&mut self, f: F) -> Result<(), WidgetError>
  where
    F: std::ops::Fn(
      &mut Self,
      for<'r, 's, 't0> fn(
        &'r mut Spreadsheet,
        painter: &Painter,
        &'s str,
        &'t0 str,
      ) -> Result<(), WidgetError>,
    ) -> Result<(), WidgetError>,
  {
    f(self, Spreadsheet::push)?;
    self.table.commit();
    Ok(())
  }

  pub fn drop(&mut self) {
    self.records.clear();

    self.table.truncate(2);
    self.table.commit();
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = Rc::new(RefCell::new(self));
    ring.push_clickable(rc.clone(), crate::logic::ring::Mark::Spreadsheet);
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
      0 => {
        println!("name");
        CallbackResult::Skip
      }
      1 => {
        println!("phone");
        CallbackResult::Skip
      }
      _ => {
        println!("{}", self.records.get(i - 2).unwrap());
        CallbackResult::Skip
      }
    }
  }
}
