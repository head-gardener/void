use std::{cell::RefCell, rc::Rc};

use crate::render::{painter::Painter, Point, TextTable};

use super::traits::{widget::WidgetError, Clickable, ClickableWidget, Widget};

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

    table.set_origin(Point::new(30, 30));
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
    ring.push(rc, crate::logic::ring::Mark::Spreadsheet);
  }
}

impl Widget for Spreadsheet {
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError> {
    self.table.plot(painter)
  }

  fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
    self.table.draw(painter)
  }

  fn plotted(&self) -> bool {
    self.table.plotted()
  }

  fn set_origin(&mut self, origin: &Point) {
    self.table.set_origin(origin.clone());
  }

  fn request_plot(&mut self) {
    self.table.request_plot();
  }
}

impl Clickable for Spreadsheet {
  fn click_area(&self) -> Option<crate::render::Area> {
    self.table.area()
  }
}

impl ClickableWidget for Spreadsheet {
  fn onclick(&self, p: Point) {
    println!("clickY!")
  }
}
