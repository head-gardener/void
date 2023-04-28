use crate::render::{painter::Painter, Point, TextTable};

use super::{widget::WidgetError, Widget};

pub struct Spreadsheet {
  table: TextTable,
  records: Vec<Box<String>>,
}

impl Spreadsheet {
  pub unsafe fn new(painter: &dyn Painter) -> Result<Self, WidgetError> {
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
    painter: &dyn Painter,
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
        painter: &dyn Painter,
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
}

impl Widget for Spreadsheet {
  unsafe fn plot(&mut self, painter: &dyn Painter) -> Result<(), WidgetError> {
    self.table.plot(painter)
  }

  fn draw(&self, painter: &dyn Painter) -> Result<(), WidgetError> {
    self.table.draw(painter)
  }

  fn catch(&self) -> bool {
    todo!()
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
