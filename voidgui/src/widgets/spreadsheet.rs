use crate::render::{painter::Painter, Point, TextTable};

use super::{widget::WidgetError, Widget};

pub struct Spreadsheet {
  table: TextTable,
  records: Vec<Box<String>>,
}

impl Spreadsheet {
  pub unsafe fn new(painter: &dyn Painter) -> Result<Self, WidgetError> {
    let records =
      vec![Box::new("Name".to_owned()), Box::new("Phone".to_owned())];
    let mut table = TextTable::from_text(painter, 1, 2, records.as_slice())?;
    table.set_origin(Point::new(30, 30));
    Ok(Self { table, records })
  }
}

impl Widget for Spreadsheet {
  unsafe fn plot(&mut self, painter: &dyn Painter) -> Result<(), WidgetError> {
    self.table.plot(painter)
  }

  fn draw(&self, painter: &dyn Painter) -> Result<(), WidgetError> {
    self.table.draw(painter)
  }

  fn plotted(&self) -> bool {
    self.table.plotted()
  }

  fn catch(&self) -> bool {
    todo!()
  }
}
