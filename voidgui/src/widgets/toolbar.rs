use crate::render::{painter::Painter, Point, TextTable};

use super::{widget::WidgetError, Widget};

pub struct Toolbar {
  table: TextTable,
}

static TOOLBAR_ITEMS: [&str; 2] = ["Table", "Tools"];

impl Toolbar {
  pub unsafe fn new(painter: &dyn Painter) -> Result<Self, WidgetError> {
    let mut table = TextTable::from_text(
      painter,
      1,
      2,
      &TOOLBAR_ITEMS
        .iter()
        .map(|s| -> Box<String> { Box::new(s.to_string()) })
        .collect::<Vec<Box<String>>>(),
    )?;
    table.commit();

    Ok(Self { table })
  }
}

impl Widget for Toolbar {
  unsafe fn plot(&mut self, painter: &dyn Painter) -> Result<(), WidgetError> {
    self.table.set_origin(Point::new(
      painter.window_area().width - self.table.constr().width,
      0,
    ));
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
