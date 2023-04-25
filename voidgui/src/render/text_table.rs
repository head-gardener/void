use crate::{
  render::{
    layout::Layout,
    painter::Painter,
    shapes::{Grid, Rectangle, Texture},
    Size,
  },
  widgets::widget::WidgetError,
};

use super::{Area, Color, Point};

pub struct TextTable {
  layout: Layout,
  grid: Grid,
  bg: Vec<Rectangle>,
  textures: Vec<Texture>,
  texture_sizes: Vec<Size>,
  origin: Option<Point>,
}

static BG_COLOR: Color = (0.7, 0.7, 0.75, 1.0);
static GRID_COLOR: Color = (0.6, 0.6, 0.6, 1.0);

impl TextTable {
  pub unsafe fn from_text(
    painter: &Painter,
    rows: usize,
    columns: usize,
    text: &[&str],
  ) -> Result<Self, WidgetError> {
    let textures: Vec<Texture> = text
      .iter()
      .map(|s| {
        let mut t = Texture::new();
        t.bind_text(painter, s)?;
        Ok(t)
      })
      .collect::<Result<Vec<Texture>, String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    let sizes = textures
      .iter()
      .map(|t| t.size().to_owned())
      .collect::<Vec<Size>>();

    let padded = sizes
      .iter()
      .map(|t| {
        let mut x = t.to_owned();
        x.expand(10, 10);
        x
      })
      .collect::<Vec<Size>>();

    let layout = Layout::from_sizes(rows, columns, padded.as_slice());

    let bg = (0..rows * columns)
      .map(|_| Rectangle::new(BG_COLOR))
      .collect();

    Ok(Self {
      layout,
      grid: Grid::new(GRID_COLOR),
      bg,
      textures,
      texture_sizes: sizes,
      origin: None,
    })
  }

  pub unsafe fn plot(&self, painter: &Painter) -> Result<(), WidgetError> {
    let origin = self.origin.ok_or(WidgetError::Unspecified(
      "Origin should be set before plotting".to_owned(),
    ))?;

    self
      .grid
      .plot(
        painter,
        self.layout.rows(),
        self.layout.columns(),
        self.layout.row_ratio(),
        self.layout.column_ratio(),
        &Area::new(
          origin.x,
          origin.y,
          self.layout.size().width,
          self.layout.size().height,
        ),
      )
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    let layout = self.layout.plot(&origin);

    self
      .textures
      .iter()
      .zip(self.texture_sizes.iter())
      .zip(layout.iter())
      .map(|((t, s), a)| {
        t.plot(painter, &Area::new(a.x + 10, a.y + 10, s.width, s.height))
      })
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self
      .bg
      .iter()
      .zip(layout.iter())
      .map(|(r, a)| r.plot(painter, a))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    Ok(())
  }

  pub unsafe fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
    self
      .bg
      .iter()
      .map(|r| r.draw(painter))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
    self.grid.draw(painter)
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
    self
      .textures
      .iter()
      .map(|t| t.draw(painter))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))
  }

  pub fn set_origin(&mut self, origin: Point) {
    self.origin = Some(origin);
  }
}
