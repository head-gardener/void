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

static BG_COLOR_NORM: Color = (0.7, 0.7, 0.75, 1.0);
static BG_COLOR_DARK: Color = (0.6, 0.6, 0.7, 1.0);
static BG_COLOR_LGHT: Color = (0.8, 0.8, 0.83, 1.0);
static GRID_COLOR: Color = (0.5, 0.5, 0.5, 1.0);

#[derive(Clone, Copy)]
pub enum CellColor {
  Normal,
  Darker,
  Lighter,
}

impl Into<Color> for CellColor {
  fn into(self) -> Color {
    match self {
      CellColor::Normal => BG_COLOR_NORM,
      CellColor::Darker => BG_COLOR_DARK,
      CellColor::Lighter => BG_COLOR_LGHT,
    }
  }
}

pub struct TextTable {
  layout: Option<Layout>,
  grid: Grid,
  plotted: bool,
  rows: usize,
  columns: usize,

  bg: Vec<Rectangle>,
  textures: Vec<Texture>,

  texture_sizes: Vec<Size>,
  cell_sizes: Vec<Size>,

  origin: Option<Point>,
}

impl TextTable {
  pub unsafe fn new(rows: usize, columns: usize) -> Self {
    if rows != 0 && columns != 0 {
      panic!(
        "Either rows or columns should be zero when
        TextTable is created with new"
      );
    }

    Self {
      layout: None,
      rows,
      columns,
      grid: Grid::new(GRID_COLOR),
      bg: vec![],
      textures: vec![],
      texture_sizes: vec![],
      cell_sizes: vec![],
      origin: None,
      plotted: false,
    }
  }

  pub unsafe fn from_text(
    painter: &dyn Painter,
    rows: usize,
    columns: usize,
    text: &[Box<String>],
  ) -> Result<Self, WidgetError> {
    let textures = text
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
      .map(|_| Rectangle::new(BG_COLOR_NORM))
      .collect();

    Ok(Self {
      layout: Some(layout),
      grid: Grid::new(GRID_COLOR),
      rows,
      columns,
      bg,
      textures,
      texture_sizes: sizes,
      cell_sizes: padded,
      origin: None,
      plotted: false,
    })
  }

  pub unsafe fn add_row<'a, I>(
    &mut self,
    painter: &dyn Painter,
    mut data: I,
    color: CellColor,
  ) -> Result<(), WidgetError>
  where
    I: std::iter::Iterator<Item = &'a &'a String>,
  {
    data
      .try_for_each(|s| -> Result<(), String> {
        let mut t = Texture::new();
        t.bind_text(painter, s)?;

        let mut size = t.size().to_owned();
        self.texture_sizes.push(size.clone());
        size.expand(10, 10);
        self.cell_sizes.push(size);

        self.textures.push(t);
        self.bg.push(Rectangle::new(color.into()));
        Ok(())
      })
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self.layout = None;
    self.rows += 1;

    Ok(())
  }

  pub fn commit(&mut self) {
    self.layout = Some(Layout::from_sizes(
      self.rows,
      self.columns,
      self.cell_sizes.as_slice(),
    ));

    self.plotted = false;
  }

  pub fn truncate(&mut self, len: usize) {
    self.textures.truncate(len);
    self.bg.truncate(len);
    self.texture_sizes.truncate(len);
    self.cell_sizes.truncate(len);

    self.layout = None;
    self.rows = len / self.columns;
  }

  pub unsafe fn plot(
    &mut self,
    painter: &dyn Painter,
  ) -> Result<(), WidgetError> {
    let origin = self
      .origin
      .ok_or(WidgetError::Uninitialized("origin".to_owned()))?;
    let layout: &Layout =
      self.layout.as_ref().ok_or(WidgetError::Unspecified(
        "Plottig a text table with outdated layout".to_owned(),
      ))?;
    self
      .grid
      .plot(
        painter,
        layout.rows(),
        layout.columns(),
        layout.row_ratio(),
        layout.column_ratio(),
        &Area::new(
          origin.x,
          origin.y,
          layout.size().width,
          layout.size().height,
        ),
      )
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    let layout = layout.plot(&origin);

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

    self.plotted = true;
    Ok(())
  }

  pub fn draw(&self, painter: &dyn Painter) -> Result<(), WidgetError> {
    if !self.plotted {
      return Err(WidgetError::Unplotted("spreadsheet".to_owned()));
    }

    unsafe {
      self
        .bg
        .iter()
        .map(|r| r.draw(painter))
        .collect::<Result<(), String>>()
        .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
      self
        .grid
        .draw(painter)
        .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
      self
        .textures
        .iter()
        .map(|t| t.draw(painter))
        .collect::<Result<(), String>>()
        .map_err(|e| WidgetError::Unspecified(e.to_owned()))
    }
  }

  pub fn set_origin(&mut self, origin: Point) {
    self.origin = Some(origin);
    self.plotted = false;
  }

  pub fn plotted(&self) -> bool {
    self.plotted
  }
}
