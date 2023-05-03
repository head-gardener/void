use crate::{
  logic::Layout,
  render::{
    painter::Painter,
    shapes::{Grid, Rectangle, Texture},
    Size,
  },
  widgets::traits::widget::WidgetError,
};

use super::{Area, Color, Origin, Point};

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

pub enum Orientation {
  Vertical,
  Horizontal,
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
  cells: Option<Vec<Area>>,
  constr: Size,

  origin: Option<Origin>,
}

impl TextTable {
  /// Generate a text table of static layout.
  pub unsafe fn make_static(
    painter: &Painter,
    or: Orientation,
    items: &[&str],
  ) -> Result<Self, WidgetError> {
    let (r, c) = match or {
      Orientation::Vertical => (items.len(), 1),
      Orientation::Horizontal => (1, items.len()),
    };

    let mut table = TextTable::from_text(painter, r, c, &items)?;
    table.commit();

    Ok(table)
  }

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
      constr: Size::new(0, 0),
      origin: None,
      cells: None,
      plotted: false,
    }
  }

  pub unsafe fn from_text<R>(
    painter: &Painter,
    rows: usize,
    columns: usize,
    text: &[R],
  ) -> Result<Self, WidgetError>
  where
    R: AsRef<str>,
  {
    let textures = text
      .iter()
      .map(|s| {
        let mut t = Texture::new();
        t.bind_text(painter, s.as_ref())?;
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
      .map(|t| t.expand(10, 10))
      .collect::<Vec<Size>>();

    let layout = Layout::from_sizes(rows, columns, padded.as_slice());

    let bg = (0..rows * columns)
      .map(|_| Rectangle::new(BG_COLOR_NORM))
      .collect();

    Ok(Self {
      grid: Grid::new(GRID_COLOR),
      rows,
      columns,
      bg,
      textures,
      texture_sizes: sizes,
      cell_sizes: padded,
      constr: layout.size(),
      layout: Some(layout),
      origin: None,
      cells: None,
      plotted: false,
    })
  }

  pub unsafe fn add_row<'a, I>(
    &mut self,
    painter: &Painter,
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

        let size = t.size();
        self.texture_sizes.push(size.clone());
        self.cell_sizes.push(size.expand(10, 10));

        self.textures.push(t);
        self.bg.push(Rectangle::new(color.into()));
        Ok(())
      })
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self.layout = None;
    self.cells = None;
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
    self.cells = None;
  }

  pub fn truncate(&mut self, len: usize) {
    self.textures.truncate(len);
    self.bg.truncate(len);
    self.texture_sizes.truncate(len);
    self.cell_sizes.truncate(len);

    self.layout = None;
    self.cells = None;
    self.rows = len / self.columns;
  }

  pub unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError> {
    let layout: &Layout =
      self.layout.as_ref().ok_or(WidgetError::Unspecified(
        "Plottig a text table with outdated layout".to_owned(),
      ))?;
    let origin = self
      .origin
      .ok_or(WidgetError::Uninitialized("origin"))?
      .to_point(&layout.size());

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
    self.cells = Some(layout);
    Ok(())
  }

  pub fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
    if !self.plotted {
      return Err(WidgetError::Unplotted("spreadsheet"));
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

  pub fn catch_point(&self, p: &Point) -> Option<usize> {
    self.cells.as_ref()?.iter().position(|c| p.contained(c))
  }

  pub fn set_origin(&mut self, origin: Origin) {
    if self.origin.map_or(false, |p| p == origin) {
      return;
    }

    self.origin = Some(origin);
    self.plotted = false;
    self.cells = None;
  }

  pub fn plotted(&self) -> bool {
    self.plotted
  }

  pub fn request_plot(&mut self) {
    self.plotted = false;
    self.cells = None;
  }

  pub fn constr(&self) -> Size {
    self.constr
  }

  pub fn area(&self) -> Option<Area> {
    let s = (&self.layout).as_ref().map(|s| s.size().clone())?;
    self.origin.map(|o| Area::from_prim(o.to_point(&s), s))
  }

  pub fn cells(&self) -> Option<&Vec<Area>> {
    self.cells.as_ref()
  }
}

// #[cfg(test)]
// mod test {
//   use super::*;

//   #[test]
//   fn state_changes() {
//     let mut t = unsafe { TextTable::new(0, 3) };
//   }
// }