use std::mem::swap;

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

#[derive(Debug)]
enum State {
  None,
  Committed(Layout),
  Plotted(Layout, Vec<Area>),
}

/// Value by which text is offset from cells' borders.
const OFFSET: u16 = 10;

impl State {
  /// Returns `true` if the state is [`Plotted`].
  ///
  /// [`Plotted`]: State::Plotted
  fn is_plotted(&self) -> bool {
    matches!(self, Self::Plotted(..))
  }

  /// Returns `true` if the state is [`None`].
  ///
  /// [`None`]: State::None
  fn is_none(&self) -> bool {
    matches!(self, Self::None)
  }

  fn try_layout(&self) -> Result<&Layout, WidgetError> {
    match self {
      State::None => Err(WidgetError::Unspecified(
        "Layout hasn't been committed".to_string(),
      )),
      State::Committed(l) => Ok(l),
      State::Plotted(l, _) => Ok(l),
    }
  }

  fn try_cells(&self) -> Result<&Vec<Area>, WidgetError> {
    match self {
      State::None => Err(WidgetError::Unspecified(
        "Layout hasn't been committed".to_string(),
      )),
      State::Committed(_) => {
        Err(WidgetError::Unplotted("Cells haven't been plotted"))
      }
      State::Plotted(_, c) => Ok(c),
    }
  }

  /// Ensures this state is [State::Committed] or worse, downgrading if
  /// necessary.
  fn ensure_committed_or_worse(&mut self) {
    let mut s = Self::default();
    std::mem::swap(self, &mut s);
    if let Self::Plotted(l, _) = s {
      *self = Self::Committed(l);
    } else {
      *self = s;
    }
  }
}

impl TryInto<Layout> for State {
  type Error = WidgetError;

  fn try_into(self) -> Result<Layout, Self::Error> {
    match self {
      State::None => Err(WidgetError::Unspecified(
        "Layout hasn't been committed".to_string(),
      )),
      State::Committed(l) => Ok(l),
      State::Plotted(l, _) => Ok(l),
    }
  }
}

impl Default for State {
  fn default() -> Self {
    Self::None
  }
}

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
  grid: Grid,
  rows: usize,
  columns: usize,

  bg: Vec<Rectangle>,
  textures: Vec<Texture>,

  texture_sizes: Vec<Size>,
  cell_sizes: Vec<Size>,
  constr: Size,

  origin: Option<Origin>,
  state: State,
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
    table.ensure_committed();

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
      rows,
      columns,
      grid: Grid::new(GRID_COLOR),
      bg: vec![],
      textures: vec![],
      texture_sizes: vec![],
      cell_sizes: vec![],
      constr: Size::new(0, 0),
      origin: None,
      state: State::None,
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
      .map(|t| t.expand(OFFSET, OFFSET))
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
      origin: None,
      state: State::Committed(layout),
    })
  }

  /// Add new row to the table and request plotting.
  ///
  /// # Errors
  ///
  /// Returns an error only if [Texture::bind_text] fails.
  ///
  /// # Safety
  ///
  /// Unsafe due to creating and rendering textures.
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
        self.cell_sizes.push(size.expand(OFFSET, OFFSET));

        self.textures.push(t);
        self.bg.push(Rectangle::new(color.into()));
        Ok(())
      })
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self.rows += 1;
    self.state = State::None;

    Ok(())
  }

  /// Update texture and sizes for specific cell.
  ///
  /// # Errors
  ///
  /// This function will return an error if `n` is out of bounds or if
  /// `Texture::bind_text` fails.
  pub fn update_cell(
    &mut self,
    p: &Painter,
    n: usize,
    s: &str,
  ) -> Result<(), WidgetError> {
    unsafe {
      self
        .textures
        .iter_mut()
        .zip(self.texture_sizes.iter_mut())
        .zip(self.cell_sizes.iter_mut())
        .nth(n)
        .map(|((t, ts), cs)| -> Result<(), String> {
          t.bind_text(p, s)?;

          let size = t.size();
          *ts = size.clone();
          *cs = size.expand(OFFSET, OFFSET);

          Ok(())
        })
        .ok_or(WidgetError::Unspecified(format!(
          "n out of bounds in update_cell: {}",
          n
        )))?
        .map_err(|e| WidgetError::Unspecified(e.to_owned()))
    }?;

    self.state = State::None;

    Ok(())
  }

  /// Ensure this [TextTable] is has been committed, calculating
  /// new [Layout] if necessary.
  pub fn ensure_committed(&mut self) {
    if !self.state.is_none() {
      return;
    }

    self.state = State::Committed(Layout::from_sizes(
      self.rows,
      self.columns,
      self.cell_sizes.as_slice(),
    ));
  }

  pub fn truncate(&mut self, len: usize) {
    self.textures.truncate(len);
    self.bg.truncate(len);
    self.texture_sizes.truncate(len);
    self.cell_sizes.truncate(len);

    self.rows = len / self.columns;
    self.state = State::None;
  }

  /// Plot this [TextTable], committing its layout if necessary.
  pub unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError> {
    let origin = self.origin.ok_or(WidgetError::Uninitialized("origin"))?;

    self.ensure_committed();
    let mut state = State::default();
    swap(&mut state, &mut self.state);

    let layout: Layout = state.try_into().unwrap();
    let origin = origin.to_point(&layout.size());
    let cells = layout.plot(&origin);

    self.do_plot(painter, origin, &layout, &cells)?;

    self.state = State::Plotted(layout, cells);
    Ok(())
  }

  pub fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
    if !self.state.is_plotted() {
      return Err(WidgetError::Unplotted("spreadsheet"));
    }

    unsafe { self.do_draw(painter) }
  }

  pub fn catch_point(&self, p: &Point) -> Option<usize> {
    self
      .state
      .try_cells()
      .ok()?
      .iter()
      .position(|c| p.contained(c))
  }

  pub fn set_origin(&mut self, origin: Origin) {
    if self.origin.map_or(false, |p| p == origin) {
      return;
    }

    self.origin = Some(origin);
    self.state.ensure_committed_or_worse();
  }

  pub fn plotted(&self) -> bool {
    self.state.is_plotted()
  }

  pub fn request_plot(&mut self) {
    self.state.ensure_committed_or_worse();
  }

  pub fn constr(&self) -> Size {
    self.constr
  }

  pub fn area(&self) -> Option<Area> {
    let s = self.state.try_layout().ok()?.size().clone();
    self.origin.map(|o| Area::from_prim(o.to_point(&s), s))
  }

  pub fn cells(&self) -> Option<&Vec<Area>> {
    self.state.try_cells().ok()
  }
}

#[cfg(not(test))]
impl TextTable {
  unsafe fn do_draw(&self, p: &Painter) -> Result<(), WidgetError> {
    self
      .bg
      .iter()
      .map(|r| r.draw(p))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
    self
      .grid
      .draw(p)
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;
    self
      .textures
      .iter()
      .map(|t| t.draw(p))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))
  }

  unsafe fn do_plot(
    &mut self,
    p: &Painter,
    o: Point,
    l: &Layout,
    cs: &Vec<Area>,
  ) -> Result<(), WidgetError> {
    self
      .grid
      .plot(
        p,
        l.rows(),
        l.columns(),
        l.row_ratio(),
        l.column_ratio(),
        &Area::new(o.x, o.y, l.size().width, l.size().height),
      )
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self
      .textures
      .iter()
      .zip(self.texture_sizes.iter())
      .zip(cs.iter())
      .map(|((t, s), a)| {
        t.plot(p, &Area::new(a.x + OFFSET, a.y + OFFSET, s.width, s.height))
      })
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?;

    self
      .bg
      .iter()
      .zip(cs.iter())
      .map(|(r, a)| r.plot(p, a))
      .collect::<Result<(), String>>()
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))
  }
}

#[cfg(test)]
impl TextTable {
  unsafe fn do_draw(&self, _: &Painter) -> Result<(), WidgetError> {
    Ok(())
  }

  unsafe fn do_plot(
    &self,
    _: &Painter,
    _: Point,
    _: &Layout,
    _: &Vec<Area>,
  ) -> Result<(), WidgetError> {
    Ok(())
  }

  fn state(&self) -> &State {
    &self.state
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn layout() {
    let offset = OFFSET * 2;

    let p = unsafe { Painter::new(0, 0) };
    let mut t = unsafe {
      TextTable::from_text(&p, 2, 2, &["abc", "ab", "a\na", "a"]).unwrap()
    };

    t.set_origin(Origin::new(100, 100, crate::render::OriginPole::TopLeft));
    unsafe { t.plot(&p).unwrap() };
    assert_eq!(
      *t.cells().unwrap(),
      {
        let w1 = offset + 3;
        let w2 = offset + 2;
        let h1 = offset + 1;
        let h2 = offset + 2;
        vec![
          Area::new(100, 100, w1, h1),
          Area::new(100 + w1, 100, w2, h1),
          Area::new(100, 100 + h1, w1, h2),
          Area::new(100 + w1, 100 + h1, w2, h2),
        ]
      },
      "textures are arranged correctly"
    );

    t.update_cell(&p, 1, "abc\nabc").unwrap();
    unsafe { t.plot(&p).unwrap() };
    assert_eq!(
      *t.cells().unwrap(),
      {
        let w1 = offset + 3;
        let w2 = offset + 3;
        let h1 = offset + 2;
        let h2 = offset + 2;
        vec![
          Area::new(100, 100, w1, h1),
          Area::new(100 + w1, 100, w2, h1),
          Area::new(100, 100 + h1, w1, h2),
          Area::new(100 + w1, 100 + h1, w2, h2),
        ]
      },
      "`update_cell` works"
    );

    unsafe {
      t.set_origin(Origin::new(100, 100, crate::render::OriginPole::TopLeft));
      t.add_row(&p, vec![&"abcd".to_owned(); 2].iter(), CellColor::Normal)
        .unwrap();
      t.plot(&p).unwrap();
    };
    assert_eq!(
      *t.cells().unwrap(),
      {
        let w1 = offset + 4;
        let w2 = offset + 4;
        let h1 = offset + 2;
        let h2 = offset + 2;
        let h3 = offset + 1;
        vec![
          Area::new(100, 100, w1, h1),
          Area::new(100 + w1, 100, w2, h1),
          Area::new(100, 100 + h1, w1, h2),
          Area::new(100 + w1, 100 + h1, w2, h2),
          Area::new(100, 100 + h1 + h2, w1, h3),
          Area::new(100 + w1, 100 + h1 + h2, w2, h3),
        ]
      },
      "`add_row` works",
    );
  }

  #[test]
  fn state_changes() {
    let p = unsafe { Painter::new(0, 0) };

    let mut t = unsafe { TextTable::new(0, 3) };
    assert!(
      matches!(t.state(), State::None),
      "table starts with no state"
    );

    assert!(
      matches!(unsafe { t.plot(&p) }, Err(_)),
      "`plot` errors when called before commit"
    );

    unsafe {
      t.add_row(&p, vec![&"".to_owned(); 3].iter(), CellColor::Normal)
        .unwrap();
      t.add_row(&p, vec![&"".to_owned(); 3].iter(), CellColor::Normal)
        .unwrap();
    };
    assert!(
      matches!(t.state(), State::None),
      "`add_row` resets state to none"
    );

    t.ensure_committed();
    assert!(
      matches!(t.state(), State::Committed(_)),
      "committing works for no state and makes state committed"
    );

    t.truncate(3);
    assert!(
      matches!(t.state(), State::None),
      "`truncate` resets state to none"
    );

    t.ensure_committed();
    assert_eq!(
      unsafe { t.plot(&p) },
      Err(WidgetError::Uninitialized("origin")),
      "`plot` errors when origin isn't set"
    );
    assert!(
      matches!(t.state(), State::Committed(_)),
      "`plot` called without origin doesn't reset state"
    );

    t.set_origin(Origin::new(0, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state(), State::Committed(_)),
      "`set_origin` doesn't affect state when it's committed"
    );

    assert_eq!(
      unsafe { t.plot(&p) },
      Ok(()),
      "`plot` works when state is committed and origin is set"
    );

    assert!(
      matches!(t.state(), State::Plotted(_, _)),
      "`plot` sets state to plotted"
    );

    t.ensure_committed();
    assert!(
      matches!(t.state(), State::Plotted(_, _)),
      "`ensure_committed` is a no-op when state table is plotted"
    );

    assert_eq!(t.cells().unwrap().len(), 3, "`truncate` works");

    t.set_origin(Origin::new(0, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state(), State::Plotted(_, _)),
      "`set_origin` is a no-op when origin remains the same after the call"
    );

    t.set_origin(Origin::new(100, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state(), State::Committed(_)),
      "`set_origin` downgrades state to committed from plotted"
    );

    unsafe { t.plot(&p).unwrap() };
    t.request_plot();
    assert!(
      matches!(t.state(), State::Committed(_)),
      "`request_plot` downgrades state to committed from plotted"
    );
  }
}
