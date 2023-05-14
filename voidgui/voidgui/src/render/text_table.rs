use std::{mem::swap, sync::RwLockReadGuard};

use crate::widgets::traits::widget;
use crate::{
  colorscheme::{
    CELL_BG_COLOR_DARK, CELL_BG_COLOR_LGHT, CELL_BG_COLOR_NORM, GRID_COLOR,
  },
  logic::Layout,
  render::{painter::Drone, Size},
  widgets::traits::widget::Error,
};

use super::shapes::TextureData;
use super::{
  painter::{Description, DroneFeed},
  shapes::rectangle,
  Area, Origin, Point,
};

#[derive(Debug)]
enum State {
  /// Table was initialized or modified.
  None,

  /// [Layout] was calculated and table wasn't plotted, origin has changed
  /// or a plot was requested since calculating layout.
  Committed(Layout),

  /// Table was plotted and suffered no changes since then.
  Plotted(Layout, Vec<Area>),
}

/// Value by which text is offset from cells' borders.
pub const OFFSET: u16 = 10;

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

  fn try_layout(&self) -> Result<&Layout, Error> {
    match self {
      State::None => Err(Error::Unspecified(
        "Layout hasn't been committed".to_string(),
      )),
      State::Committed(l) => Ok(l),
      State::Plotted(l, _) => Ok(l),
    }
  }

  fn try_cells(&self) -> Result<&Vec<Area>, Error> {
    match self {
      State::None => Err(Error::Unspecified(
        "Layout hasn't been committed".to_string(),
      )),
      State::Committed(_) => {
        Err(Error::Unplotted("Cells haven't been plotted"))
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
  type Error = Error;

  fn try_into(self) -> Result<Layout, Self::Error> {
    match self {
      State::None => Err(Error::Unspecified(
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

#[derive(Clone, Copy)]
pub enum CellStyle {
  Normal,
  Darker,
  Lighter,
  Lit,
}

impl Into<rectangle::Style> for CellStyle {
  fn into(self) -> rectangle::Style {
    match self {
      CellStyle::Normal => rectangle::Style::Solid(CELL_BG_COLOR_NORM),
      CellStyle::Darker => rectangle::Style::Solid(CELL_BG_COLOR_DARK),
      CellStyle::Lighter => rectangle::Style::Solid(CELL_BG_COLOR_LGHT),
      CellStyle::Lit => rectangle::Style::Lit(CELL_BG_COLOR_DARK, None),
    }
  }
}

pub enum Orientation {
  Vertical,
  Horizontal,
}

pub struct TextTable {
  rows: usize,
  columns: usize,

  grid: usize,
  bg: Vec<usize>,
  textures: Vec<usize>,

  texture_sizes: Vec<Size>,
  cell_sizes: Vec<Size>,
  constr: Size,

  origin: Option<Origin>,
  state: State,
}

impl TextTable {
  /// Generate a text table of static layout.
  pub unsafe fn make_static(
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    or: Orientation,
    style: CellStyle,
    items: &[&str],
  ) -> Result<Self, Error> {
    let (r, c) = match or {
      Orientation::Vertical => (items.len(), 1),
      Orientation::Horizontal => (1, items.len()),
    };

    let table = TextTable::from_text(desc, drone, r, c, style, &items)?;

    Ok(table)
  }

  pub unsafe fn new(
    drone: &Drone,
    rows: usize,
    columns: usize,
  ) -> Result<Self, widget::Error> {
    if rows != 0 && columns != 0 {
      panic!(
        "Either rows or columns should be zero when
        TextTable is created with new"
      );
    }

    Ok(Self {
      rows,
      columns,
      grid: drone.get_grids(1, GRID_COLOR).ok_or(Error::InitFailure)?[0],
      bg: vec![],
      textures: vec![],
      texture_sizes: vec![],
      cell_sizes: vec![],
      constr: Size::new(0, 0),
      origin: None,
      state: State::None,
    })
  }

  pub unsafe fn from_text<R>(
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    rows: usize,
    columns: usize,
    style: CellStyle,
    text: &[R],
  ) -> Result<Self, Error>
  where
    R: AsRef<str>,
  {
    let data = text
      .iter()
      .map(|s| TextureData::from_text(desc.font(), s.as_ref()))
      .collect::<Result<Vec<TextureData>, String>>()?;

    let sizes = data.iter().map(|t| t.into()).collect::<Vec<Size>>();

    let textures = drone.get_textures(text.len()).ok_or(Error::InitFailure)?;
    textures
      .iter()
      .zip(data.into_iter())
      .for_each(|(t, d)| drone.feed().bind_text(*t, d));

    let padded = sizes
      .iter()
      .map(|t| t.expand(OFFSET, OFFSET))
      .collect::<Vec<Size>>();

    let layout = Layout::from_sizes(rows, columns, padded.as_slice());

    Ok(Self {
      grid: drone.get_grids(1, GRID_COLOR).ok_or(Error::InitFailure)?[0],
      rows,
      columns,
      bg: drone.get_rectangles(textures.len(), style.into()).unwrap(),
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
  pub unsafe fn add_row<'a, I>(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    data: I,
    style: CellStyle,
  ) -> Result<(), Error>
  where
    I: std::iter::Iterator<Item = &'a str>,
  {
    let tex = drone
      .get_textures(self.columns)
      .ok_or(Error::SpawnFailure)?;
    data
      .zip(tex.into_iter())
      .try_for_each(|(s, t)| -> Result<(), String> {
        let d: TextureData = TextureData::from_text(desc.font(), s)?;
        let size: Size = (&d).into();
        drone.feed().bind_text(t, d);

        self.texture_sizes.push(size.clone());
        self.cell_sizes.push(size.expand(OFFSET, OFFSET));

        self.textures.push(t);
        Ok(())
      })?;

    self.bg.append(
      &mut drone
        .get_rectangles(self.columns, style.into())
        .ok_or(Error::InitFailure)?,
    );

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
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    n: usize,
    s: &str,
  ) -> Result<(), Error> {
    unsafe {
      self
        .textures
        .iter_mut()
        .zip(self.texture_sizes.iter_mut())
        .zip(self.cell_sizes.iter_mut())
        .nth(n)
        .map(|((t, ts), cs)| -> Result<(), String> {
          let d = TextureData::from_text(&desc.font(), s)?;
          let size: Size = (&d).into();
          drone.feed().bind_text(*t, d);

          *ts = size.clone();
          *cs = size.expand(OFFSET, OFFSET);

          Ok(())
        })
        .ok_or(Error::Unspecified(format!(
          "n out of bounds in update_cell: {}",
          n
        )))?
        .map_err(|e| Error::Unspecified(e.to_owned()))
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
  pub fn plot(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    feed: &DroneFeed,
  ) -> Result<(), Error> {
    let origin = self.origin.ok_or(Error::Uninitialized("origin"))?;

    self.ensure_committed();
    let mut state = State::default();
    swap(&mut state, &mut self.state);

    let layout: Layout = state.try_into().unwrap();
    let origin = origin.to_point(&layout.size());
    let cells = layout.plot(&origin);

    let vs = Area::new(
      origin.x,
      origin.y,
      layout.size().width,
      layout.size().height,
    )
    .gridify(
      desc.window_area(),
      layout.rows(),
      layout.columns(),
      layout.row_ratio(),
      layout.column_ratio(),
    );
    feed.plot_grid(self.grid, vs);

    self
      .textures
      .iter()
      .zip(self.texture_sizes.iter())
      .zip(cells.iter())
      .for_each(|((t, s), a)| {
        feed.plot_tex(
          *t,
          Area::new(a.x + OFFSET, a.y + OFFSET, s.width, s.height)
            .to_normalized(desc.window_area())
            .to_tex_coords(),
        );
      });

    self.bg.iter().zip(cells.iter()).for_each(|(r, a)| {
      feed.plot_rectangle(*r, a.to_normalized(desc.window_area()).to_coords())
    });

    self.state = State::Plotted(layout, cells);
    Ok(())
  }

  pub fn draw(&mut self, feed: &DroneFeed) -> Result<(), Error> {
    if !self.state.is_plotted() {
      return Err(Error::Unplotted("table"));
    }

    self.bg.iter().for_each(|r| feed.draw_rectangle(*r));
    feed.draw_grid(self.grid);
    self.textures.iter().for_each(|t| feed.draw_tex(*t));
    Ok(())
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

  /// Calculate area, occupied by the table. Returns `None` if origin
  /// hasn't been set or if state is worse then [State::Committed].
  pub fn area(&self) -> Option<Area> {
    let s = self.state.try_layout().ok()?.size().clone();
    self.origin.map(|o| Area::from_prim(o.to_point(&s), s))
  }

  pub fn cells(&self) -> Option<&Vec<Area>> {
    self.state.try_cells().ok()
  }

  pub fn columns(&self) -> usize {
    self.columns
  }
}

#[cfg(test)]
mod test {
  use crate::backend::Backend;

  use super::*;

  #[test]
  fn layout() {
    let offset = OFFSET * 2;
    let b = Backend::mock(200, 200);
    let desc_lock = b.desc.read().unwrap();

    let mut t = unsafe {
      TextTable::from_text(
        &desc_lock,
        &b.drone,
        2,
        2,
        CellStyle::Normal,
        &["abc", "ab", "a\na", "a"],
      )
      .unwrap()
    };

    t.set_origin(Origin::new(100, 100, crate::render::OriginPole::TopLeft));
    t.plot(&desc_lock, &b.drone.feed()).unwrap();
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

    t.update_cell(&desc_lock, &b.drone, 1, "abc\nabc").unwrap();
    t.plot(&desc_lock, &b.drone.feed()).unwrap();
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
      t.add_row(
        &desc_lock,
        &b.drone,
        vec!["abcd"; 2].into_iter(),
        CellStyle::Normal,
      )
      .unwrap();
      t.plot(&desc_lock, &b.drone.feed()).unwrap();
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
    let b = Backend::mock(200, 200);
    let desc_lock = b.desc.read().unwrap();

    let mut t = unsafe { TextTable::new(&b.drone, 0, 3) }.unwrap();
    assert!(matches!(t.state, State::None), "table starts with no state");

    assert!(
      matches!(t.plot(&desc_lock, &b.drone.feed()), Err(_)),
      "`plot` errors when called before commit"
    );

    unsafe {
      t.add_row(
        &desc_lock,
        &b.drone,
        vec![""; 3].into_iter(),
        CellStyle::Normal,
      )
      .unwrap();
      t.add_row(
        &desc_lock,
        &b.drone,
        vec![""; 3].into_iter(),
        CellStyle::Normal,
      )
      .unwrap();
    };
    assert!(
      matches!(t.state, State::None),
      "`add_row` resets state to none"
    );

    t.ensure_committed();
    assert!(
      matches!(t.state, State::Committed(_)),
      "committing works for no state and makes state committed"
    );

    t.truncate(3);
    assert!(
      matches!(t.state, State::None),
      "`truncate` resets state to none"
    );

    t.ensure_committed();
    assert_eq!(
      t.plot(&desc_lock, &b.drone.feed()),
      Err(Error::Uninitialized("origin")),
      "`plot` errors when origin isn't set"
    );
    assert!(
      matches!(t.state, State::Committed(_)),
      "`plot` called without origin doesn't reset state"
    );

    t.set_origin(Origin::new(0, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state, State::Committed(_)),
      "`set_origin` doesn't affect state when it's committed"
    );

    assert_eq!(
      t.plot(&desc_lock, &b.drone.feed()),
      Ok(()),
      "`plot` works when state is committed and origin is set"
    );

    assert!(
      matches!(t.state, State::Plotted(_, _)),
      "`plot` sets state to plotted"
    );

    t.ensure_committed();
    assert!(
      matches!(t.state, State::Plotted(_, _)),
      "`ensure_committed` is a no-op when state table is plotted"
    );

    assert_eq!(t.cells().unwrap().len(), 3, "`truncate` works");

    t.set_origin(Origin::new(0, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state, State::Plotted(_, _)),
      "`set_origin` is a no-op when origin remains the same after the call"
    );

    t.set_origin(Origin::new(100, 0, crate::render::OriginPole::TopLeft));
    assert!(
      matches!(t.state, State::Committed(_)),
      "`set_origin` downgrades state to committed from plotted"
    );

    t.plot(&desc_lock, &b.drone.feed()).unwrap();
    t.request_plot();
    assert!(
      matches!(t.state, State::Committed(_)),
      "`request_plot` downgrades state to committed from plotted"
    );
  }
}
