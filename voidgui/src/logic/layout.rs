use crate::render::{Size, Point, Area};


/// Given a bunch of sizes this structure conatins information necessary for
/// putting them in a grid. This is achieved in two stages:
/// - Generating a layout fills in position independent fields like sizes and
/// ratios. Done by `from_sizes`.
/// - Plotting a layout maps the data to actual coordinates using an origin
/// point. Done by `plot`.
///
/// A laoyout can be plotted multiple times but can't be updated, only
/// regenerated from scratch.
pub struct Layout {
  row_ratio: Vec<f32>,
  column_ratio: Vec<f32>,
  rows: usize,
  columns: usize,
  size: Size,
  layout: Vec<Size>,
}

impl Layout {
  /// Generate an unplotted layout from sizes.
  pub fn from_sizes(r: usize, c: usize, sizes: &[Size]) -> Self {
    assert_eq!(sizes.len(), r*c);

    let rows: Vec<u16> = sizes
      .chunks(c)
      .map(|ss| ss.iter().map(|s| s.height).max().unwrap_or(0))
      .collect();
    let columns: Vec<u16> = (0..c)
      .map(|rem| {
        let (_, xs) = sizes.split_at(rem);
        xs.chunks(r)
          .map(|c| c.first().map_or(0, |s| s.width))
          .max()
          .unwrap_or(0)
      })
      .collect();

    let h = rows.iter().sum();
    let w = columns.iter().sum();

    let row_ratio = rows.iter().map(|x| *x as f32 / (h as f32)).collect();
    let column_ratio = columns.iter().map(|x| *x as f32 / (w as f32)).collect();

    let layout = rows
      .iter()
      .map(|h| columns.iter().map(|w| Size::new(*w, *h)))
      .flatten()
      .collect();

    Self {
      row_ratio,
      column_ratio,
      rows: r,
      columns: c,
      size: Size::new(w, h),
      layout,
    }
  }

  /// Generate areas from a layout and an origin point.
  pub fn plot(&self, origin: &Point) -> Vec<Area> {
    let mut y = Box::new(origin.y);
    self
      .layout
      .chunks(self.columns)
      .map(move |ss| {
        let res = {
          let y = *y;
          let mut x = Box::new(origin.x);
          ss.iter().map(move |s| {
            let a = Area::new(*x, y, s.width, s.height);
            *x += a.width;
            a
          })
        };
        *y += ss.get(0).map_or(0, |s| s.height);
        res
      })
      .flatten()
      .collect()
  }

  pub fn row_ratio(&self) -> &[f32] {
    self.row_ratio.as_ref()
  }

  pub fn column_ratio(&self) -> &[f32] {
    self.column_ratio.as_ref()
  }

  pub fn rows(&self) -> usize {
    self.rows
  }

  pub fn columns(&self) -> usize {
    self.columns
  }

  pub fn size(&self) -> Size {
    self.size
  }

  pub fn layout(&self) -> &[Size] {
    self.layout.as_ref()
  }
}
