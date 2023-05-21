use crate::render::{Area, Point, Size};

/// Given a bunch of sizes this structure conatins information necessary for
/// putting them in a grid. This is achieved in two stages:
/// - Generating a layout fills in position independent fields like sizes and
/// ratios. Done by `from_sizes`.
/// - Plotting a layout maps the data to actual coordinates using an origin
/// point. Done by `plot`.
///
/// A laoyout can be plotted multiple times but can't be updated, only
/// regenerated from scratch.
#[derive(Debug)]
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
    assert_eq!(sizes.len(), r * c);

    let rows: Vec<i32> = sizes
      .chunks(c)
      .map(|ss| ss.iter().map(|s| s.height).max().unwrap_or(0))
      .collect();
    let columns: Vec<i32> = (0..c)
      .map(|rem| {
        let (_, xs) = sizes.split_at(rem);
        xs.chunks(c)
          .map(|c| c.first().unwrap().width)
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
  pub fn plot(&self, origin: &Point, scale: f32) -> Vec<Area> {
    let mut y = Box::new(origin.y);
    self
      .layout
      .chunks(self.columns)
      .map(move |ss| {
        let res = {
          let y = *y;
          let mut x = Box::new(origin.x);
          ss.iter().map(move |s| {
            let a = Area::new(*x, y, s.width, s.height).scale(scale);
            *x += a.width;
            a
          })
        };
        *y += ss.get(0).map_or(0, |s| (s.height as f32 * scale) as i32);
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
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn horizontal_menu() {
    let a = vec![Size::new(100, 40), Size::new(200, 30), Size::new(50, 50)];
    let b = vec![
      Area::new(0, 0, 100, 50),
      Area::new(100, 0, 200, 50),
      Area::new(300, 0, 50, 50),
    ];

    let l = Layout::from_sizes(1, 3, a.as_slice());
    assert_eq!(l.plot(&Point::new(0, 0), 1.0), b);
  }

  #[test]
  fn vertical_menu() {
    let a = vec![Size::new(100, 40), Size::new(200, 30), Size::new(50, 50)];
    let b = vec![
      Area::new(0, 0, 200, 40),
      Area::new(0, 40, 200, 30),
      Area::new(0, 70, 200, 50),
    ];

    let l = Layout::from_sizes(3, 1, a.as_slice());
    assert_eq!(l.plot(&Point::new(0, 0), 1.0), b);
  }

  #[test]
  fn square_menu() {
    let a = vec![
      Size::new(100, 40),
      Size::new(200, 30),
      Size::new(50, 50),
      Size::new(150, 60),
    ];
    let b = vec![
      Area::new(0, 0, 100, 40),
      Area::new(100, 0, 200, 40),
      Area::new(0, 40, 100, 60),
      Area::new(100, 40, 200, 60),
    ];

    let l = Layout::from_sizes(2, 2, a.as_slice());
    assert_eq!(l.plot(&Point::new(0, 0), 1.0), b);
  }

  #[test]
  fn scale() {
    let a = vec![
      Size::new(100, 40),
      Size::new(200, 30),
      Size::new(50, 50),
      Size::new(150, 60),
    ];
    let b = vec![
      Area::new(0, 0, 200, 80),
      Area::new(200, 0, 400, 80),
      Area::new(0, 80, 200, 120),
      Area::new(200, 80, 400, 120),
    ];
    let c = vec![
      Area::new(0, 0, 50, 20),
      Area::new(50, 0, 100, 20),
      Area::new(0, 20, 50, 30),
      Area::new(50, 20, 100, 30),
    ];

    let l = Layout::from_sizes(2, 2, a.as_slice());
    assert_eq!(l.plot(&Point::new(0, 0), 2.0), b);
    assert_eq!(l.plot(&Point::new(0, 0), 0.5), c);
  }

  #[test]
  fn offset() {
    let a = vec![Size::new(100, 40), Size::new(200, 30)];
    let b = vec![Area::new(100, 50, 100, 40), Area::new(200, 50, 200, 40)];

    let l = Layout::from_sizes(1, 2, a.as_slice());
    assert_eq!(l.plot(&Point::new(100, 50), 1.0), b);
  }
}
