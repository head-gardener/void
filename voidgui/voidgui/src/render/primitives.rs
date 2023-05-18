pub type Color = (f32, f32, f32, f32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OriginPole {
  TopLeft,
  TopRight,
  BottomLeft,
  BottomRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Origin {
  pub x: i32,
  pub y: i32,
  pub pole: OriginPole,
}

impl Origin {
  pub fn new(x: i32, y: i32, pole: OriginPole) -> Self {
    Self { x, y, pole }
  }

  pub fn from_point(p: Point, pole: OriginPole) -> Self {
    Self {
      x: p.x,
      y: p.y,
      pole,
    }
  }

  pub fn to_point(&self, a: &Size) -> Point {
    match self.pole {
      OriginPole::TopLeft => Point::new(self.x, self.y),
      OriginPole::TopRight => Point::new(self.x - a.width, self.y),
      OriginPole::BottomLeft => Point::new(self.x, self.y - a.height),
      OriginPole::BottomRight => {
        Point::new(self.x - a.width, self.y - a.height)
      }
    }
  }

  pub fn scroll(&self, y: i32) -> Self {
    Self {
      x: self.x,
      y: self.y + y,
      pole: self.pole,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {
  pub x: i32,
  pub y: i32,
}

impl Point {
  pub fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }

  pub fn contained(&self, a: &Area) -> bool {
    self.x >= a.x
      && self.y >= a.y
      && self.x <= a.x + a.width
      && self.y <= a.y + a.height
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Size {
  pub width: i32,
  pub height: i32,
}

impl Size {
  pub fn new(width: i32, height: i32) -> Self {
    Self { width, height }
  }

  pub fn expand(&self, vert: i32, horz: i32) -> Size {
    Size {
      width: self.width + vert * 2,
      height: self.height + horz * 2,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Area {
  pub x: i32,
  pub y: i32,
  pub width: i32,
  pub height: i32,
}

impl Area {
  pub fn new(x: i32, y: i32, width: i32, height: i32) -> Self {
    Self {
      x,
      y,
      width,
      height,
    }
  }

  pub fn from_prim(p: Point, s: Size) -> Self {
    Self {
      x: p.x,
      y: p.y,
      width: s.width,
      height: s.height,
    }
  }

  pub fn to_prim(&self) -> (Point, Size) {
    (
      Point {
        x: self.x,
        y: self.y,
      },
      Size {
        width: self.width,
        height: self.height,
      },
    )
  }

  #[inline]
  pub fn to_normalized(&self, outer: &Area) -> NormalizedArea {
    let half_width = outer.width as f32 / 2.0;
    let half_height = outer.height as f32 / 2.0;
    let o_x = outer.x as f32 + half_width;
    let o_y = outer.y as f32 + half_height;
    let a_x = self.x as f32;
    let a_y = self.y as f32;
    let b_x = (self.x + self.width) as f32;
    let b_y = (self.y + self.height) as f32;

    NormalizedArea::new(
      (a_x - o_x) / half_width,
      -(a_y - o_y) / half_height,
      (b_x - o_x) / half_width,
      -(b_y - o_y) / half_height,
    )
  }

  pub fn gridify(
    &self,
    outer: &Area,
    rows: usize,
    columns: usize,
    row_ratio: &[f32],
    column_ratio: &[f32],
  ) -> Vec<f32> {
    let norm = self.to_normalized(outer);
    let vert_points = section(norm.a_y, norm.b_y, rows + 1, row_ratio);
    let horiz_points = section(norm.a_x, norm.b_x, columns + 1, column_ratio);

    let mut vertices = vec![];
    for i in 0..=rows {
      vertices.append(&mut vec![
        horiz_points[0],
        vert_points[i],
        horiz_points[columns],
        vert_points[i],
      ]);
    }
    for i in 0..=columns {
      vertices.append(&mut vec![
        horiz_points[i],
        vert_points[0],
        horiz_points[i],
        vert_points[rows],
      ]);
    }

    vertices
  }

  pub fn collect_row(areas: &[Area]) -> Self {
    Self {
      x: areas[0].x,
      y: areas[0].y,
      width: areas.iter().map(|a| a.width).sum(),
      height: areas[0].height,
    }
  }

  pub fn expand(&self, arg: i32) -> Area {
    Self {
      x: self.x - arg,
      y: self.y - arg,
      width: self.width + arg * 2,
      height: self.height + arg * 2,
    }
  }
}

pub struct NormalizedArea {
  pub a_x: f32,
  pub a_y: f32,
  pub b_x: f32,
  pub b_y: f32,
}

impl NormalizedArea {
  pub fn new(a_x: f32, a_y: f32, b_x: f32, b_y: f32) -> Self {
    Self { a_x, a_y, b_x, b_y }
  }
}

pub fn section(a: f32, b: f32, n: usize, ratios: &[f32]) -> Vec<f32> {
  let mut points = Vec::<f32>::with_capacity(n);
  let d = (b - a) as f32;
  points.push(a);
  for i in 1..n {
    points.push(points[i - 1] + d * ratios[i - 1]);
  }
  points
}

impl NormalizedArea {
  #[inline]
  pub fn to_coords(self) -> [f32; 8] {
    [
      self.a_x, self.a_y, // top-left
      self.b_x, self.a_y, // top-right
      self.b_x, self.b_y, // bottom-right
      self.a_x, self.b_y, // bottom-left
    ]
  }

  #[inline]
  pub fn to_tex_coords(self) -> [f32; 16] {
    [
      self.a_x, self.a_y, 0.0, 0.0, // tl
      self.b_x, self.a_y, 1.0, 0.0, // tr
      self.b_x, self.b_y, 1.0, 1.0, // br
      self.a_x, self.b_y, 0.0, 1.0, // bl
    ]
  }
}

#[cfg(test)]
mod test_area {
  use super::*;

  #[test]
  fn to_from_prim() {
    let a = Area::new(50, 10, 100, 60);
    let p = Point::new(50, 10);
    let s = Size::new(100, 60);
    assert_eq!(Area::from_prim(p, s), a);
    assert_eq!(a.to_prim(), (p, s));
  }

  #[test]
  fn expand() {
    let a = Area::new(50, 10, 100, 60);
    assert_eq!(a.expand(2), Area::new(48, 8, 104, 64));
  }
}

#[cfg(test)]
mod test_primitives {
  use super::*;

  #[test]
  fn point() {
    let a = Area::new(50, 10, 100, 60);

    assert!(!Point::new(0, 0).contained(&a));
    assert!(!Point::new(51, 9).contained(&a));
    assert!(!Point::new(49, 11).contained(&a));
    assert!(!Point::new(151, 69).contained(&a));
    assert!(!Point::new(149, 71).contained(&a));
    assert!(!Point::new(170, 80).contained(&a));

    assert!(Point::new(60, 20).contained(&a));
    assert!(Point::new(60, 65).contained(&a));
    assert!(Point::new(145, 20).contained(&a));
    assert!(Point::new(145, 66).contained(&a));
  }

  #[test]
  fn size() {
    assert_eq!(Size::new(100, 60).expand(50, 10), Size::new(200, 80));
  }

  #[test]
  fn origin() {
    let s = Size::new(100, 60);
    let p = Point::new(200, 100);

    let tl = Origin::from_point(p, OriginPole::TopLeft);
    let tr = Origin::from_point(p, OriginPole::TopRight);
    let bl = Origin::from_point(p, OriginPole::BottomLeft);
    let br = Origin::from_point(p, OriginPole::BottomRight);

    assert_eq!(tl.to_point(&s), Point::new(200, 100));
    assert_eq!(tr.to_point(&s), Point::new(100, 100));
    assert_eq!(bl.to_point(&s), Point::new(200, 40));
    assert_eq!(br.to_point(&s), Point::new(100, 40));
  }
}
