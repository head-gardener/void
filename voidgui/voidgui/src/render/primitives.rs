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
  pub x: u16,
  pub y: u16,
  pub pole: OriginPole,
}

impl Origin {
  pub fn new(x: u16, y: u16, pole: OriginPole) -> Self {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {
  pub x: u16,
  pub y: u16,
}

impl Point {
  pub fn new(x: u16, y: u16) -> Self {
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
  pub width: u16,
  pub height: u16,
}

impl Size {
  pub fn new(width: u16, height: u16) -> Self {
    Self { width, height }
  }

  pub fn expand(&self, vert: u16, horz: u16) -> Size {
    Size {
      width: self.width + vert * 2,
      height: self.height + horz * 2,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Area {
  pub x: u16,
  pub y: u16,
  pub width: u16,
  pub height: u16,
}

impl Area {
  pub fn new(x: u16, y: u16, width: u16, height: u16) -> Self {
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
}

#[cfg(test)]
mod test {
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
  fn area() {
    let a = Area::new(50, 10, 100, 60);
    let p = Point::new(50, 10);
    let s = Size::new(100, 60);
    assert_eq!(Area::from_prim(p, s), a);
    assert_eq!(a.to_prim(), (p, s));
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
