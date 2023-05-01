pub type Color = (f32, f32, f32, f32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point {
  pub x: u16,
  pub y: u16,
}

impl Point {
  pub fn new(x: u16, y: u16) -> Self {
    Self {
      x,
      y,
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Size {
  pub width: u16,
  pub height: u16,
}

impl Size {
  pub fn new(width: u16, height: u16) -> Self {
    Self {
      width,
      height,
    }
  }
  
  pub fn expand(&mut self, vert: u16, horz: u16) -> () {
    self.width += vert * 2;
    self.height += horz * 2;
  }
}

#[derive(Debug, Clone, Copy)]
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
}
