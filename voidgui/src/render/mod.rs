pub mod painter;
pub mod shaders;
pub mod shapes;

pub type Color = (f32, f32, f32, f32);

#[derive(Debug)]
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
}
