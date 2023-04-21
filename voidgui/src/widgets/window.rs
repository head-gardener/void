use crate::render::painter::Painter;

pub struct VoidWindow {
  hw_window: sdl2::video::Window,
  painter: Painter,
}

impl VoidWindow {
  pub fn new(hw_window: sdl2::video::Window) -> Self {
    Self {
      hw_window,
      painter: Painter::new(),
    }
  }

  pub fn swap(&self) {
    self.hw_window.gl_swap_window()
  }

  pub fn size(&self) -> (u32, u32) {
    self.hw_window.size()
  }

  pub fn painter(&self) -> &Painter {
    &self.painter
  }
}
