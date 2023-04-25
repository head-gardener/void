use crate::{
  render::painter::SPainter,
  widgets::Spreadsheet, logic::Ring,
};

pub struct VoidWindow {
  hw_window: sdl2::video::Window,
  painter: SPainter,
  ring: Ring,
}

impl VoidWindow {
  pub unsafe fn new(hw_window: sdl2::video::Window) -> Self {
    let (width, height) = hw_window.size();
    let painter = SPainter::new(width as u16, height as u16);

    let ssheet =  Box::new(Spreadsheet::new(&painter).unwrap());
    let mut ring = Ring::new();
    ring.push(ssheet, crate::logic::ring::Mark::Spreadsheet);

    Self {
      hw_window,
      painter,
      ring,
    }
  }

  pub fn draw(&mut self) {
    self.ring.draw(&(self.painter));
  }

  pub fn swap(&self) {
    self.hw_window.gl_swap_window()
  }

  pub fn size(&self) -> (u32, u32) {
    self.hw_window.size()
  }
}
