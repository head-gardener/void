use pangocairo::pango::FontDescription;

use crate::render::Area;

#[derive(PartialEq, Eq, Debug)]
pub enum Mode {
  Normal,
  Delete,
}

pub struct Description {
  mode: Mode,
  window_area: Area,
  font: FontDescription,
}

impl Description {
  /// Creates a new [`Description`].
  pub fn new(window_width: i32, window_height: i32) -> Self {
    let window_area = Area {
      x: 0,
      y: 0,
      width: window_width,
      height: window_height,
    };

    Self {
      mode: Mode::Normal,
      window_area,
      font: FontDescription::from_string("Sans 18"),
    }
  }

  pub fn resize(&mut self, w: i32, h: i32) {
    self.window_area.width = w;
    self.window_area.height = h;
  }

  pub fn window_area(&self) -> &Area {
    &self.window_area
  }

  pub fn font(&self) -> &FontDescription {
    &self.font
  }

  pub fn mode(&self) -> &Mode {
    &self.mode
  }

  pub fn set_mode(&mut self, mode: Mode) {
    self.mode = mode;
  }
}
