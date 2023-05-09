use pangocairo::pango::FontDescription;

use super::{shaders::Shaders, Area};

#[cfg(not(test))]
pub struct Painter {
  shaders: Shaders,
  common: CommonResources,
  window_area: Area,
  font: FontDescription,
}

#[cfg(not(test))]
impl Painter {
  /// Creates a new [`Painter`].
  pub unsafe fn new(window_width: u16, window_height: u16) -> Self {
    let window_area = Area {
      x: 0,
      y: 0,
      width: window_width,
      height: window_height,
    };

    Self {
      shaders: Shaders::new(),
      common: CommonResources::allocate(),
      window_area,
      font: FontDescription::from_string("Sans 18"),
    }
  }

  pub fn resize(&mut self, w: u16, h: u16) {
    self.window_area.width = w;
    self.window_area.height = h;
  }

  /// Returns a reference to the shaders of this [`Painter`].
  pub fn shaders(&self) -> &Shaders {
    &self.shaders
  }

  pub fn common(&self) -> &CommonResources {
    &self.common
  }

  pub fn window_area(&self) -> &Area {
    &self.window_area
  }

  pub fn font(&self) -> &FontDescription {
    &self.font
  }
}

#[cfg(test)]
pub struct Painter {
  font: FontDescription,
}

#[cfg(test)]
impl Painter {
  pub fn new(_: u16, _: u16) -> Self {
    Self {
      font: FontDescription::default(),
    }
  }

  pub fn resize(&mut self, w: u16, h: u16) {
    todo!();
  }

  /// Returns a reference to the shaders of this [`Painter`].
  pub fn shaders(&self) -> &Shaders {
    todo!();
  }

  pub fn common(&self) -> &CommonResources {
    todo!();
  }

  pub fn update_window_area(&mut self, width: u16, height: u16) -> () {
    todo!();
  }

  pub fn window_area(&self) -> &Area {
    todo!();
  }

  pub fn font(&self) -> &FontDescription {
    &self.font
  }
}

pub struct CommonResources {
  rect_ebo: gl::types::GLuint,
}

impl CommonResources {
  pub unsafe fn allocate() -> Self {
    let mut rect_ebo: gl::types::GLuint = 0;
    let elements: Vec<i32> = vec![0, 1, 2, 2, 3, 0];
    gl::GenBuffers(1, &mut rect_ebo);
    gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, rect_ebo);
    gl::BufferData(
      gl::ELEMENT_ARRAY_BUFFER,
      (elements.len() * std::mem::size_of::<i32>()) as gl::types::GLsizeiptr,
      elements.as_ptr() as *const gl::types::GLvoid,
      gl::STATIC_DRAW,
    );
    Self { rect_ebo }
  }

  pub unsafe fn bind_rect_ebo(&self) -> () {
    gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.rect_ebo);
  }
}

impl Drop for CommonResources {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteBuffers(1, &self.rect_ebo);
    }
  }
}

pub fn pop_gl_error() {
  let error = unsafe { gl::GetError() };
  if error != gl::NO_ERROR {
    println!("Gl error: {}", error);
  }
}
