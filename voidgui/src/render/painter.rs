use pangocairo::pango::FontDescription;

use super::{super::Area, shaders::Shaders};

pub struct SPainter {
  shaders: Shaders,
  common: CommonResources,
  window_area: Area,
  font: FontDescription,
}

pub trait Painter {
  fn shaders(&self) -> &Shaders;
  fn common(&self) -> &CommonResources;
  fn update_window_area(&mut self, width: u16, height: u16) -> ();
  fn window_area(&self) -> &Area;
  fn font(&self) -> &FontDescription;
}

impl SPainter {
  /// Creates a new [`Painter`].
  pub unsafe fn new(window_width: u16, window_height: u16) -> Self {
    let window_box = Area {
      x: 0,
      y: 0,
      width: window_width,
      height: window_height,
    };

    gl::Enable(gl::BLEND);
    gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

    Self {
      shaders: Shaders::new(),
      common: CommonResources::allocate(),
      window_area: window_box,
      font: FontDescription::from_string("Sans 18"),
    }
  }
}

impl Painter for SPainter {
  /// Creates a new [`Painter`].

  /// Returns a reference to the shaders of this [`Painter`].
  fn shaders(&self) -> &Shaders {
    &self.shaders
  }

  fn common(&self) -> &CommonResources {
    &self.common
  }

  fn update_window_area(&mut self, width: u16, height: u16) -> () {
    self.window_area.width = width;
    self.window_area.height = height;
  }

  fn window_area(&self) -> &Area {
    &self.window_area
  }

  fn font(&self) -> &FontDescription {
    &self.font
  }
}

#[cfg(test)]
pub struct MockPainter {}

#[cfg(test)]
impl MockPainter {
  pub fn new() -> Self {
    Self {}
  }
}

#[cfg(test)]
impl Painter for MockPainter {
  fn shaders(&self) -> &Shaders {
    todo!()
  }

  fn common(&self) -> &CommonResources {
    todo!()
  }

  fn update_window_area(&mut self, width: u16, height: u16) -> () {
    todo!()
  }

  fn window_area(&self) -> &Area {
    todo!()
  }

  fn font(&self) -> &FontDescription {
    todo!()
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

  fn mock() -> CommonResources {
    todo!()
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
