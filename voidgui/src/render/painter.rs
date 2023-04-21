use super::{shaders::Shaders, super::Area};

pub struct Painter {
  shaders: Shaders,
  common: CommonResources,
  window_area: Area,
}

impl Painter {
  /// Creates a new [`Painter`].
  pub fn new(window_width: u16, window_height: u16) -> Self {
    let window_box = Area {
      x: 0,
      y: 0,
      width: window_width,
      height: window_height,
    };
    Self {
      shaders: Shaders::new(),
      common: CommonResources::allocate(),
      window_area: window_box,
    }
  }

  /// Returns a reference to the shaders of this [`Painter`].
  pub fn shaders(&self) -> &Shaders {
    &self.shaders
  }

  pub fn common(&self) -> &CommonResources {
    &self.common
  }

  pub fn update_window_area(&mut self, width: u16, height: u16) -> () {
    self.window_area_mut().width = width;
    self.window_area_mut().height = height;
  }

  pub fn window_area(&self) -> &Area {
    &self.window_area
  }

  fn window_area_mut(&mut self) -> &mut Area {
    &mut self.window_area
  }
}

pub struct CommonResources {
  rect_ebo: gl::types::GLuint,
}

impl CommonResources {
  pub fn allocate() -> Self {
    let mut rect_ebo: gl::types::GLuint = 0;
    let elements: Vec<i32> = vec![0, 1, 2, 2, 3, 0];
    unsafe {
      gl::GenBuffers(1, &mut rect_ebo);
      gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, rect_ebo);
      gl::BufferData(
        gl::ELEMENT_ARRAY_BUFFER,
        (elements.len() * std::mem::size_of::<i32>()) as gl::types::GLsizeiptr,
        elements.as_ptr() as *const gl::types::GLvoid,
        gl::STATIC_DRAW,
      );
    }
    Self { rect_ebo }
  }

  pub fn bind_rect_ebo(&self) -> () {
    unsafe {
      gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.rect_ebo);
    }
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
