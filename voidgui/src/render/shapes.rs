use super::{painter::Painter, shaders::Shader};

pub type Color = (f32, f32, f32, f32);

pub struct Rectangle {
  color: Color,
  res: CommonRes,
}

impl Rectangle {
  pub fn new(color: Color) -> Self {
    let res = CommonRes::allocate();
    Self { color, res }
  }

  pub fn plot(&self, painter: &Painter) -> () {
    self.res.bind_buffers();
    let vertices: Vec<f32> = vec![-0.5, 0.5, 0.5, 0.5, 0.5, -0.5, -0.5, -0.5];

    unsafe {
      gl::BufferData(
        gl::ARRAY_BUFFER,
        (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
        vertices.as_ptr() as *const gl::types::GLvoid,
        gl::STATIC_DRAW,
      );
      gl::BindBuffer(gl::ARRAY_BUFFER, 0);
    }

    self.res.bind();
    self.res.bind_buffers();
    painter.common().bind_rect_ebo();
    painter.shaders().common().enable_attribs();
    unsafe {
      gl::VertexAttribPointer(
        painter.shaders().common().pos().id() as u32,
        2,
        gl::FLOAT,
        gl::FALSE,
        0,
        std::ptr::null(),
      );
      gl::BindVertexArray(0);
    }
  }

  pub fn draw(&self, painter: &Painter) -> () {
    painter.shaders().common().set_used();
    painter.shaders().common().set_color(&self.color);
    self.res.bind();
    unsafe {
      gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());
    }
  }
}

struct CommonRes {
  vao: gl::types::GLuint,
  vbo: gl::types::GLuint,
}

impl CommonRes {
  pub fn allocate() -> Self {
    let mut vao: gl::types::GLuint = 0;
    let mut vbo: gl::types::GLuint = 0;
    unsafe {
      gl::GenVertexArrays(1, &mut vao);
      gl::GenBuffers(1, &mut vbo);
    }
    Self { vao, vbo }
  }

  pub fn bind(&self) -> () {
    unsafe {
      gl::BindVertexArray(self.vao);
    }
  }

  pub fn bind_buffers(&self) -> () {
    unsafe {
      gl::BindVertexArray(self.vao);
      gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
    }
  }
}

impl Drop for CommonRes {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteVertexArrays(1, &self.vao);
      gl::DeleteBuffers(1, &self.vbo);
    }
  }
}
