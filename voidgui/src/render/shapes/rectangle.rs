use crate::render::painter::Painter;
use crate::render::shaders::Shader;
use crate::render::{shapes::*, Color};
use crate::Area;

pub struct Rectangle {
  color: Color,
  res: CommonRes,
}

impl Rectangle {
  pub fn new(color: Color) -> Self {
    let res = CommonRes::allocate();
    Self { color, res }
  }

  pub fn plot(&self, painter: &Painter, area: &Area) -> () {
    self.res.bind_buffers();
    let norm = boxes_to_normalized(area, painter.window_area());
    let vertices = normalized_to_coords(norm);

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
