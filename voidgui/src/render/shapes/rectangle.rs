use crate::render::{painter::Painter, shaders::Shader, shapes::*, Color};
use crate::Area;

pub struct Rectangle {
  color: Color,
  res: CommonRes,
}

impl Rectangle {
  pub unsafe fn new(color: Color) -> Self {
    let res = CommonRes::allocate();
    Self { color, res }
  }

  pub unsafe fn plot(
    &self,
    painter: &dyn Painter,
    area: &Area,
  ) -> Result<(), String> {
    let norm = boxes_to_normalized(area, painter.window_area());
    let vertices = normalized_to_coords(norm);

    self.res.bind();
    self.res.bind_buffers();
    painter.common().bind_rect_ebo();
    painter.shaders().common().enable_attribs();

    gl::BufferData(
      gl::ARRAY_BUFFER,
      (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
      vertices.as_ptr() as *const gl::types::GLvoid,
      gl::STATIC_DRAW,
    );

    gl::VertexAttribPointer(
      painter.shaders().common().pos().id() as u32,
      2,
      gl::FLOAT,
      gl::FALSE,
      0,
      std::ptr::null(),
    );
    gl::BindVertexArray(0);

    Ok(())
  }

  pub unsafe fn draw(&self, painter: &dyn Painter) -> Result<(), String> {
    painter.shaders().common().set_used();
    painter.shaders().common().set_color(&self.color);
    self.res.bind();
    gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

    Ok(())
  }
}
