use super::{boxes_to_normalized, section, CommonRes};
use crate::render::{painter::Painter, shaders::Shader, Area, Color};

pub struct Grid {
  color: Color,
  res: CommonRes,
}

impl Grid {
  pub fn new(color: Color) -> Self {
    let res = CommonRes::allocate();
    Self { color, res }
  }

  pub fn plot(
    &self,
    painter: &Painter,
    rows: usize,
    columns: usize,
    row_ratio: &[f32],
    column_ratio: &[f32],
    area: &Area,
  ) -> () {
    self.res.bind_buffers();
    let norm = boxes_to_normalized(area, painter.window_area());
    let vert_points = section(norm.a_y, norm.b_y, rows + 1, row_ratio);
    let horiz_points = section(norm.a_x, norm.b_x, columns + 1, column_ratio);

    let mut vertices = vec![];
    for i in 0..rows + 1 {
      vertices.append(&mut vec![
        horiz_points[0],
        vert_points[i],
        horiz_points[columns],
        vert_points[i],
      ]);
    }
    for i in 0..columns + 1 {
      vertices.append(&mut vec![
        horiz_points[i],
        vert_points[0],
        horiz_points[i],
        vert_points[rows],
      ]);
    }

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
      gl::DrawArrays(gl::LINES, 0, 16);
    }
  }
}
