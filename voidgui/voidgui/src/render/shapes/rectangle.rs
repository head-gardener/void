use crate::render::{painter::Painter, shaders::Shader, shapes::*, Color};

#[derive(Debug)]
pub enum Style {
  Solid(Color),
  Lit(Color, Option<Area>),
}

pub struct Rectangle {
  style: Style,
  res: CommonRes,
}

impl Rectangle {
  pub unsafe fn new(style: Style) -> Self {
    let res = CommonRes::allocate();
    println!("style = {:?}", style);

    Self { style, res }
  }

  pub unsafe fn plot(
    &mut self,
    painter: &Painter,
    area: &Area,
  ) -> Result<(), String> {
    let norm = boxes_to_normalized(area, painter.window_area());
    let vertices = normalized_to_coords(norm);

    self.res.bind();
    self.res.bind_buffers();
    painter.common().bind_rect_ebo();

    match &mut self.style {
      Style::Solid(_) => {
        painter.shaders().common().enable_attribs();
      }
      Style::Lit(_, a) => {
        painter.shaders().grad().enable_attribs();
        *a = Some(area.clone());
      }
    }

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

  pub unsafe fn draw(&self, painter: &Painter) -> Result<(), String> {
    match self.style {
      Style::Solid(c) => {
        painter.shaders().common().set_used();
        painter.shaders().common().set_color(&c);
        Ok::<(), String>(())
      }
      Style::Lit(c, a) => {
        painter.shaders().grad().set_used();
        painter.shaders().grad().set_color(&c);
        painter
          .shaders()
          .grad()
          .set_constr(&a.ok_or("unplotted grad rectangle".to_string())?);
        Ok(())
      }
    }?;

    self.res.bind();
    gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

    Ok(())
  }

  pub fn set_style(&mut self, style: Style) {
    self.style = style;
  }
}
