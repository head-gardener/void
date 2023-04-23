use pangocairo::cairo::ffi::cairo_image_surface_create;
use pangocairo::cairo::Format::ARgb32;

use crate::render::{painter::Painter, shaders::Shader, shapes::*};
use crate::Area;

pub struct Texture {
  res: CommonRes,
  texture: Option<gl::types::GLuint>,
}

impl Texture {
  pub unsafe fn new() -> Self {
    let res = CommonRes::allocate();
    Self {
      res,
      texture: Option::None,
    }
  }

  pub unsafe fn bind_text(
    &mut self,
    _painter: &Painter,
    text: &str,
  ) -> Result<(), String> {
    let tmp_surface = pangocairo::cairo::Surface::from_raw_full(
      cairo_image_surface_create(i32::from(ARgb32), 0, 0),
    )
    .map_err(|e| e.to_string())?;
    let layout_context = pangocairo::cairo::Context::new(tmp_surface)
      .map_err(|e| e.to_string())?;

    let desc = pangocairo::pango::FontDescription::from_string("Sans Bold 30");
    let layout = pangocairo::create_layout(&layout_context);
    layout.set_text(text);
    layout.set_font_description(Some(&desc));
    let (w, h) = layout.pixel_size();

    let mut data: Vec<u8> = Vec::with_capacity((w * h * 4) as usize);
    let out_surface = pangocairo::cairo::ImageSurface::create_for_data_unsafe(
      data.as_mut_ptr(),
      ARgb32,
      w,
      h,
      4 * w,
    )
    .map_err(|e| e.to_string())?;
    let render_context = pangocairo::cairo::Context::new(out_surface)
      .map_err(|e| e.to_string())?;
    render_context.set_source_rgba(0.3, 0.3, 0.3, 1.0);
    pangocairo::show_layout(&render_context, &layout);

    self.texture.map(|s| gl::DeleteTextures(1, &s));
    let mut t: gl::types::GLuint = 0;
    gl::GenTextures(1, &mut t);
    self.texture = Option::Some(t);

    self.res.bind();
    gl::BindTexture(gl::TEXTURE_2D, t);
    gl::TexParameteri(
      gl::TEXTURE_2D,
      gl::TEXTURE_MIN_FILTER,
      gl::LINEAR as i32,
    );
    gl::TexParameteri(
      gl::TEXTURE_2D,
      gl::TEXTURE_MAG_FILTER,
      gl::LINEAR as i32,
    );
    gl::TexImage2D(
      gl::TEXTURE_2D,
      0,
      gl::RGBA as i32,
      w,
      h,
      0,
      gl::BGRA,
      gl::UNSIGNED_BYTE,
      data.as_mut_ptr() as *const gl::types::GLvoid,
    );
    gl::BindVertexArray(0);

    Ok(())
  }

  pub unsafe fn plot(&self, painter: &Painter, area: &Area) -> () {
    let norm = boxes_to_normalized(area, painter.window_area());
    let vertices = tex_vertices(&norm);

    self.res.bind();
    self.res.bind_buffers();
    painter.common().bind_rect_ebo();
    painter.shaders().tex().enable_attribs();
    gl::BufferData(
      gl::ARRAY_BUFFER,
      (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
      vertices.as_ptr() as *const gl::types::GLvoid,
      gl::STATIC_DRAW,
    );
    gl::VertexAttribPointer(
      painter.shaders().tex().pos().id() as u32,
      2,
      gl::FLOAT,
      gl::FALSE,
      (4 * std::mem::size_of::<f32>()) as i32,
      std::ptr::null() as *const gl::types::GLvoid,
    );
    gl::VertexAttribPointer(
      painter.shaders().tex().tex().id() as u32,
      2,
      gl::FLOAT,
      gl::FALSE,
      (4 * std::mem::size_of::<f32>()) as i32,
      (2 * std::mem::size_of::<f32>()) as *const gl::types::GLvoid,
    );
    gl::BindVertexArray(0);
  }

  pub unsafe fn draw(&self, painter: &Painter) -> () {
    painter.shaders().tex().set_used();
    self.res.bind();
    gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());
  }

  pub fn texture(&self) -> Option<u32> {
    self.texture
  }
}

fn tex_vertices(norm: &NormalizedArea) -> Vec<f32> {
  vec![
    norm.a_x, norm.a_y, 0.0, 0.0, // tl
    norm.b_x, norm.a_y, 1.0, 0.0, // tr
    norm.b_x, norm.b_y, 1.0, 1.0, // br
    norm.a_x, norm.b_y, 0.0, 1.0, // bl
  ]
}

impl Drop for Texture {
  fn drop(&mut self) {
    self.texture().map(|s| unsafe { gl::DeleteTextures(1, &s) });
  }
}
