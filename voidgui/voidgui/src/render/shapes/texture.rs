use pangocairo::cairo::ffi::cairo_image_surface_create;
use pangocairo::cairo::Format::ARgb32;
use pangocairo::pango::FontDescription;

use crate::render::Size;

use super::common_res::CommonRes;

pub struct Texture {
  pub res: CommonRes,
  pub texture: Option<gl::types::GLuint>,
  // pub size: Size,
}

impl Texture {
  pub unsafe fn new() -> Self {
    let res = CommonRes::allocate();
    Self {
      res,
      texture: Option::None,
      // size: Size::new(0, 0),
    }
  }
}

// #[cfg(test)]
// unsafe fn get_text(_: &Drone, s: &str) -> Result<(i32, i32, Vec<u8>), String> {
//   Ok((
//     s.lines().map(|l| l.len()).max().unwrap_or_default() as i32,
//     s.lines().count() as i32,
//     vec![],
//   ))
// }

impl Drop for Texture {
  fn drop(&mut self) {
    self.texture.map(|s| unsafe { gl::DeleteTextures(1, &s) });
  }
}

pub struct TextureData(pub i32, pub i32, pub Vec<u8>);

impl TextureData {
  pub unsafe fn from_text(
    font: &FontDescription,
    s: &str,
  ) -> Result<Self, String> {
    let tmp_surface = pangocairo::cairo::Surface::from_raw_full(
      cairo_image_surface_create(i32::from(ARgb32), 0, 0),
    )
    .map_err(|e| e.to_string())?;
    let layout_context = pangocairo::cairo::Context::new(tmp_surface)
      .map_err(|e| e.to_string())?;

    let layout = pangocairo::create_layout(&layout_context);
    layout.set_text(s);
    layout.set_font_description(Some(&font));
    let (w, h) = layout.pixel_size();

    let mut data: Vec<u8> = vec![0; (w * h * 4) as usize];
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
    Ok(Self(w, h, data))
  }
}

impl Into<Size> for &TextureData {
  fn into(self) -> Size {
    Size {
      width: self.0 as u16,
      height: self.1 as u16,
    }
  }
}
