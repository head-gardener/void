use super::Area;

mod grid;
mod rectangle;
mod texture;

pub use grid::Grid;
pub use rectangle::Rectangle;
pub use texture::Texture;

struct NormalizedArea {
  pub a_x: f32,
  pub a_y: f32,
  pub b_x: f32,
  pub b_y: f32,
}

impl NormalizedArea {
  pub fn new(a_x: f32, a_y: f32, b_x: f32, b_y: f32) -> Self {
    Self { a_x, a_y, b_x, b_y }
  }
}

fn boxes_to_normalized(inner: &Area, outer: &Area) -> NormalizedArea {
  let half_width = outer.width as f32 / 2.0;
  let half_height = outer.height as f32 / 2.0;
  let o_x = outer.x as f32 + half_width;
  let o_y = outer.y as f32 + half_height;
  let a_x = inner.x as f32;
  let a_y = inner.y as f32;
  let b_x = (inner.x + inner.width) as f32;
  let b_y = (inner.y + inner.height) as f32;

  NormalizedArea::new(
    (a_x - o_x) / half_width,
    -(a_y - o_y) / half_height,
    (b_x - o_x) / half_width,
    -(b_y - o_y) / half_height,
  )
}

fn section(a: f32, b: f32, n: usize, ratios: &[f32]) -> Vec<f32> {
  let mut points = Vec::<f32>::with_capacity(n);
  let d = (b - a) as f32;
  points.push(a);
  for i in 1..n {
    points.push(points[i - 1] + d * ratios[i - 1]);
  }
  points
}

fn normalized_to_coords(norm: NormalizedArea) -> Vec<f32> {
  vec![
    norm.a_x, norm.a_y, // top-left
    norm.b_x, norm.a_y, // top-right
    norm.b_x, norm.b_y, // bottom-right
    norm.a_x, norm.b_y, // bottom-left
  ]
}

fn tex_vertices() -> Vec<f32> {
  vec![0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0]
}

struct CommonRes {
  vao: gl::types::GLuint,
  vbo: gl::types::GLuint,
}

impl CommonRes {
  pub unsafe fn allocate() -> Self {
    let mut vao: gl::types::GLuint = 0;
    let mut vbo: gl::types::GLuint = 0;
    gl::GenVertexArrays(1, &mut vao);
    gl::GenBuffers(1, &mut vbo);
    Self { vao, vbo }
  }

  pub unsafe fn bind(&self) -> () {
    gl::BindVertexArray(self.vao);
  }

  pub unsafe fn bind_buffers(&self) -> () {
    gl::BindVertexArray(self.vao);
    gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
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
