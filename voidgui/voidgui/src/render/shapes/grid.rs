use crate::render::{
  painter::{Drone, Description},
  section,
  shaders::Shader,
  Area, Color,
};

use super::common_res::CommonRes;

pub struct Grid {
  pub color: Color,
  pub res: CommonRes,
  pub vertices: i32,
}

impl Grid {
  pub unsafe fn new(color: Color) -> Self {
    let res = CommonRes::allocate();
    Self {
      color,
      res,
      vertices: 0,
    }
  }
}
