use crate::render::{Area, Color};

use super::common_res::CommonRes;

#[derive(Debug, Clone)]
pub enum Style {
  Solid(Color),
  Lit(Color, Option<Area>),
}

pub struct Rectangle {
  pub style: Style,
  pub res: CommonRes,
}

impl Rectangle {
  pub unsafe fn new(style: Style) -> Self {
    let res = CommonRes::allocate();
    Self { style, res }
  }
}
