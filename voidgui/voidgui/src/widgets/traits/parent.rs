use crate::render::Origin;

pub trait Parent {
  fn nth_child(&self, n: usize) -> Option<Origin>;
}
