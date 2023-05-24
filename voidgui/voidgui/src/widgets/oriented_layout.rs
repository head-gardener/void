use std::sync::RwLockReadGuard;

use crate::{
  logic::StableBuffer,
  render::{painter::DroneFeed, Origin, OriginPole, Size},
  Description,
};

use super::traits::{Drawable, Parent, Widget};

pub struct OrientedLayout {
  origin: Option<Origin>,
  sizes: StableBuffer<i32>,
}

impl OrientedLayout {
  pub fn new() -> Self {
    Self {
      origin: None,
      sizes: StableBuffer::new(),
    }
  }
}

impl Widget for OrientedLayout {
  fn plotted(&self) -> bool {
    self.origin.is_some()
  }

  fn request_plot(&mut self) {
    self.origin = None;
  }

  fn set_origin(&mut self, o: &Origin) {
    self.origin = Some(o.clone());
  }
}

impl Drawable for OrientedLayout {
  fn plot(
    &mut self,
    _: RwLockReadGuard<Description>,
    _: DroneFeed,
  ) -> Result<(), super::traits::Error> {
    Ok(())
  }

  unsafe fn draw(&mut self, _: DroneFeed) -> Result<(), super::traits::Error> {
    Ok(())
  }

  fn size(&mut self) -> crate::render::Size {
    Size::new(0, 0)
  }
}

impl Parent for OrientedLayout {
  fn nth_child(&self, n: usize) -> Option<Origin> {
    if !self.sizes.has(n) {
      return None;
    }

    self.origin.map(|o| {
      let offset: i32 = self.sizes.iter().take(n).filter_map(|x| x).sum();
      Origin::new(
        o.x,
        match o.pole {
          OriginPole::TopLeft | OriginPole::TopRight => o.y + offset,
          OriginPole::BottomLeft | OriginPole::BottomRight => o.y - offset,
        },
        o.pole,
      )
    })
  }

  fn add_child(&mut self, s: Size) -> usize {
    self.sizes.put(s.height)
  }

  fn pop_child(&mut self, n: usize) {
    self.sizes.del(n);
  }
}
