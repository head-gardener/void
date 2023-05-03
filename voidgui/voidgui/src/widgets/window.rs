use std::{cell::RefCell, rc::Rc};

use crate::render::{Area, Origin, OriginPole};

use super::traits::{Parent, Widget};

pub struct Window {
  area: Area,
}

impl Window {
  pub fn new(painter: &crate::render::painter::Painter) -> Self {
    Self {
      area: painter.window_area().clone(),
    }
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = Rc::new(RefCell::new(self));
    ring.push_parent(rc.clone(), crate::logic::ring::Mark::Window);
    ring.push(
      rc,
      crate::logic::ring::Mark::Window,
      crate::logic::ring::Mark::None,
      0,
    );
  }
}

impl Widget for Window {
  unsafe fn plot(
    &mut self,
    painter: &crate::render::painter::Painter,
  ) -> Result<(), super::traits::WidgetError> {
    self.area = painter.window_area().clone();
    Ok(())
  }

  fn draw(
    &self,
    _: &crate::render::painter::Painter,
  ) -> Result<(), super::traits::WidgetError> {
    Ok(())
  }

  fn plotted(&self) -> bool {
    true
  }

  fn request_plot(&mut self) {}

  fn set_origin(&mut self, _: &Origin) {}
}

impl Parent for Window {
  fn nth_child(&self, n: usize) -> Option<Origin> {
    match n {
      0 => Some(Origin::new(30, 30, OriginPole::TopLeft)),
      1 => Some(Origin::new(self.area.width, 0, OriginPole::TopRight)),
      2 => Some(Origin::new(0, self.area.height, OriginPole::BottomLeft)),
      _ => None,
    }
  }
}
