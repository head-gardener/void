use std::sync::RwLockReadGuard;

use crate::{
  logic::ring,
  render::{
    painter::{Description, DroneFeed},
    Area, Origin, OriginPole, Size,
  },
};

use super::traits::{Drawable, Parent, Widget};

pub struct Window {
  area: Area,
  plotted: bool,
}

impl Window {
  pub fn new(painter: &RwLockReadGuard<Description>) -> Self {
    Self {
      area: painter.window_area().clone(),
      plotted: false,
    }
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
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
  fn plotted(&self) -> bool {
    self.plotted
  }

  fn request_plot(&mut self) {
    self.plotted = false;
  }

  fn set_origin(&mut self, _: &Origin) {}
}

impl Drawable for Window {
  fn plot(
    &mut self,
    painter: RwLockReadGuard<Description>,
    _: DroneFeed,
  ) -> Result<(), super::traits::Error> {
    self.area = painter.window_area().clone();
    self.plotted = true;
    Ok(())
  }

  unsafe fn draw(&mut self, _: DroneFeed) -> Result<(), super::traits::Error> {
    Ok(())
  }

  fn size(&mut self) -> crate::render::Size {
    let (_, s) = self.area.to_prim();
    s
  }
}

impl Parent for Window {
  fn nth_child(&self, n: usize, _: Size) -> Option<Origin> {
    match n {
      0 => Some(Origin::new(30, 30, OriginPole::TopLeft)),
      1 => Some(Origin::new(self.area.width, 0, OriginPole::TopRight)),
      2 => Some(Origin::new(0, self.area.height, OriginPole::BottomLeft)),
      3 => Some(Origin::new(
        self.area.width,
        self.area.height,
        OriginPole::BottomRight,
      )),
      _ => None,
    }
  }
}
