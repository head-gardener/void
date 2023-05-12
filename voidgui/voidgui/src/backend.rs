use std::sync::{mpsc::Receiver, Arc, RwLock};

use crate::render::painter::{Drone, Painter, Parka};

pub struct Backend {
  pub painter: Arc<RwLock<Painter>>,
  pub events: Receiver<(f64, glfw::WindowEvent)>,
  pub drone: Drone,
}

impl Backend {
  pub unsafe fn new(w: u16, h: u16) -> Self {
    let (drone, events, painter) =
      Drone::new(Parka::new(w as u32, h as u32)).unwrap();

    Self {
      painter,
      events,
      drone,
    }
  }
}
