use std::sync::{mpsc::Receiver, Arc, RwLock};

use crate::render::painter::{Description, Drone};

pub struct Backend {
  pub desc: Arc<RwLock<Description>>,
  pub events: Receiver<(f64, glfw::WindowEvent)>,
  pub drone: Drone,
}

impl Backend {
  pub unsafe fn new(w: u16, h: u16) -> Self {
    let desc = Arc::new(RwLock::new(Description::new(w, h)));
    let (drone, events) = Drone::new(desc.clone(), w as u32, h as u32).unwrap();

    Self {
      desc,
      events,
      drone,
    }
  }
}
