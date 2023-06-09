use std::sync::{mpsc::Receiver, Arc, RwLock};

use crate::{render::painter::Drone, Description};
pub struct Backend {
  pub desc: Arc<RwLock<Description>>,
  pub events: Receiver<(f64, glfw::WindowEvent)>,
  pub drone: Drone,
}

impl Backend {
  pub unsafe fn new(w: i32, h: i32) -> Self {
    let desc = Arc::new(RwLock::new(Description::new(w, h)));
    let (drone, events) = Drone::new(desc.clone(), w as u32, h as u32).unwrap();

    Self {
      desc,
      events,
      drone,
    }
  }

  #[cfg(test)]
  pub fn mock(w: i32, h: i32) -> Self {
    use std::sync::mpsc;

    let desc = Arc::new(RwLock::new(Description::new(w, h)));
    let drone = Drone::mock();
    let (_, events) = mpsc::channel();

    Self {
      desc,
      events,
      drone,
    }
  }
}
