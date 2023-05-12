use glfw::WindowEvent;

use crate::{render::painter::Drone, logic::CallbackResult};

pub trait KeySink {
  fn handle_key(&mut self, painter: &Drone, e: &WindowEvent) -> CallbackResult;
}
