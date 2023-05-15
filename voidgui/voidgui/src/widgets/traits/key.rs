use glfw::WindowEvent;

use crate::{render::painter::Drone, logic::CallbackResult};

pub trait KeySink: Send + Sync {
  fn handle_key(&mut self, painter: &Drone, e: &WindowEvent) -> CallbackResult;
}
