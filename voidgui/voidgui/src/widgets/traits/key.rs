use glfw::WindowEvent;

use crate::{logic::CallbackResult, render::painter::Drone};

pub trait KeySink: Send + Sync {
  fn handle_key(&mut self, drone: &Drone, e: &WindowEvent) -> CallbackResult;
}
