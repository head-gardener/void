use glfw::WindowEvent;

use crate::{logic::CallbackResult, render::painter::Drone, Description};

pub trait KeySink: Send + Sync {
  fn handle_key(
    &mut self,
    desc: &Description,
    drone: &Drone,
    e: &WindowEvent,
  ) -> CallbackResult;
}
