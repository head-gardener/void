use glfw::WindowEvent;

use crate::render::painter::Painter;

use super::CallbackResult;

pub trait KeySink {
  fn handle_key(&mut self, painter: &Painter, e: &WindowEvent) -> CallbackResult;
}
