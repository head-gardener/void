use crate::render::painter::Painter;

use super::CallbackResult;

pub enum InputEvent {
  Char(char),
  Left,
  Right,
  Delete,
  Backspace,
  Home,
  End,
}

pub trait InputSink {
  fn handle_event(&mut self, p: &Painter, e: &InputEvent) -> CallbackResult;
}
