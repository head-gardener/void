use std::sync::RwLockReadGuard;

use crate::{
  logic::CallbackResult,
  render::painter::{Description, Drone},
};

pub enum InputEvent {
  Char(char),
  Left,
  Right,
  Delete,
  Backspace,
  Home,
  End,
  Newline,
}

/// `Input sink` is an object, that catches [InputEvent].
///
/// # Properties
///
/// Only one input sink can be consuming text input at a time.
pub trait InputSink {
  fn handle_event(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: &InputEvent,
  ) -> CallbackResult;
}
