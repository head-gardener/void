use crate::render::painter::Painter;

use super::CallbackResult;

/// `Transient` is an object, that listens for control events, as described
/// in [Ring::handle_transient_control_event]. All objects, stored with the
/// same [Mark] as a transient, will be deleted once it dies.
///
/// # Properties
///
/// Only one transient can exist at a time, previous transient will be
/// closed with `cancel` if new one is created.
pub trait Transient {
  fn handle_cancel(&self, painter: &Painter) -> CallbackResult;
  fn handle_accept(&self, painter: &Painter) -> CallbackResult;
}
