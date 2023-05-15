use crate::{render::painter::DroneFeed, logic::CallbackResult};

/// `Transient` is an object, that listens for control events, as described
/// in [Ring::handle_transient_control_event]. All objects, stored with the
/// same [Mark] as a transient, will be deleted once it dies.
pub trait Transient: Send + Sync {
  fn handle_cancel(&self, _: &DroneFeed) -> CallbackResult {
    CallbackResult::None
  }

  fn handle_accept(&self, _: &DroneFeed) -> CallbackResult {
    CallbackResult::None
  }
}
