use crate::render::{Origin, Size};

/// `Parent` is an object, that can be polled for position of its children.
/// Parents can be *static* and *dynamic*. *Static* parents have child positions
/// pre-defined relative to some variable - size or origin of the parent.
/// *Dynamic* parents implement add and pop, generating child positions on 
/// the fly.
///
/// # Drawable
/// Implementing [Drawable] isn't strictly required, but useful for plotting
/// a parent after certain events (window resizes or mouse drags). In that case
/// note, however, that parent should be pushed to ring before children, since
/// plotting and origin polling is performed in order from older to earlier.
pub trait Parent: Send + Sync {
  fn add_child(&mut self, _: Size) -> usize {
    panic!("Add called on a static parent");
  }
  fn pop_child(&mut self, _: usize) {}
  fn nth_child(&self, n: usize) -> Option<Origin>;
}
