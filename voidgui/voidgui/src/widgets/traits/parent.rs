use crate::render::Origin;

/// `Parent` is an object, that can be polled for position of its children.
/// 
/// # Drawable
/// Implementing [Drawable] isn't strictly required, but useful for plotting
/// a parent after certain events (window resizes or mouse drags). In that case
/// note, however, that parent should be pushed to ring before children, since
/// plotting and origin polling is performed in order from older to earlier.
pub trait Parent: Send + Sync {
  fn nth_child(&self, n: usize) -> Option<Origin>;
}
