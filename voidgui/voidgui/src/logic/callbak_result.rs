use crate::{
  description::Mode,
  render::painter::Drone,
  widgets::{self, traits::Drawable},
  Description,
};

use super::{DamageTracker, File, FileCallback, RingElement, Tag, Wrap};

pub enum CallbackResult {
  /// Event was dropped.
  Pass,

  /// No side effects.
  None,

  /// Callback failed.
  Error(widgets::Error),

  /// Push a widget to the ring.
  Push(Box<dyn RingElement>),

  /// Read a file by tag.
  /// Since reading can have no side effects, you can return them -
  /// they even might be evaluated recursively.
  Read(
    Tag,
    Box<
      dyn FnOnce(
        Option<Wrap<dyn Drawable>>,
        Option<Wrap<File>>,
        &Description,
        &Drone,
      ) -> CallbackResult,
    >,
  ),

  /// Modify a file by tag.
  /// In the perfect world the callback would be a monad, but it is what it is
  /// so play nice and only use it to mutate the file with file's methods!
  /// Not guaranteed to execute if tag is invalid.
  File(Tag, FileCallback),

  /// Access damage tracker.
  Damage(Box<dyn FnOnce(&mut DamageTracker)>),

  /// Exit codes for interacting with external caller. Can't be 0.
  ExitCode(u64),

  /// Change window global mode.
  Mode(Mode),
}

impl std::fmt::Debug for CallbackResult {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CallbackResult::Pass => write!(f, "Pass"),
      CallbackResult::None => write!(f, "None"),
      CallbackResult::Error(e) => write!(f, "Error({})", e),
      CallbackResult::Push(_) => write!(f, "Push(_)"),
      CallbackResult::Damage(_) => write!(f, "Damage(_)"),
      CallbackResult::ExitCode(c) => write!(f, "ExitCode({})", c),
      CallbackResult::Mode(m) => write!(f, "Mode({:?})", m),
      CallbackResult::Read(t, _) => write!(f, "Read({t}, _)"),
      CallbackResult::File(t, _) => write!(f, "File({t}, _)"),
    }
  }
}

impl CallbackResult {
  /// Returns `true` if the callback result is [`Pass`].
  ///
  /// [`Pass`]: CallbackResult::Pass
  #[must_use]
  pub fn is_pass(&self) -> bool {
    matches!(self, Self::Pass)
  }
}

impl From<&'static str> for CallbackResult {
  fn from(s: &'static str) -> Self {
    CallbackResult::Error(widgets::Error::Unspecified(s.to_string()))
  }
}
