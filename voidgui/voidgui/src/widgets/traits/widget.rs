use crate::{
  logic::ring::{Mark, RingMember},
  render::Origin,
};

pub enum CallbackResult {
  /// Event was dropped.
  Skip,
  /// No side effects.
  None,
  /// Callback failed.
  Error(WidgetError),
  /// Push a widget to the ring.
  Push(Box<dyn RingMember>),
  /// Modify a widget by mark.
  Modify(Mark, fn(&mut dyn Widget)),
}

#[derive(Debug, PartialEq, Eq)]
pub enum WidgetError {
  InitFailure(&'static str),
  Unspecified(String),
  Uninitialized(&'static str),
  Unplotted(&'static str),
}

impl std::fmt::Display for WidgetError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      WidgetError::Unspecified(cause) => {
        write!(f, "Unspecified error caused by: {}", cause)
      }
      WidgetError::Uninitialized(param) => {
        write!(f, "Field '{}' uninitialized", param)
      }
      WidgetError::Unplotted(widget) => {
        write!(f, "Widget '{}' drawn before being plotted", widget)
      }
      WidgetError::InitFailure(what) => {
        write!(f, "Component {} failed to initialize.", what)
      }
    }
  }
}

impl std::error::Error for WidgetError {}

impl From<String> for WidgetError {
  fn from(s: String) -> Self {
    WidgetError::Unspecified(s)
  }
}

impl From<&str> for WidgetError {
  fn from(s: &str) -> Self {
    WidgetError::Unspecified(s.to_string())
  }
}

pub trait Widget {
  /// Set origin point.
  fn set_origin(&mut self, origin: &Origin);

  /// Whether widget was plotted. This value should be reset to false to
  /// trigger plotting after any changes.
  fn plotted(&self) -> bool;

  /// Sets plotted to false, requesting replotting.
  fn request_plot(&mut self);
}

impl std::fmt::Debug for dyn Widget {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "widget")
  }
}
