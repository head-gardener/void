use crate::render::Origin;


#[derive(Debug, PartialEq, Eq)]
pub enum Error {
  InitFailure(&'static str),
  Unspecified(String),
  Uninitialized(&'static str),
  Unplotted(&'static str),
}

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Unspecified(cause) => {
        write!(f, "Unspecified error caused by: {}", cause)
      }
      Error::Uninitialized(param) => {
        write!(f, "Field '{}' uninitialized", param)
      }
      Error::Unplotted(widget) => {
        write!(f, "Widget '{}' drawn before being plotted", widget)
      }
      Error::InitFailure(what) => {
        write!(f, "Component {} failed to initialize.", what)
      }
    }
  }
}

impl std::error::Error for Error {}

impl From<String> for Error {
  fn from(s: String) -> Self {
    Error::Unspecified(s)
  }
}

impl From<&str> for Error {
  fn from(s: &str) -> Self {
    Error::Unspecified(s.to_string())
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
