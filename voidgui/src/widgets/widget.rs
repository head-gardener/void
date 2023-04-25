use crate::render::painter::Painter;

#[derive(Debug, PartialEq, Eq)]
pub enum WidgetError {
  Unspecified(String),
  Uninitialized(String),
  Unplotted(String),
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
    }
  }
}

impl std::error::Error for WidgetError {}

pub trait Widget {
  unsafe fn plot(&mut self, painter: &dyn Painter) -> Result<(), WidgetError>;
  fn draw(&self, painter: &dyn Painter) -> Result<(), WidgetError>;
  fn plotted(&self) -> bool;
  fn catch(&self) -> bool;
}
