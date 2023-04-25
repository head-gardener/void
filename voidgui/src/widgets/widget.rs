use crate::render::painter::Painter;

#[derive(Debug)]
pub enum WidgetError {
  Unspecified(String),
}

impl std::fmt::Display for WidgetError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      WidgetError::Unspecified(cause) => {
        write!(f, "Unspecified error caused by: {}", cause)
      }
    }
  }
}

impl std::error::Error for WidgetError {}

pub trait Widget {
  unsafe fn plot(&self, painter: &Painter) -> Result<(), WidgetError>;
  unsafe fn draw(&self, painter: &Painter);
  fn catch(&self) -> bool;
}
