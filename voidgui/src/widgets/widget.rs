use downcast_rs::{impl_downcast, Downcast};

use crate::render::{painter::Painter, Point};

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

pub trait Widget: Downcast {
  /// Map widget's layout to normalized coordinates according to origin.
  ///
  /// # Errors
  ///
  /// Returns error when sequenced incorrectly.
  ///
  /// # Safety
  ///
  /// Since this function calls GL functions, there is a risk of an
  /// unexpected exit.
  unsafe fn plot(&mut self, painter: &dyn Painter) -> Result<(), WidgetError>;

  /// 
  ///
  /// # Errors
  ///
  /// This function will return an error if .
  fn draw(&self, painter: &dyn Painter) -> Result<(), WidgetError>;

  fn catch(&self) -> bool;

  /// Whether widget was plotted. This value should be reset to false to
  /// trigger plotting after any changes.
  fn plotted(&self) -> bool;

  /// Sets plotted to false, requesting replotting.
  fn request_plot(&mut self);

  /// Set origin point.
  fn set_origin(&mut self, origin: &Point);
}
impl_downcast!(Widget);

impl std::fmt::Debug for dyn Widget {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "widget")
  }
}
