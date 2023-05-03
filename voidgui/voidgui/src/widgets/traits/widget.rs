use downcast_rs::{impl_downcast, Downcast};

use crate::{render::{painter::Painter, Origin}, logic::ring::{Mark, RingMember}};

#[derive(Debug, PartialEq, Eq)]
pub enum WidgetError {
  InitFailure(&'static str),
  Unspecified(String),
  Uninitialized(&'static str),
  Unplotted(&'static str),
}

pub enum CallbackResult {
  /// Event was dropped.
  Skip,
  /// No side effects.
  None,
  /// Callback failed.
  Error(WidgetError),
  /// Push a widget to the ring.
  Push(Box::<dyn RingMember>),
  /// Modify a widget by mark.
  Modify(Mark, fn(&mut dyn Widget)),
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
        },
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
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError>;

  /// 
  ///
  /// # Errors
  ///
  /// This function will return an error if .
  fn draw(&self, painter: &Painter) -> Result<(), WidgetError>;

  /// Whether widget was plotted. This value should be reset to false to
  /// trigger plotting after any changes.
  fn plotted(&self) -> bool;

  /// Sets plotted to false, requesting replotting.
  fn request_plot(&mut self);

  /// Set origin point.
  fn set_origin(&mut self, origin: &Origin);
}
impl_downcast!(Widget);

impl std::fmt::Debug for dyn Widget {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "widget")
  }
}
