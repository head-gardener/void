use downcast_rs::{impl_downcast, Downcast};

use crate::render::painter::Painter;

use super::{Widget, WidgetError};

pub trait Drawable: Widget + Downcast {
  /// Map widget's layout to normalized coordinates according to origin.
  /// Must be called before drawing.
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

  /// Draw the widget. [Drawable::plot] should be called beforehand.
  /// Can be run multiple times after single plotting.
  /// 
  /// # Errors
  /// Returns error when sequenced incorrectly and in obscure cases when
  /// drawing fails.
  fn draw(&self, painter: &Painter) -> Result<(), WidgetError>;
}
impl_downcast!(Drawable);
