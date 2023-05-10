use downcast_rs::{impl_downcast, Downcast};

use crate::render::painter::Painter;

use super::{Widget, Error};

/// `Drawable` is an object, capable of being drawn.
pub trait Drawable: Widget + Downcast + Send + Sync {
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
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), Error>;

  /// Draw the widget. [Drawable::plot] should be called beforehand.
  /// Can be run multiple times after single plotting.
  /// 
  /// # Errors
  /// Returns error when sequenced incorrectly and in obscure cases when
  /// drawing fails.
  fn draw(&self, painter: &Painter) -> Result<(), Error>;
}
impl_downcast!(Drawable);
