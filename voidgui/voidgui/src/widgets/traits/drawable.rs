use std::sync::{Arc, RwLock, RwLockReadGuard};

use downcast_rs::{impl_downcast, Downcast};

use crate::render::painter::{Drone, DroneFeed, Painter};

use super::{Error, Widget};

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
  fn plot(
    &mut self,
    painter: RwLockReadGuard<Painter>,
    feed: DroneFeed,
  ) -> Result<(), Error>;

  /// Draw the widget. [Drawable::plot] should be called beforehand.
  /// Can be run multiple times after single plotting.
  ///
  /// # Errors
  /// Returns error when sequenced incorrectly and in obscure cases when
  /// drawing fails.
  unsafe fn draw(&mut self, feed: DroneFeed) -> Result<(), Error>;
}
impl_downcast!(Drawable);
