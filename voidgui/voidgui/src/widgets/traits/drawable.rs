use std::sync::RwLockReadGuard;

use downcast_rs::{impl_downcast, DowncastSync};

use crate::render::{painter::{Description, DroneFeed}, Size};

use super::{Error, Widget};

/// `Drawable` is an object, capable of being drawn.
pub trait Drawable: Widget + DowncastSync + Send + Sync {
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
    desc: RwLockReadGuard<Description>,
    feed: DroneFeed,
  ) -> Result<(), Error>;

  /// Draw the widget. [Drawable::plot] should be called beforehand.
  /// Can be run multiple times after single plotting.
  ///
  /// # Errors
  /// Returns error when sequenced incorrectly and in obscure cases when
  /// drawing fails.
  unsafe fn draw(&mut self, feed: DroneFeed) -> Result<(), Error>;

  fn size(&mut self) -> Size;
}
impl_downcast!(Drawable);
