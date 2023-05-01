use crate::render::{painter::Painter, Point, TextTable};

use super::traits::{widget::WidgetError, Clickable, ClickableWidget, Widget};

pub enum Orientation {
  Vertical,
  Horizontal,
}

pub trait Menu {}
