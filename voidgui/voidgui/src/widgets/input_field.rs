use crate::{
  colorscheme::CURSOR_COLOR,
  logic::ring::{Mark, RingMember},
  render::{
    painter::Painter,
    shapes::{texture::get_text_size, Rectangle},
    text_table::{Orientation, OFFSET},
    Area, Origin, TextTable,
  },
  widgets::traits::CallbackResult,
};
use std::{cell::RefCell, rc::Rc};

use super::traits::{
  widget::WidgetError, Drawable, InputEvent, InputSink, Widget,
};

use voidmacro::Menu;

#[derive(Menu)]
pub struct InputField {
  table: TextTable,
  cursor: Rectangle,
  state: State,
}

impl InputField {
  pub unsafe fn new(painter: &Painter, s: &str) -> Result<Self, WidgetError> {
    let mut table =
      TextTable::make_static(painter, Orientation::Vertical, &[&s])?;
    table
      .set_cell_color(0, crate::render::text_table::CellColor::Lighter)
      .unwrap();
    Ok(Self {
      table,
      cursor: Rectangle::new(CURSOR_COLOR),
      state: s.into(),
    })
  }

  pub fn wrap(
    self,
    mark: Mark,
    parent: Mark,
    child_n: usize,
  ) -> InputFieldWrapper {
    InputFieldWrapper {
      field: Rc::new(RefCell::new(self)),
      mark,
      parent,
      child_n,
    }
  }
}

impl InputSink for InputField {
  fn handle_event(&mut self, p: &Painter, e: &InputEvent) -> CallbackResult {
    self.state.dispatch(e);
    match self.table.update_cell(p, 0, &self.state.to_string()) {
      Ok(()) => CallbackResult::None,
      Err(e) => CallbackResult::Error(e),
    }
  }
}

impl Drawable for InputField {
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), WidgetError> {
    self.table.plot(painter)?;
    let (o, s) = self.table.area().unwrap().to_prim();
    let after = get_text_size(painter, &self.state.before_cursor())
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))?
      .width;
    let a =
      Area::new(o.x + OFFSET + after, o.y + OFFSET / 2, 2, s.height - OFFSET);
    self
      .cursor
      .plot(painter, &a)
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))
  }

  fn draw(&self, painter: &Painter) -> Result<(), WidgetError> {
    self.table.draw(painter)?;
    unsafe { self.cursor.draw(painter) }
      .map_err(|e| WidgetError::Unspecified(e.to_owned()))
  }
}

#[derive(Debug, PartialEq, Eq)]
struct State {
  data: Vec<char>,
  cursor: usize,
}

impl State {
  #[cfg(test)]
  fn new(s: &str, c: usize) -> Self {
    Self {
      data: s.chars().collect(),
      cursor: c,
    }
  }

  fn dispatch(&mut self, e: &InputEvent) {
    match e {
      InputEvent::Char(c) => {
        self.data.insert(self.cursor, *c);
        self.cursor += 1;
      }
      InputEvent::Left => {
        if self.cursor > 0 {
          self.cursor -= 1;
        } else {
          self.cursor = 0;
        }
      }
      InputEvent::Right => {
        if self.cursor < self.data.len() {
          self.cursor += 1;
        } else {
          self.cursor = self.data.len();
        }
      }
      InputEvent::Delete => {
        if self.cursor < self.data.len() {
          self.data.remove(self.cursor);
        }
      }
      InputEvent::Backspace => {
        if self.cursor > 0 {
          self.data.remove(self.cursor - 1);
          self.cursor -= 1;
        } else {
          self.cursor = 0;
        }
      }
      InputEvent::Home => self.cursor = 0,
      InputEvent::End => self.cursor = self.data.len(),
    }
  }

  fn before_cursor(&self) -> String {
    self.data.iter().take(self.cursor).collect()
  }

  fn to_string(&self) -> String {
    self.data.iter().collect()
  }
}

impl From<&str> for State {
  fn from(s: &str) -> Self {
    Self {
      cursor: s.chars().count(),
      data: s.chars().collect(),
    }
  }
}

pub struct InputFieldWrapper {
  field: Rc<RefCell<InputField>>,
  mark: Mark,
  parent: Mark,
  child_n: usize,
}

impl RingMember for InputFieldWrapper {
  fn push_to_ring(&self, ring: &mut crate::logic::Ring) {
    ring.push_input_sink(self.field.clone(), self.mark);
    ring.push(self.field.clone(), self.mark, self.parent, self.child_n);
  }
}

#[cfg(test)]
mod test_state {
  use super::*;

  #[test]
  fn events_handling() {
    let mut s: State = "123".into();
    assert_eq!(s, State::new("123", 3));

    s.dispatch(&InputEvent::Home);
    assert_eq!(s, State::new("123", 0));
    s.dispatch(&InputEvent::End);
    assert_eq!(s, State::new("123", 3));

    s.dispatch(&InputEvent::Left);
    assert_eq!(s, State::new("123", 2));
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Left);
    assert_eq!(s, State::new("123", 0));

    s.dispatch(&InputEvent::Right);
    assert_eq!(s, State::new("123", 1));
    s.dispatch(&InputEvent::Right);
    s.dispatch(&InputEvent::Right);
    s.dispatch(&InputEvent::Right);
    assert_eq!(s, State::new("123", 3));

    s.dispatch(&InputEvent::Delete);
    assert_eq!(s, State::new("123", 3));
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Delete);
    assert_eq!(s, State::new("12", 2));
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Delete);
    assert_eq!(s, State::new("2", 0));

    s.dispatch(&InputEvent::Char('1'));
    assert_eq!(s, State::new("12", 1));
    s.dispatch(&InputEvent::Right);
    s.dispatch(&InputEvent::Char('3'));
    assert_eq!(s, State::new("123", 3));

    s.dispatch(&InputEvent::Backspace);
    assert_eq!(s, State::new("12", 2));
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Backspace);
    assert_eq!(s, State::new("2", 0));
    s.dispatch(&InputEvent::Backspace);
    assert_eq!(s, State::new("2", 0));
  }

  #[test]
  fn unicode() {
    let mut s: State = "мама".into();
    assert_eq!(s, State::new("мама", 4));

    s.dispatch(&InputEvent::Left);
    assert_eq!(s, State::new("мама", 3));
    s.dispatch(&InputEvent::Right);
    assert_eq!(s, State::new("мама", 4));

    s.dispatch(&InputEvent::Home);
    assert_eq!(s, State::new("мама", 0));
    s.dispatch(&InputEvent::End);
    assert_eq!(s, State::new("мама", 4));

    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Backspace);
    assert_eq!(s, State::new("маа", 2));
    s.dispatch(&InputEvent::Char('ш'));
    assert_eq!(s, State::new("маша", 3));

    s.dispatch(&InputEvent::Delete);
    assert_eq!(s, State::new("маш", 3));
  }

  #[test]
  fn before_cursor() {
    let s = State::new("мама", 2);
    assert_eq!(s.before_cursor(), "ма")
  }
}
