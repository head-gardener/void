use crate::{
  logic::ring::{Mark, RingMember},
  render::{painter::Painter, text_table::Orientation, Origin, TextTable},
  widgets::traits::CallbackResult,
};
use std::{cell::RefCell, rc::Rc};

use super::traits::{widget::WidgetError, InputEvent, InputSink, Widget};

use voidmacro::Menu;

#[derive(Menu)]
pub struct InputField {
  table: TextTable,
  state: State,
}

impl InputField {
  pub unsafe fn new(painter: &Painter, s: &str) -> Result<Self, WidgetError> {
    Ok(Self {
      table: TextTable::make_static(painter, Orientation::Vertical, &[&s])?,
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
    println!("self.state = {:?}", self.state);
    match self.table.update_cell(p, 0, &self.state.data) {
      Ok(()) => CallbackResult::None,
      Err(e) => CallbackResult::Error(e),
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
struct State {
  data: String,
  cursor: usize,
}

impl State {
  #[cfg(test)]
  fn new(s: &str, c: usize) -> Self {
    Self {
      data: s.to_string(),
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
    }
  }
}

impl From<&str> for State {
  fn from(s: &str) -> Self {
    let s = s.to_string();
    Self {
      cursor: s.len(),
      data: s,
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

// #[cfg(test)]
mod test_state {
  use super::*;

  #[test]
  fn test() {
    let mut s: State = "123".into();
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
}
