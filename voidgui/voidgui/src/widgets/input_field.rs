use std::sync::RwLockReadGuard;

use crate::{
  colorscheme::CURSOR_COLOR,
  logic::CallbackResult,
  render::{
    painter::{Description, Drone, DroneFeed},
    shapes::{rectangle, texture::get_text_size},
    text_table::{Orientation, OFFSET},
    Area, Origin, TextTable, Size,
  },
};

use super::traits::{widget::Error, Drawable, InputEvent, InputSink, Widget};

use voidmacro::Menu;

#[derive(Menu)]
pub struct InputField<T> {
  table: TextTable,
  cursor: usize,
  state: State,
  closure: T,
}

impl<T: Send> InputField<T> {
  pub unsafe fn new(
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    s: &str,
    closure: T,
  ) -> Result<Self, Error> {
    let table = TextTable::make_static(
      desc,
      drone,
      Orientation::Vertical,
      crate::render::text_table::CellStyle::Lighter,
      &[&s],
    )?;
    Ok(Self {
      table,
      cursor: drone
        .get_rectangles(1, rectangle::Style::Solid(CURSOR_COLOR))
        .ok_or(Error::SpawnFailure)?[0],
      state: s.into(),
      closure,
    })
  }

  pub fn closure(&self) -> &T {
    &self.closure
  }
}

impl<T: Send + Sync> InputSink for InputField<T> {
  fn handle_event(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
    e: &InputEvent,
  ) -> CallbackResult {
    self.state.dispatch(e);
    match self
      .table
      .update_cell(desc, drone, 0, &self.state.to_string())
    {
      Ok(()) => CallbackResult::None,
      Err(e) => CallbackResult::Error(e),
    }
  }
}

impl<T: Send + Sync + 'static> Drawable for InputField<T> {
  fn plot(
    &mut self,
    desc: RwLockReadGuard<Description>,
    feed: DroneFeed,
  ) -> Result<(), Error> {
    self.table.plot(&desc, &feed)?;

    // FIXME: wtf is this shit

    let d = self.state.to_string();
    let (o, s) = self.table.area().unwrap().to_prim();
    let after = unsafe {
      get_text_size(
        desc.font(),
        &self.state.before_cursor().lines().last().unwrap_or(""),
      )
      .map_err(|e| Error::Unspecified(e.to_owned()))
    }?
    .width;
    let c = d.lines().count();
    let c = if c == 0 { 1 } else { c };
    let l = s.height / c as i32;
    let a = Area::new(
      o.x + OFFSET + after,
      o.y + OFFSET / 2 + s.height - l,
      2,
      l - OFFSET * 2,
    );
    feed.plot_rectangle(
      self.cursor,
      a.to_normalized(desc.window_area()).to_coords(),
    );
    Ok(())
  }

  unsafe fn draw(&mut self, feed: DroneFeed) -> Result<(), Error> {
    self.table.draw(&feed)?;
    feed.draw_rectangle(self.cursor);
    Ok(())
  }

  fn size(&mut self) -> Size {
    self.table.size()
  }
}

impl<T> ToString for InputField<T> {
  fn to_string(&self) -> String {
    self.state.to_string()
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
      InputEvent::Newline => {
        self.data.insert(self.cursor, '\n');
        self.cursor += 1;
      }
    }
  }

  fn before_cursor(&self) -> String {
    self.data.iter().take(self.cursor).collect()
  }
}

impl ToString for State {
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

#[cfg(test)]
mod test_state {
  use super::*;

  #[test]
  fn movement() {
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
  }

  #[test]
  fn editing() {
    let mut s: State = "123".into();
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

    s.dispatch(&InputEvent::Char('1'));
    assert_eq!(s, State::new("12", 1));
    s.dispatch(&InputEvent::Newline);
    assert_eq!(s, State::new("1\n2", 2));
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

  #[test]
  fn to_string() {
    let mut s: State = "мама".into();

    s.dispatch(&InputEvent::Newline);
    s.dispatch(&InputEvent::Char('а'));
    assert_eq!(s.to_string(), "мама\nа")
  }
}
