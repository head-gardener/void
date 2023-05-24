use std::{marker::PhantomData, str::FromStr, sync::RwLockReadGuard};

use crate::{
  colorscheme::CURSOR_COLOR,
  logic::CallbackResult,
  render::{
    painter::{Drone, DroneFeed},
    shapes::{rectangle, texture::get_text_size},
    text_table::{Orientation, OFFSET},
    Area, Origin, Size, TextTable,
  },
  Description,
};

use super::traits::{widget::Error, Drawable, InputEvent, InputSink, Widget};

use voidmacro::Menu;

#[derive(Menu)]
pub struct InputField<T, R> {
  table: TextTable,
  cursor: usize,
  state: State<R>,
  closure: T,
}

impl<T, R> InputField<T, R>
where
  T: Send,
  R: Send + std::fmt::Display + Default + FromStr + Clone,
  <R>::Err: std::fmt::Debug,
{
  pub unsafe fn new(
    desc: &Description,
    drone: &Drone,
    s: &str,
    closure: T,
  ) -> Result<Self, Error> {
    let state: State<R> = s.into();
    let table = TextTable::make_static(
      desc,
      drone,
      Orientation::Vertical,
      crate::render::text_table::CellStyle::Lighter,
      &[&state.to_string()],
    )?;
    Ok(Self {
      table,
      cursor: drone
        .get_rectangles(1, rectangle::Style::Solid(CURSOR_COLOR))
        .ok_or(Error::SpawnFailure)?[0],
      state,
      closure,
    })
  }

  pub fn closure(&self) -> &T {
    &self.closure
  }

  pub fn unwrap(&self) -> R {
    self.state.unwrap()
  }
}

impl<T, R> InputSink for InputField<T, R>
where
  T: Send + Sync,
  R: Clone + FromStr + Send + Sync,
  <R>::Err: std::fmt::Debug,
{
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

impl<T, R> Drawable for InputField<T, R>
where
  T: Send + Sync + 'static,
  R: Send + Sync + FromStr + Clone + 'static,
  <R>::Err: std::fmt::Debug,
{
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

impl<T, R> ToString for InputField<T, R>
where
  R: FromStr + ToString,
  <R>::Err: std::fmt::Debug,
{
  fn to_string(&self) -> String {
    self.state.to_string().parse::<R>().unwrap().to_string()
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct State<T> {
  data: Vec<char>,
  cursor: usize,
  output: PhantomData<T>,
}

impl<T> State<T>
where
  T: FromStr + Clone,
  <T>::Err: std::fmt::Debug,
{
  #[cfg(test)]
  fn new(s: &str, c: usize) -> Self {
    Self {
      data: s.chars().collect(),
      cursor: c,
      output: PhantomData,
    }
  }

  fn dispatch(&mut self, e: &InputEvent) {
    let safe = self.clone();
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
    if !self.validate() {
      *self = safe;
    }
  }

  fn before_cursor(&self) -> String {
    self.data.iter().take(self.cursor).collect()
  }

  fn validate(&self) -> bool {
    self.to_string().parse::<T>().is_ok()
  }

  fn unwrap(&self) -> T {
    self.to_string().parse().unwrap()
  }
}

impl<T> ToString for State<T> {
  fn to_string(&self) -> String {
    self.data.iter().collect()
  }
}

impl<T> From<&str> for State<T>
where
  T: Default + FromStr + Clone + ToString,
  <T>::Err: std::fmt::Debug,
{
  fn from(s: &str) -> Self {
    let d = T::default().to_string();
    let cs = if s.parse::<T>().is_ok() {
      s.chars()
    } else {
      d.chars()
    };
    Self {
      cursor: cs.clone().count(),
      data: cs.collect(),
      output: PhantomData,
    }
  }
}

#[cfg(test)]
mod test_state {
  use super::*;

  #[test]
  fn movement() {
    let mut s: State<String> = "123".into();
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
    let mut s: State<String> = "123".into();
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
    let mut s: State<String> = "мама".into();
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
    let s = State::<String>::new("мама", 2);
    assert_eq!(s.before_cursor(), "ма")
  }

  #[test]
  fn to_string() {
    let mut s: State<String> = "мама".into();

    s.dispatch(&InputEvent::Newline);
    s.dispatch(&InputEvent::Char('а'));
    assert_eq!(s.to_string(), "мама\nа")
  }

  #[test]
  fn integer() {
    let mut s: State<i32> = "123".into();
    assert_eq!(s, State::new("123", 3));

    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Newline);
    assert_eq!(s, State::new("123", 2));
    s.dispatch(&InputEvent::Char('a'));
    assert_eq!(s, State::new("123", 2));
    s.dispatch(&InputEvent::Char('1'));
    assert_eq!(s, State::new("1213", 3));

    assert_eq!(s.unwrap(), 1213);
  }

  #[test]
  fn float() {
    let mut s: State<f32> = "123.3".into();
    assert_eq!(s, State::new("123.3", 5));

    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Newline);
    assert_eq!(s, State::new("123.3", 4));
    s.dispatch(&InputEvent::Char('a'));
    assert_eq!(s, State::new("123.3", 4));
    s.dispatch(&InputEvent::Left);
    s.dispatch(&InputEvent::Char('1'));
    assert_eq!(s, State::new("1231.3", 4));

    assert_eq!(s.unwrap(), 1231.3);
  }

  #[test]
  fn error_handling() {
    let s: State<f32> = "abc.3".into();
    assert_eq!(s.to_string(), f32::default().to_string());
    assert_eq!(s.unwrap(), f32::default());

    let s: State<i32> = "abc.3".into();
    assert_eq!(s.to_string(), i32::default().to_string());
    assert_eq!(s.unwrap(), i32::default());
  }
}
