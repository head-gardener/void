use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::{render::painter::Drone, widgets::traits::KeySink, Description};

use super::{CallbackResult, Dataset, Mark, Record, Wrap};

pub trait FileSub: Send + Sync {
  fn on_drop(&mut self, _: &Description, _: &Drone) {}
  fn on_put(&mut self, _: &Description, _: &Drone, _: i64, _: &[&str]) {}
  fn on_set(&mut self, _: &Description, _: &Drone, _: i64, _: usize, _: &str) {}
  fn on_rem(&mut self, _: &Description, _: &Drone, _: i64) {}
  fn on_new_search(&mut self, _: &Description, _: &Drone) {}
  fn on_drop_search(&mut self, _: &Description, _: &Drone) {}
  fn on_search_update(
    &mut self,
    _: &Description,
    _: &Drone,
    _: &Option<(i64, usize)>,
  ) {
  }
}

/// [File] controls access to [Dataset], notifying subscribers
/// on any updates.
pub struct File {
  data: Dataset,
  subscribers: Vec<(Mark, Wrap<dyn FileSub>)>,
}

pub type FileCallback = Box<dyn FnOnce(&mut File, &Description, &Drone)>;

impl File {
  pub fn new<R: Record>() -> Self {
    Self {
      data: Dataset::new::<R>(),
      subscribers: vec![],
    }
  }

  pub fn read(&self) -> &Dataset {
    &self.data
  }

  pub fn drop(&mut self, desc: &Description, drone: &Drone) {
    self.data.clear();
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_drop(desc, drone));
  }

  pub fn put<R: Record>(&mut self, desc: &Description, drone: &Drone, x: R) {
    let uid = *x.uid();
    self.data.put(x);
    let row = self.data.row(uid);
    row.map(|row| {
      self.subscribers.iter().for_each(|(_, s)| {
        s.write().unwrap().on_put(desc, drone, uid, row.as_slice())
      });
    });
  }

  pub fn set(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    field: usize,
    value: &str,
  ) {
    self.data.set(uid, field, value);
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_set(desc, drone, uid, field, value)
    });
  }

  pub fn new_row(&mut self, desc: &Description, drone: &Drone, uid: i64) {
    self.data.new_row(uid);
    let row = self.data.row(uid);
    row.map(|row| {
      self.subscribers.iter().for_each(|(_, s)| {
        s.write().unwrap().on_put(desc, drone, uid, row.as_slice())
      });
    });
  }

  pub fn rem(&mut self, desc: &Description, drone: &Drone, uid: i64) {
    self.data.rem(uid);
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_rem(desc, drone, uid));
  }

  pub fn new_search(&mut self, desc: &Description, drone: &Drone, value: &str) {
    self.data.new_search(value);
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_new_search(desc, drone));
    self.find_next(desc, drone);
  }

  pub fn clear_search(&mut self, desc: &Description, drone: &Drone) -> bool {
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_drop_search(desc, drone));
    self.data.clear_search()
  }

  pub fn find_prev(&mut self, desc: &Description, drone: &Drone) -> bool {
    let res = self.data.find_prev();
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_search_update(desc, drone, &res)
    });
    res.is_some()
  }

  pub fn find_next(&mut self, desc: &Description, drone: &Drone) -> bool {
    let res = self.data.find_next();
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_search_update(desc, drone, &res)
    });

    res.is_some()
  }

  pub fn subscribe(&mut self, s: Wrap<dyn FileSub>, m: Mark) {
    self.subscribers.push((m, s));
  }

  pub fn umsubscribe(&mut self, m: Mark) {
    self.subscribers.retain(|s| s.0 != m);
  }
}

/// Files listen for keys, related to searches.
impl KeySink for File {
  fn handle_key(
    &mut self,
    desc: &Description,
    drone: &Drone,
    e: &glfw::WindowEvent,
  ) -> CallbackResult {
    if match e {
      WindowEvent::Key(Key::N, _, Action::Press, m) if m.is_empty() => {
        self.find_next(desc, drone)
      }
      WindowEvent::Key(Key::N, _, Action::Press, Modifiers::Shift) => {
        self.find_prev(desc, drone)
      }
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        self.clear_search(desc, drone)
      }
      _ => false,
    } {
      CallbackResult::None
    } else {
      CallbackResult::Pass
    }
  }
}
