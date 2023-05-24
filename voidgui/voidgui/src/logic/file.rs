use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::{render::painter::Drone, widgets::traits::KeySink, Description};

use super::{CallbackResult, Dataset, GenericDataset, Mark, Record, Wrap};

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

/// Used for dynamic dispatch without knowing `R` record type.
/// Most file operations don't require knowledge of concrete Record type
/// and can be abstracted.
/// If your operations concerns the record, use [file::dcast]
pub trait GenericFile {
  fn drop(&mut self, desc: &Description, drone: &Drone);
  fn read(&self) -> &dyn GenericDataset;

  fn set(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    field: usize,
    value: &str,
  );
  fn new_row(&mut self, desc: &Description, drone: &Drone, uid: i64);
  fn rem(&mut self, desc: &Description, drone: &Drone, uid: i64);

  fn new_search(&mut self, desc: &Description, drone: &Drone, value: &str);
  fn clear_search(&mut self, desc: &Description, drone: &Drone) -> bool;
  fn find_prev(&mut self, desc: &Description, drone: &Drone) -> bool;
  fn find_next(&mut self, desc: &Description, drone: &Drone) -> bool;

  fn subscriber(&mut self, s: Wrap<dyn FileSub>, m: Mark);
  fn unsubscribe(&mut self, m: Mark);
}

impl<R: Record> GenericFile for File<R> {
  fn drop(&mut self, desc: &Description, drone: &Drone) {
    self.data.clear();
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_drop(desc, drone));
  }

  fn read(&self) -> &dyn GenericDataset {
    self.read()
  }

  fn set(
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

  fn new_row(&mut self, desc: &Description, drone: &Drone, uid: i64) {
    self.data.new_row(uid);
    let row = self.data.row(uid);
    row.map(|row| {
      self.subscribers.iter().for_each(|(_, s)| {
        s.write().unwrap().on_put(desc, drone, uid, row.as_slice())
      });
    });
  }

  fn rem(&mut self, desc: &Description, drone: &Drone, uid: i64) {
    self.data.rem(uid);
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_rem(desc, drone, uid));
  }

  fn new_search(&mut self, desc: &Description, drone: &Drone, value: &str) {
    self.data.new_search(value);
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_new_search(desc, drone));
    self.find_next(desc, drone);
  }

  fn clear_search(&mut self, desc: &Description, drone: &Drone) -> bool {
    self
      .subscribers
      .iter()
      .for_each(|(_, s)| s.write().unwrap().on_drop_search(desc, drone));
    self.data.clear_search()
  }

  fn find_prev(&mut self, desc: &Description, drone: &Drone) -> bool {
    let res = self.data.find_prev();
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_search_update(desc, drone, &res)
    });
    res.is_some()
  }

  fn find_next(&mut self, desc: &Description, drone: &Drone) -> bool {
    let res = self.data.find_next();
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_search_update(desc, drone, &res)
    });

    res.is_some()
  }

  fn subscriber(&mut self, s: Wrap<dyn FileSub>, m: Mark) {
    self.subscribers.push((m, s));
  }

  fn unsubscribe(&mut self, m: Mark) {
    self.subscribers.retain(|s| s.0 != m);
  }
}

/// [File] controls access to [Dataset], notifying subscribers
/// on any updates.
pub struct File<R: Record> {
  data: Dataset<R>,
  subscribers: Vec<(Mark, Wrap<dyn FileSub>)>,
}

pub type FileCallback =
  Box<dyn FnOnce(Wrap<dyn GenericFile>, &Description, &Drone)>;

impl<R: Record> File<R> {
  pub fn new() -> Self {
    Self {
      data: Dataset::new(),
      subscribers: vec![],
    }
  }

  pub fn read(&self) -> &Dataset<R> {
    &self.data
  }

  pub fn put(&mut self, desc: &Description, drone: &Drone, x: R) {
    let uid = *x.uid();
    self.data.put(x);
    let row = self.data.row(uid);
    row.map(|row| {
      self.subscribers.iter().for_each(|(_, s)| {
        s.write().unwrap().on_put(desc, drone, uid, row.as_slice())
      });
    });
  }
}

/// Files listen for keys, related to searches.
impl<R: Record + 'static> KeySink for File<R> {
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
