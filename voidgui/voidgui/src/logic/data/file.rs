use crate::{
  render::painter::Drone,
  widgets::traits::{Drawable, KeySink},
  CallbackResult, Description, Mark, Wrap,
};

use super::{Data, Dataset, GenericDataset, Record, Tag};

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
  fn set_raw(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    field: usize,
    value: Data,
  );
  fn new_row(&mut self, desc: &Description, drone: &Drone, uid: i64);
  fn rem(&mut self, desc: &Description, drone: &Drone, uid: i64);

  /// Get a [CallbackResult] that, once handled, fills first FKeys in the table
  /// with appropriate value from a target dataset.
  fn join(&self, tag: Tag, uid: i64) -> CallbackResult;

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
      s.write().unwrap().on_set(
        desc,
        drone,
        uid,
        field,
        self.data.get(uid, field).unwrap(),
      )
    });
  }

  fn set_raw(
    &mut self,
    desc: &Description,
    drone: &Drone,
    uid: i64,
    field: usize,
    value: Data,
  ) {
    self.data.set_raw(uid, field, value);
    self.subscribers.iter().for_each(|(_, s)| {
      s.write().unwrap().on_set(
        desc,
        drone,
        uid,
        field,
        self.data.get(uid, field).unwrap(),
      )
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

  fn join(&self, tag: Tag, uid: i64) -> CallbackResult {
    let tgt = self
      .data
      .datatypes()
      .iter()
      .enumerate()
      .find_map(|(ind, t)| match t {
        super::Datatype::FKey(t, f) => Some((*t, *f, ind)),
        _ => None,
      });
    if tgt.is_none() {
      return CallbackResult::None;
    }
    // (target tag, target field, own field)
    let tgt = tgt.unwrap();
    let tgt_uid = match self.data.raw(uid, tgt.2).unwrap() {
      super::Data::FKey(None) => return CallbackResult::Pass,
      super::Data::FKey(Some(uid)) => uid,
      _ => panic!(),
    };

    CallbackResult::Read(
      tgt.0,
      Box::new(
        move |_: Option<Wrap<dyn Drawable>>,
              f: Option<Wrap<dyn GenericFile>>,
              _: &Description,
              _: &Drone| {
          let f = f.unwrap();
          let f = f.try_write().ok();
          if f.is_none() {
            return "can't lock file".into();
          }
          let f = f.unwrap();
          let val = f.read().get(tgt_uid, tgt.1).unwrap().to_string();

          CallbackResult::File(
            tag,
            Box::new(
              move |f: Wrap<dyn GenericFile>,
                    desc: &Description,
                    drone: &Drone| {
                f.write().unwrap().set(desc, drone, uid, tgt.2, &val);
              },
            ),
          )
        },
      ),
    )
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
      glfw::WindowEvent::Key(glfw::Key::N, _, glfw::Action::Press, m)
        if m.is_empty() =>
      {
        self.find_next(desc, drone)
      }
      glfw::WindowEvent::Key(
        glfw::Key::N,
        _,
        glfw::Action::Press,
        glfw::Modifiers::Shift,
      ) => self.find_prev(desc, drone),
      glfw::WindowEvent::Key(glfw::Key::Escape, _, glfw::Action::Press, _) => {
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
