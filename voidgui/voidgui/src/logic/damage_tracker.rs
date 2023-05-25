use std::io::Write;

use glfw::{Action, Key, Modifiers, WindowEvent};
use serde::Serialize;
use slice_group_by::GroupBy;

use crate::{
  data::{File, GenericFile, Record, Tag},
  logic::ring::Mark,
  render::painter::Drone,
  widgets::traits::KeySink,
  Description,
};

use super::{
  ring::{self, Wrap},
  CallbackResult,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Damage {
  None(i64),
  Update(i64, usize, String, String),
  Add(i64),
  Remove(i64),
}

static DAMAGE_ZERO: Damage = Damage::None(0);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tagged(Tag, Damage);

impl Tagged {
  fn map<F: Fn(&Damage) -> Damage>(&self, f: F) -> Tagged {
    Self(self.0, f(&self.1))
  }
}

/// Describes a change that is going to happen once the damage object is
/// acted on.
impl Damage {
  pub fn invert(self) -> Self {
    match self {
      Damage::Update(uid, n, from, to) => Damage::Update(uid, n, to, from),
      Damage::Add(uid) => Damage::Remove(uid),
      Damage::Remove(uid) => Damage::Add(uid),
      Damage::None(uid) => Damage::None(uid),
    }
  }

  pub fn inverted(&self) -> Self {
    match self {
      Damage::Update(uid, n, from, to) => {
        Damage::Update(*uid, *n, to.clone(), from.clone())
      }
      Damage::Add(uid) => Damage::Remove(*uid),
      Damage::Remove(uid) => Damage::Add(*uid),
      Damage::None(uid) => Damage::None(*uid),
    }
  }

  pub fn uid(&self) -> i64 {
    match self {
      Damage::Update(uid, _, _, _) => *uid,
      Damage::Add(uid) => *uid,
      Damage::Remove(uid) => *uid,
      Damage::None(uid) => *uid,
    }
  }

  /// Attempts to join two consequitve operations.
  /// Will panic when passed an invalid sequence.
  /// May not preserve uid if [Damage::None] is returned.
  pub fn join<'a>(&'a self, other: &'a Self) -> &'a Self {
    match self {
      Damage::None(_) => other,
      Damage::Update(_, _, _, _) => match other {
        Damage::None(_) => self,
        Damage::Update(_, _, _, _) => self,
        Damage::Add(_) => panic!("invalid sequence"),
        Damage::Remove(_) => other,
      },
      Damage::Add(_) => match other {
        Damage::None(_) => self,
        Damage::Update(_, _, _, _) => self,
        Damage::Add(_) => panic!("invalid sequence"),
        Damage::Remove(_) => &DAMAGE_ZERO,
      },
      Damage::Remove(_) => match other {
        Damage::None(_) => self,
        Damage::Update(_, _, _, _) => panic!("invalid sequence"),
        Damage::Add(_) => other,
        Damage::Remove(_) => panic!("invalid sequence"),
      },
    }
  }
}

impl Into<CallbackResult> for Tagged {
  fn into(self) -> CallbackResult {
    CallbackResult::File(
      self.0,
      match self.1 {
        Damage::Update(uid, n, _, to) => Box::new(
          move |f: Wrap<dyn GenericFile>, desc: &Description, drone: &Drone| {
            f.write().unwrap().set(desc, drone, uid, n, &to);
          },
        ),
        Damage::Add(uid) => Box::new(
          move |f: Wrap<dyn GenericFile>, desc: &Description, drone: &Drone| {
            f.write().unwrap().new_row(desc, drone, uid);
          },
        ),
        Damage::Remove(uid) => Box::new(
          move |f: Wrap<dyn GenericFile>, desc: &Description, drone: &Drone| {
            f.write().unwrap().rem(desc, drone, uid);
          },
        ),
        Damage::None(_) => panic!(),
      },
    )
  }
}

/// Damage, compressed and optimised for export.
#[derive(Serialize)]
pub enum Scar<R: Record> {
  Update(R),
  Insert(R),
  Remove(i64),
}

impl<R: Record> Scar<R> {
  pub fn compress(damage: &mut Vec<Damage>, f: &File<R>) -> Vec<Self> {
    damage.sort_by_key(|x| x.uid());
    damage
      .linear_group_by_key(|x| x.uid())
      .map(|xs| xs.into_iter().reduce(|x, y| x.join(y)).unwrap())
      .filter_map(|d| match d {
        Damage::None(_) => None,
        Damage::Update(uid, _, _, _) => {
          Some(Self::Update(f.read().record(*uid).unwrap()))
        }
        Damage::Add(uid) => Some(Self::Insert(f.read().record(*uid).unwrap())),
        Damage::Remove(uid) => Some(Self::Remove(*uid)),
      })
      .collect()
  }
}

pub struct DamageTracker {
  undo: Vec<Tagged>,
  redo: Vec<Tagged>,
  pending: Vec<Tagged>,
  target: Tag,
}

impl DamageTracker {
  pub fn new() -> Self {
    Self {
      undo: vec![],
      redo: vec![],
      pending: vec![],
      target: Tag::default(),
    }
  }

  pub fn undo(&mut self) -> CallbackResult {
    pop(&mut self.undo, &mut self.redo)
  }

  pub fn redo(&mut self) -> CallbackResult {
    pop(&mut self.redo, &mut self.undo)
  }

  pub fn drain<'a>(&'a mut self) -> impl Iterator<Item = CallbackResult> + 'a {
    self.pending.drain(..).map(|d| {
      self.undo.push(d.map(Damage::inverted));
      d.into()
    })
  }

  pub fn push(&mut self, d: Damage) {
    self.pending.push(Tagged(self.target, d));
  }

  pub fn serialize<W: Write, R: Record>(&self, w: &mut W, f: &File<R>) {
    // let file = std::fs::File::create("obj.cbor").unwrap();
    // ciborium::ser::into_writer(
    //   &Scar::<E>::compress(
    //     &mut self.undo.iter().map(|x| x.inverted()).collect(),
    //     ss,
    //   ),
    //   file,
    // )
    // .unwrap();
    ciborium::ser::into_writer(
      &Scar::<R>::compress(
        &mut self
          .undo
          .iter()
          .map(|x| x.map(Damage::inverted).1)
          .collect(),
        f,
      ),
      w,
    )
    .unwrap();
  }

  pub fn wipe(&mut self) {
    self.undo.clear();
    self.redo.clear();
    self.pending.clear();
  }

  pub fn push_to_ring(self, ring: &mut crate::logic::Ring) -> Wrap<Self> {
    let rc = ring::wrap(self);
    ring.push_key_sink(rc.clone(), Mark::Spreadsheet);
    rc
  }

  pub fn set_target(&mut self, target: Tag) {
    self.target = target;
  }

  pub fn target(&self) -> u32 {
    self.target
  }
}

impl KeySink for DamageTracker {
  fn handle_key(
    &mut self,
    _: &Description,
    _: &crate::render::painter::Drone,
    e: &WindowEvent,
  ) -> CallbackResult {
    match e {
      WindowEvent::Key(Key::Z, _, Action::Press, Modifiers::Control) => {
        self.undo()
      }
      WindowEvent::Key(Key::Z, _, Action::Press, m)
        if *m == Modifiers::Control | Modifiers::Shift =>
      {
        self.redo()
      }
      _ => CallbackResult::Pass,
    }
  }
}

fn pop(src: &mut Vec<Tagged>, dst: &mut Vec<Tagged>) -> CallbackResult {
  let d = src.pop();
  match d {
    Some(x) => {
      dst.push(x.clone().map(Damage::inverted));
      x.into()
    }
    None => CallbackResult::None,
  }
}

#[cfg(test)]
mod test_damage {
  use super::*;

  #[test]
  fn invert() {
    let d = Damage::Update(1, 0, "hi".to_string(), "hello".to_string());
    assert_eq!(d, d.clone().invert().invert());
    let d = Damage::Add(1);
    assert_eq!(d, d.clone().invert().invert());
    let d = Damage::Remove(1);
    assert_eq!(d, d.clone().invert().invert());
    let d = Damage::None(1);
    assert_eq!(d, d.clone().invert().invert());
  }

  #[test]
  fn join() {
    let u = Damage::Update(1, 0, "hi".to_string(), "hello".to_string());
    let a = Damage::Add(1);
    let r = Damage::Remove(1);

    assert_eq!(
      [a.clone(), u.clone(), r.clone(), a.clone()]
        .into_iter()
        .scan(Damage::None(1), |s, x| {
          *s = s.join(&x).clone();
          Some(s.clone())
        })
        .map(|r| r.clone())
        .collect::<Vec<Damage>>(),
      vec![
        Damage::Add(1),
        Damage::Add(1),
        Damage::None(0),
        Damage::Add(1),
      ]
    );

    assert_eq!(
      [u.clone(), r, a]
        .iter()
        .cloned()
        .scan(Damage::None(1), |s, x| {
          *s = s.join(&x).clone();
          Some(s.clone())
        })
        .map(|r| r.clone())
        .collect::<Vec<Damage>>(),
      vec![u.clone(), Damage::Remove(1), Damage::Add(1)]
    );
  }
}

#[cfg(test)]
mod test_damage_tracker {
  use super::*;

  #[test]
  fn drain_undo_redo() {
    let mut t = DamageTracker::new();
    t.set_target(0);

    let d1 = Damage::Update(0, 0, "hi".to_string(), "hello".to_string());
    let d2 = Damage::Update(3, 1, "ops".to_string(), "oops".to_string());
    let d1r = d1.clone().invert();
    let d2r = d2.clone().invert();

    let t1 = Tagged(0, d1.clone());
    let t2 = Tagged(0, d2.clone());
    let t1r = Tagged(0, d1r.clone());
    let t2r = Tagged(0, d2r.clone());

    t.push(d1r.clone());
    t.push(d2r.clone());
    assert_eq!(t.pending, vec![t1r.clone(), t2r.clone()], "`push` works");

    {
      let mut iter = t.drain();
      assert!(matches!(iter.next(), Some(CallbackResult::File(0, _))));
      assert!(matches!(iter.next(), Some(CallbackResult::File(0, _))));
      assert!(matches!(iter.next(), None,));
    }
    assert_eq!(t.pending, vec![], "`drain` flushes pending");
    assert_eq!(
      t.undo,
      vec![t1.clone(), t2.clone()],
      "`drain` fills undo buffer"
    );

    t.undo();
    assert_eq!(t.undo, vec![t1.clone()], "`undo` pops from undo buffer");
    assert_eq!(t.redo, vec![t2r.clone()], "`undo` fills redo buffer");

    assert!(
      !matches!(t.undo(), CallbackResult::None),
      "unempty undo buffer doesn't yield None"
    );
    assert!(
      matches!(t.undo(), CallbackResult::None),
      "empty undo buffer yields None"
    );
    assert_eq!(
      t.redo,
      vec![t2r.clone(), t1r.clone()],
      "redo buffer is filled in reverse order by undo"
    );

    t.redo();
    assert_eq!(t.redo, vec![t2r.clone()], "`redo` pops from redo buffer");
    assert_eq!(t.undo, vec![t1.clone()], "`redo` fills undo buffer");

    assert!(
      !matches!(t.redo(), CallbackResult::None),
      "unempty redo buffer doesn't yield None"
    );
    assert!(
      matches!(t.redo(), CallbackResult::None),
      "empty redo buffer yields None"
    );
    assert_eq!(
      t.undo,
      vec![t1.clone(), t2.clone()],
      "undo buffer is filled correctly by redo"
    );
  }
}
