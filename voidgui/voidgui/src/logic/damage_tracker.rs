use std::io::Write;

use glfw::{Action, Key, Modifiers, WindowEvent};
use serde::Serialize;

use crate::{
  logic::ring::Mark,
  render::painter::{Description, Drone},
  widgets::{traits::KeySink, Spreadsheet},
};

use super::{
  ring::{self, Wrap},
  CallbackResult,
};

#[derive(Serialize, Clone, Debug, PartialEq, Eq)]
pub enum Damage {
  Update(u64, usize, String, String),
  Add(u64),
  Remove(u64),
}

/// Describes a change that is going to happen once the damage object is
/// acted on.
impl Damage {
  pub fn invert(self) -> Self {
    match self {
      Damage::Update(uuid, n, from, to) => Damage::Update(uuid, n, to, from),
      Damage::Add(uuid) => Damage::Remove(uuid),
      Damage::Remove(uuid) => Damage::Add(uuid),
    }
  }
}

impl Into<CallbackResult> for Damage {
  fn into(self) -> CallbackResult {
    CallbackResult::Modify(
      Mark::Spreadsheet,
      match self {
        Damage::Update(uuid, n, _, to) => ring::with_spreadsheet(
          move |desc: &Description, drone: &Drone, s: &mut Spreadsheet| {
            s.update_record(desc, drone, uuid, n, &to).unwrap();
          },
        ),
        Damage::Add(uuid) => ring::with_spreadsheet(
          move |desc: &Description, drone: &Drone, s: &mut Spreadsheet| {
            s.add_record(desc, drone, uuid).unwrap();
          },
        ),
        Damage::Remove(uuid) => ring::with_spreadsheet(
          move |_: &Description, drone: &Drone, s: &mut Spreadsheet| {
            s.rem_record(drone, uuid).unwrap();
          },
        ),
      },
    )
  }
}

pub struct DamageTracker {
  undo: Vec<Damage>,
  redo: Vec<Damage>,
  pending: Vec<Damage>,
}

impl DamageTracker {
  pub fn new() -> Self {
    Self {
      undo: vec![],
      redo: vec![],
      pending: vec![],
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
      self.undo.push(d.clone().invert());
      d.into()
    })
  }

  pub fn push(&mut self, d: Damage) {
    self.pending.push(d);
  }

  pub fn serialize<W: Write>(&self, w: &mut W) {
    // let file = std::fs::File::create("obj.cbor").unwrap();
    // ciborium::ser::into_writer(&self.undo, file).unwrap();
    ciborium::ser::into_writer(&self.undo, w).unwrap();
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

fn pop(src: &mut Vec<Damage>, dst: &mut Vec<Damage>) -> CallbackResult {
  let d = src.pop();
  match d {
    Some(x) => {
      dst.push(x.clone().invert());
      x.into()
    }
    None => CallbackResult::None,
  }
}

#[cfg(test)]
mod test_damage_tracker {
  use super::*;

  #[test]
  fn damage() {
    let d1 = Damage::Update(0, 0, "hi".to_string(), "hello".to_string());
    assert_eq!(d1, d1.clone().invert().invert())
  }

  #[test]
  fn tracker_state() {
    let mut t = DamageTracker::new();

    let d1 = Damage::Update(0, 0, "hi".to_string(), "hello".to_string());
    let d2 = Damage::Update(3, 1, "ops".to_string(), "oops".to_string());
    let d1r = d1.clone().invert();
    let d2r = d2.clone().invert();
    t.push(d1r.clone());
    t.push(d2r.clone());
    assert_eq!(t.pending, vec![d1r.clone(), d2r.clone()], "`push` works");

    {
      let mut iter = t.drain();
      assert!(matches!(
        iter.next(),
        Some(CallbackResult::Modify(Mark::Spreadsheet, _))
      ));
      assert!(matches!(
        iter.next(),
        Some(CallbackResult::Modify(Mark::Spreadsheet, _))
      ));
      assert!(matches!(iter.next(), None,));
    }
    assert_eq!(t.pending, vec![], "`drain` flushes pending");
    assert_eq!(
      t.undo,
      vec![d1.clone(), d2.clone()],
      "`drain` fills undo buffer"
    );

    t.undo();
    assert_eq!(t.undo, vec![d1.clone()], "`undo` pops from undo buffer");
    assert_eq!(t.redo, vec![d2r.clone()], "`undo` fills redo buffer");

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
      vec![d2r.clone(), d1r.clone()],
      "redo buffer is filled in reverse order by undo"
    );

    t.redo();
    assert_eq!(t.redo, vec![d2r.clone()], "`redo` pops from redo buffer");
    assert_eq!(t.undo, vec![d1.clone()], "`redo` fills undo buffer");

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
      vec![d1.clone(), d2.clone()],
      "undo buffer is filled correctly by redo"
    );
  }
}
