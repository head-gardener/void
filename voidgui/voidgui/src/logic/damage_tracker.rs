use std::{cell::RefCell, rc::Rc};

use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::{
  logic::ring::Mark,
  render::painter::Painter,
  widgets::{
    traits::{CallbackResult, Drawable, KeySink},
    Spreadsheet,
  },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Damage {
  Update(usize, String, String),
}

/// Describes a change that is going to happen once the damage object is
/// acted on.
impl Damage {
  pub fn invert(self) -> Self {
    match self {
      Damage::Update(n, from, to) => Damage::Update(n, to, from),
    }
  }
}

impl Into<CallbackResult> for Damage {
  fn into(self) -> CallbackResult {
    match self {
      Damage::Update(u, _, to) => CallbackResult::Modify(
        Mark::Spreadsheet,
        with_spreadsheet(move |p: &Painter, s: &mut Spreadsheet| {
          s.update_record(p, u, &to).unwrap();
        }),
      ),
    }
  }
}

fn with_spreadsheet(
  f: impl FnOnce(&Painter, &mut Spreadsheet) + 'static,
) -> Box<dyn FnOnce(Option<Rc<RefCell<dyn Drawable>>>, &Painter)> {
  Box::new(move |s, p| {
    s.expect("Spreadsheet should always be in the ring")
      .borrow_mut()
      .downcast_mut()
      .map(|s| f(p, s));
  })
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
}

impl KeySink for DamageTracker {
  fn handle_key(
    &mut self,
    _: &crate::render::painter::Painter,
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
    let d1 = Damage::Update(0, "hi".to_string(), "hello".to_string());
    assert_eq!(d1, d1.clone().invert().invert())
  }

  #[test]
  fn tracker_state() {
    let mut t = DamageTracker::new();

    let d1 = Damage::Update(0, "hi".to_string(), "hello".to_string());
    let d2 = Damage::Update(3, "ops".to_string(), "oops".to_string());
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
