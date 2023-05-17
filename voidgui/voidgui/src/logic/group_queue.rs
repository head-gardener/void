use std::{collections::VecDeque, string::Drain};

/// Vector that groups all elements by equality on push
pub struct GroupQueue<T>(VecDeque<T>);

impl<T: PartialEq> GroupQueue<T> {
  pub fn new() -> Self {
    Self(VecDeque::new())
  }

  pub fn push(&mut self, t: T) {
    if let Some(p) = self.0.iter().rev().position(|x| t.eq(x)) {
      self.0.insert(self.0.len() - p, t);
    } else {
      self.0.push_back(t);
    }
  }

  pub fn pop(&mut self) -> Option<T> {
    self.0.pop_front()
  }

  pub fn drain(&mut self) -> std::collections::vec_deque::Drain<T> {
    self.0.drain(0..)
  }
}

#[cfg(test)]
mod test_group_vec {
  use super::*;

  #[derive(Debug)]
  enum E {
    A,
    A1,
    A2,
    B,
  }

  impl PartialEq for E {
    fn eq(&self, other: &Self) -> bool {
      match self {
        E::A | E::A1 | E::A2 => matches!(other, E::A | E::A1 | E::A2),
        E::B => matches!(other, E::B),
      }
    }
  }

  impl E {
    fn exact(&self, other: &Self) -> bool {
      match self {
        E::A => matches!(other, E::A),
        E::A1 => matches!(other, E::A1),
        E::A2 => matches!(other, E::A2),
        E::B => matches!(other, E::B),
      }
    }
  }

  #[test]
  fn simple_add() {
    let mut q = GroupQueue::new();
    [E::B, E::A, E::A, E::B].into_iter().for_each(|x| q.push(x));

    assert_eq!(q.0, [E::B, E::B, E::A, E::A]);
  }

  #[test]
  fn order_preservation() {
    let mut q = GroupQueue::new();
    [E::B, E::A, E::A1, E::A2, E::B]
      .into_iter()
      .for_each(|x| q.push(x));

    q.0
      .iter()
      .zip([E::B, E::B, E::A, E::A1, E::A2].into_iter())
      .for_each(|(a, b)| {
        assert!(a.exact(&b), "{:?} should preserve order", q.0)
      });
  }
}
