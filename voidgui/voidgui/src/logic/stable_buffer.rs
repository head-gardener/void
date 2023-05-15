pub struct StableBuffer<T>(Vec<Option<T>>);

impl<T> StableBuffer<T> {
  pub fn new() -> Self {
    Self(vec![])
  }

  pub fn get(&mut self, n: usize) -> &mut T {
    self.0[n].as_mut().unwrap()
  }

  pub fn put(&mut self, x: T) -> usize {
    if let Some((i, o)) =
      self.0.iter_mut().enumerate().find(|(_, o)| o.is_none())
    {
      *o = Some(x);
      i
    } else {
      self.0.push(Some(x));
      self.0.len() - 1
    }
  }

  pub fn del(&mut self, n: usize) {
    self.0[n] = None
  }

  pub fn has(&self, n: usize) -> bool {
    self.0.get(n).and_then(|o| o.as_ref()).is_some()
  }

  pub fn iter(&self) -> impl Iterator<Item = Option<&T>> + '_ {
    self.0.iter().map(|x| x.as_ref())
  }

  pub fn unwrapped_iter(&self) -> impl Iterator<Item = &T> + '_ {
    self.0.iter().filter_map(|x| x.as_ref())
  }
}
