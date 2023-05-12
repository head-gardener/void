pub struct Buffer<T> (Vec<Option<T>>);

impl<T> Buffer<T> {
  pub fn new() -> Self {
    Self(vec![])
  }

  pub fn get(&mut self, n: usize) -> &mut T {
    self.0[n].as_mut().unwrap()
  }

  pub fn put(&mut self, x: T) -> usize {
    self.0.push(Some(x));
    self.0.len() - 1
  }

  pub fn del(&mut self, n: usize) {
    self.0[n] = None
  }
}
