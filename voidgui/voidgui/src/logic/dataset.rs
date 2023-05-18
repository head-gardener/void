pub struct Dataset {
  records: Vec<Box<(String, String)>>,
}

impl Dataset {
  pub fn new() -> Self {
    Self { records: vec![] }
  }

  pub fn clear(&mut self) {
    self.records.clear()
  }

  pub fn push(&mut self, value: &str) {
    self.records.push(Box::new((
      value.to_owned(),
      value.to_owned().to_lowercase(),
    )));
  }

  pub fn set(&mut self, n: usize, value: &str) {
    self.records[n].0 = value.to_owned();
    self.records[n].1 = value.to_owned().to_lowercase();
  }

  pub fn get(&self, n: usize) -> &str {
    &self.records[n].0
  }
}
