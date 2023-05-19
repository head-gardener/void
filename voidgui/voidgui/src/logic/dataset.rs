type Search = Option<(String, usize)>;

pub struct Dataset {
  records: Vec<Box<(String, String)>>,
  search: Search,
}

impl Dataset {
  pub fn new() -> Self {
    Self {
      records: vec![],
      search: None,
    }
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

  pub fn new_search(&mut self, value: &str) {
    self.search = Some((value.to_string().to_lowercase(), 0));
  }

  pub fn clear_search(&mut self) -> bool {
    let res = self.search.is_some();
    self.search = None;
    res
  }

  pub fn find_prev(&mut self) -> Option<usize> {
    self.search.as_mut().and_then(|(s, i)| {
      self
        .records
        .iter()
        .rev()
        .chain(self.records.iter().rev())
        .skip(self.records.len() - *i + 1)
        .position(|b| b.1.contains(s.as_str()))
        .map(|r| {
          if r >= *i {
            *i += self.records.len() + 1
          }
          *i -= r + 1;
          *i - 1
        })
    })
  }

  pub fn find_next(&mut self) -> Option<usize> {
    self.search.as_mut().and_then(|(s, i)| {
      self
        .records
        .iter()
        .chain(self.records.iter())
        .skip(*i)
        .position(|b| b.1.contains(s.as_str()))
        .map(|r| {
          *i += r + 1;
          if *i > self.records.len() {
            *i -= self.records.len()
          }
          *i - 1
        })
    })
  }

  pub fn cut(&mut self, ind: usize, n: usize) {
    self.records.drain(ind..ind + n);
  }
}

#[cfg(test)]
mod test_dataset {
  use super::*;

  #[test]
  fn basics() {
    let mut d = Dataset::new();
    d.push("hi");
    assert_eq!(d.get(0), "hi");
    d.set(0, "hello");
    assert_eq!(d.get(0), "hello");
    d.clear();
    d.push("yup");
    assert_eq!(d.get(0), "yup");
  }

  #[test]
  fn cut() {
    let mut d = Dataset::new();
    ["hello", "hi", "Hey", "ahead"]
      .into_iter()
      .for_each(|x| d.push(x));

    d.cut(1, 2);
    assert_eq!(d.get(1), "ahead");
  }

  #[test]
  fn search_basics() {
    let mut d = Dataset::new();

    // no search - no finds
    assert_eq!(d.find_next(), None);
    assert_eq!(d.find_prev(), None);

    ["hello"].into_iter().for_each(|x| d.push(x));
    d.new_search("He");

    // next works
    assert_eq!(d.find_next(), Some(0));
    d.clear_search();

    // no search - no finds
    assert_eq!(d.find_next(), None);
    assert_eq!(d.find_prev(), None);
  }

  #[test]
  fn search_good() {
    let mut d = Dataset::new();

    ["hello", "hi", "Hey", "ahead"]
      .into_iter()
      .for_each(|x| d.push(x));
    d.new_search("He");

    // next works
    assert_eq!(d.find_next(), Some(0));
    assert_eq!(d.find_next(), Some(2));
    assert_eq!(d.find_next(), Some(3));

    // prev works
    assert_eq!(d.find_prev(), Some(2));
    assert_eq!(d.find_next(), Some(3));
  }

  #[test]
  fn search_good_wrap() {
    let mut d = Dataset::new();

    ["hello", "hi", "Hey", "ahead"]
      .into_iter()
      .for_each(|x| d.push(x));
    d.new_search("He");

    assert_eq!(d.find_prev(), Some(3), "find_prev should wrap");
    assert_eq!(d.find_next(), Some(0), "find_next should wrap");
  }

  #[test]
  fn search_bad() {
    let mut d = Dataset::new();

    ["hello", "hi", "Hey", "ahead"]
      .into_iter()
      .for_each(|x| d.push(x));
    d.new_search("miss");

    assert_eq!(d.find_prev(), None);
    assert_eq!(d.find_prev(), None);
  }
}
