use crate::data::{Data, Datatype, Record};
use std::cmp::max;

type Search = Option<(String, usize)>;

pub trait GenericDataset {
  /// Get `field` of record with `uid` as `&str`.
  fn get(&self, uid: i64, field: usize) -> Option<&str>;

  /// Get `field` of record with `uid` as [Data].
  fn raw(&self, uid: i64, field: usize) -> Option<Data>;

  /// Get `field` of all records as `&str`.
  fn column(&self, field: usize) -> Option<Vec<(i64, &str)>>;

  /// Get row under `uid`.
  fn row(&self, uid: i64) -> Option<Vec<&str>>;

  /// Returns a reference to the datatypes of this [`Dataset`].
  fn datatypes(&self) -> &[Datatype];
}

impl<R: Record> GenericDataset for Dataset<R> {
  fn get(&self, uid: i64, field: usize) -> Option<&str> {
    self
      .find(uid)
      .and_then(|(_, b)| b.0.get(field).map(|p| p.0.as_str()))
  }

  fn raw(&self, uid: i64, field: usize) -> Option<Data> {
    self.find(uid).and_then(|(_, b)| b.1.get_nth(field))
  }

  fn column(&self, field: usize) -> Option<Vec<(i64, &str)>> {
    self.datatypes().iter().nth(field)?;
    Some(
      self
        .records
        .iter()
        .map(|(uid, b)| (*uid, b.0[field].0.as_str()))
        .collect(),
    )
  }

  fn row(&self, uid: i64) -> Option<Vec<&str>> {
    self
      .find(uid)
      .map(|(_, b)| b.0.iter().map(|(s, _)| s.as_str()).collect())
  }

  fn datatypes(&self) -> &[Datatype] {
    self.datatypes.as_ref()
  }
}

/// Searchable generic data storage.
pub struct Dataset<R: Record> {
  records: Vec<(i64, Box<(Vec<(String, String)>, R)>)>,
  search: Search,
  datatypes: Vec<Datatype>,
}

impl<R: Record> Dataset<R> {
  pub fn new() -> Self {
    Self {
      datatypes: R::datatypes(),
      records: vec![],
      search: None,
    }
  }

  pub fn clear(&mut self) {
    self.records.clear()
  }

  /// Put a record in the dataset.
  pub fn put(&mut self, r: R) {
    let fs = (0..)
      .map(|n| r.get_nth(n))
      .take_while(|o| o.is_some())
      .filter_map(|o| o)
      .map(|d| d.to_string())
      .map(|s| {
        let lc = s.to_lowercase();
        (s, lc)
      })
      .collect();
    self.records.push((*r.uid(), Box::new((fs, r))));
  }

  /// Insert new default record.
  pub fn new_row(&mut self, uid: i64) {
    let mut r = R::default();
    r.set_uid(uid);
    self.put(r);
  }

  /// Set `field` of record with `uid` to `value`.
  /// This function is 'shallow': FKey values won't be overwritten, only their
  /// representation. For 'deep' alternative see [Dataset::set_raw]
  pub fn set(&mut self, uid: i64, field: usize, value: &str) {
    self
      .records
      .iter_mut()
      .find(|(u, _)| *u == uid)
      .iter_mut()
      .for_each(|(_, b)| {
        if b.0.get(field).is_some() {
          if !self.datatypes[field].is_fkey() {
            b.1.set_nth_str(field, value);
          }
          b.0[field] = (value.to_string(), value.to_lowercase());
        }
      });
  }

  pub fn set_raw(&mut self, uid: i64, field: usize, value: Data) {
    self
      .records
      .iter_mut()
      .find(|(u, _)| *u == uid)
      .iter_mut()
      .for_each(move |(_, b)| {
        if b.0.get(field).is_some() {
          b.1.set_nth_raw(field, value.clone());
          let s = value.to_string();
          let lc = (&s).to_lowercase();
          b.0[field] = (s, lc);
        }
      });
  }

  /// Remove record under `uid`.
  pub fn rem(&mut self, uid: i64) {
    self.records.retain(|(u, _)| *u != uid);
  }

  /// Start new search.
  pub fn new_search(&mut self, value: &str) {
    self.search = Some((value.to_string().to_lowercase(), 0));
  }

  /// Get record under `uid`.
  pub fn record(&self, uid: i64) -> Option<R> {
    self.find(uid).map(|(_, b)| b.1.clone())
  }

  /// Remove search.
  pub fn clear_search(&mut self) -> bool {
    let res = self.search.is_some();
    self.search = None;
    res
  }

  /// Find previous occurance of search pattern. Normally wouldn't be called
  /// immidiately after initializing the search.
  pub fn find_prev(&mut self) -> Option<(i64, usize)> {
    let w = self.datatypes.len();
    let (s, i) = self.search.as_mut()?;
    self
      .records
      .iter()
      .rev()
      .chain(self.records.iter().rev())
      .map(|(_, b)| b.0.iter().rev())
      .flatten()
      .skip(if *i == 0 {
        0
      } else {
        self.records.len() * w - *i + 1
      })
      .position(|b| b.1.contains(s.as_str()))
      .map(|r| {
        if r + 1 >= *i {
          *i = max(*i, 1);
          *i += self.records.len() * w
        }
        *i -= r + 1;
        *i - 1
      })
      .map(|i| (self.records[i / w].0, i % w))
  }

  /// Find next occurance of search pattern. Normally would be called
  /// immidiately after initializing the search.
  pub fn find_next(&mut self) -> Option<(i64, usize)> {
    let w = self.datatypes.len();
    let (s, i) = self.search.as_mut()?;
    self
      .records
      .iter()
      .chain(self.records.iter())
      .map(|(_, b)| b.0.iter())
      .flatten()
      .skip(*i)
      .position(|b| b.1.contains(s.as_str()))
      .map(|r| {
        *i += r + 1;
        if *i > self.records.len() * w {
          *i -= self.records.len() * w
        }
        *i - 1
      })
      .map(|i| (self.records[i / w].0, i % w))
  }

  /// Helper function for selecting row by index.
  #[inline]
  fn find(&self, uid: i64) -> Option<&(i64, Box<(Vec<(String, String)>, R)>)> {
    self.records.iter().find(|(u, _)| *u == uid)
  }
}

#[cfg(test)]
mod test_dataset {
  use crate::data::*;
  use crate::Record;

  extern crate self as voidgui;
  #[derive(Record, Clone)]
  pub struct Pair {
    uid: i64,
    d_i: i64,
    d_s: String,
  }

  impl serde::Serialize for Pair {
    fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error>
    where
      S: serde::Serializer,
    {
      todo!()
    }
  }

  impl Header for Pair {
    fn header() -> Vec<&'static str> {
      todo!()
    }
  }

  fn setup() -> Dataset<Pair> {
    let mut d = Dataset::new();

    [
      Pair {
        uid: 0,
        d_i: 101,
        d_s: "Hello".to_string(),
      },
      Pair {
        uid: 1,
        d_i: 102,
        d_s: "Str".to_string(),
      },
      Pair {
        uid: 2,
        d_i: 103,
        d_s: "ahead".to_string(),
      },
    ]
    .into_iter()
    .for_each(|x| d.put(x));
    d
  }

  #[test]
  fn basics() {
    let mut d = setup();

    assert_eq!(d.get(0, 0), Some("101"));
    assert_eq!(d.get(0, 1), Some("Hello"));
    d.set(0, 1, "hi");
    assert_eq!(d.get(0, 1), Some("hi"));
  }

  #[test]
  fn search_framework() {
    let mut d = setup();

    // no search - no finds
    assert!(d.find_next().is_none());
    assert!(d.find_prev().is_none());

    d.new_search("St");

    // next works
    assert!(d.find_next().is_some());
    d.clear_search();

    // no search - no finds
    assert!(d.find_next().is_none());
    assert!(d.find_prev().is_none());
  }

  #[test]
  fn search_good() {
    let mut d = setup();
    d.new_search("He");

    // next works
    assert_eq!(d.find_next(), Some((0, 1)));
    assert_eq!(d.find_next(), Some((2, 1)));

    // prev works
    assert_eq!(d.find_prev(), Some((0, 1)));
  }

  #[test]
  fn search_prop_assoc() {
    let mut d = setup();

    d.new_search("He");
    d.find_next();
    d.find_prev();
    d.find_next();
    let s = d.search.clone();
    d.clear_search();

    d.new_search("He");
    d.find_next();
    d.find_next();
    d.find_prev();
    assert_eq!(s, d.search.clone(), "prev . next == next . prev");
    d.clear_search();
  }

  #[test]
  fn search_prev_wraps_properly() {
    let mut d = setup();

    d.new_search("He");
    d.find_prev();
    d.find_prev();
    d.find_prev();
    let s = d.search.clone();
    d.clear_search();

    d.new_search("He");
    d.find_prev();
    assert_eq!(s, d.search.clone(), "prev . prev . prev == prev");
    d.clear_search();
  }

  #[test]
  fn search_next_wraps_properly() {
    let mut d = setup();

    d.new_search("He");
    d.find_next();
    d.find_next();
    d.find_next();
    let s = d.search.clone();
    d.clear_search();

    d.new_search("He");
    d.find_next();
    assert_eq!(s, d.search.clone(), "next . next . next == next");
    d.clear_search();
  }

  #[test]
  fn search_bad() {
    let mut d = setup();

    d.new_search("miss");

    assert_eq!(d.find_prev(), None);
    assert_eq!(d.find_prev(), None);
  }
}
