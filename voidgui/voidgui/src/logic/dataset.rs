use std::cmp::max;

use serde::Serialize;

pub type Tag = u32;

type Search = Option<(String, usize)>;

/// An entry in a spreadsheet. Implement with `Entry` derive macro.
/// When doing so, prefix all data field with `d_` and give them `&'a str` type.
/// Those fields will be used when displaying data.
pub trait Record: Recordable {
  const N_FIELDS: usize;

  fn fields<'a>(&'a self) -> Vec<&'a dyn Data>;
  fn datatypes() -> Vec<Datatype>;
  fn uid(&self) -> &i64;
}

pub trait Recordable:
  Serialize + Header + Send + Sync + From<(i64, Vec<String>)>
{
}

pub trait Header {
  fn header() -> Vec<&'static str>;
}

pub enum Datatype {
  String,
  Integer,
}

static DEF_STR: String = String::new();
static DEF_INT: i64 = 0;

impl Datatype {
  pub fn default(&self) -> &'static dyn Data {
    match self {
      Datatype::String => &DEF_STR,
      Datatype::Integer => &DEF_INT,
    }
  }
}

pub trait Data: ToString {}
impl Data for String {}
impl Data for i64 {}

/// Searchable generic data storage.
pub struct Dataset {
  records: Vec<(i64, Box<Vec<(String, String)>>)>,
  search: Search,
  datatypes: Vec<Datatype>,
}

impl Dataset {
  pub fn new<R: Record>() -> Self {
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
  pub fn put<R: Record>(&mut self, x: R) {
    let fs = x
      .fields()
      .into_iter()
      .map(|d| d.to_string())
      .map(|s| {
        let lc = s.to_lowercase();
        (s, lc)
      })
      .collect();
    self.records.push((*x.uid(), Box::new(fs)));
  }

  /// Insert new default record.
  pub fn new_row(&mut self, uid: i64) {
    let fs: Vec<(String, String)> = self
      .datatypes
      .iter()
      .map(|f| f.default())
      .map(|s| (s.to_string(), s.to_string().to_lowercase()))
      .collect();
    self.records.push((uid, Box::new(fs)));
  }

  /// Set `field` of record with `uid` to `value`.
  pub fn set(&mut self, uid: i64, field: usize, value: &str) {
    self.find_mut(uid).iter_mut().for_each(|(_, xs)| {
      if xs.get(field).is_some() {
        xs[field] = (value.to_string(), value.to_lowercase());
      }
    });
  }

  /// Get `field` of record with `uid`.
  pub fn get(&self, uid: i64, field: usize) -> Option<&str> {
    self
      .find(uid)
      .and_then(|(_, xs)| xs.get(field).map(|p| p.0.as_str()))
  }

  /// Get row under `uid`.
  pub fn row(&self, uid: i64) -> Option<Vec<&str>> {
    self
      .find(uid)
      .map(|(_, xs)| xs.iter().map(|(s, _)| s.as_str()).collect())
  }

  /// Get record under `uid`.
  pub fn record<R: Record>(&self, uid: i64) -> Option<R> {
    self
      .row(uid)
      .map(|xs| R::from((uid, xs.iter().map(|s| s.to_string()).collect())))
  }

  /// Remove record under `uid`.
  pub fn rem(&mut self, uid: i64) {
    self.records.retain(|(u, _)| *u != uid);
  }

  /// Start new search.
  pub fn new_search(&mut self, value: &str) {
    self.search = Some((value.to_string().to_lowercase(), 0));
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
      .map(|(_, xs)| xs.iter().rev())
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
      .map(|(_, xs)| xs.iter())
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

  /// Returns a reference to the datatypes of this [`Dataset`].
  pub fn datatypes(&self) -> &[Datatype] {
    self.datatypes.as_ref()
  }

  /// Helper function for selecting row by index.
  #[inline]
  fn find(&self, uid: i64) -> Option<&(i64, Box<Vec<(String, String)>>)> {
    self.records.iter().find(|(u, _)| *u == uid)
  }

  /// Helper function for selecting row by index, mutably.
  #[inline]
  fn find_mut(
    &mut self,
    uid: i64,
  ) -> Option<&mut (i64, Box<Vec<(String, String)>>)> {
    self.records.iter_mut().find(|(u, _)| *u == uid)
  }
}

#[cfg(test)]
mod test_dataset {
  use voidmacro::Record;

  use super::*;

  extern crate self as voidgui;
  #[derive(Record)]
  pub struct Pair {
    uid: i64,
    d_i: i64,
    d_s: String,
  }

  impl Serialize for Pair {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
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

  impl From<(i64, Vec<String>)> for Pair {
    fn from(_: (i64, Vec<String>)) -> Self {
      todo!()
    }
  }

  fn setup() -> Dataset {
    let mut d = Dataset::new::<Pair>();

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
