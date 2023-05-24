use serde::{Deserialize, Serialize};
use voidgui::{logic::Header, Record};

#[derive(Record, Serialize, Deserialize, Debug)]
pub struct Subscriber {
  d_name: String,
  d_phone: String,
  d_mou: i64,
  d_plan: i64,
  uid: i64,
}

impl Default for Subscriber {
  fn default() -> Self {
    Self {
      d_name: Default::default(),
      d_phone: Default::default(),
      d_mou: Default::default(),
      d_plan: Default::default(),
      uid: Default::default(),
    }
  }
}

// TODO: add this to proc macro
impl From<(i64, Vec<String>)> for Subscriber {
  fn from((uid, fs): (i64, Vec<String>)) -> Self {
    Self {
      d_name: fs[0].to_string(),
      d_phone: fs[1].to_string(),
      d_mou: fs[2].parse().unwrap(),
      d_plan: fs[3].parse().unwrap(),
      uid,
    }
  }
}

impl Header for Subscriber {
  fn header() -> Vec<&'static str> {
    vec!["Name", "Phone", "MoU", "Plan"]
  }
}

#[derive(Record, Serialize, Deserialize, Debug)]
pub struct Plan {
  d_name: String,
  d_rate: i64,
  d_minutes: i64,
  uid: i64,
}

impl Default for Plan {
  fn default() -> Self {
    Self {
      d_name: Default::default(),
      d_rate: Default::default(),
      d_minutes: Default::default(),
      uid: Default::default(),
    }
  }
}

// TODO: add this to proc macro
impl From<(i64, Vec<String>)> for Plan {
  fn from((uid, fs): (i64, Vec<String>)) -> Self {
    panic!()
  }
}

impl Header for Plan {
  fn header() -> Vec<&'static str> {
    vec!["Name", "Rate", "Minutes"]
  }
}
