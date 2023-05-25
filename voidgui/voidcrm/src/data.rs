use serde::{Deserialize, Serialize};
use voidgui::{
  data::{Data, FKey, Header},
  Record,
};

#[derive(Record, Clone, Serialize, Deserialize, Debug)]
// #[fkey(1, 0)]
pub struct Subscriber {
  d_name: String,
  d_phone: String,
  d_mou: i64,
  d_plan: FKey,
  uid: i64,
}

impl Header for Subscriber {
  fn header() -> Vec<&'static str> {
    vec!["Name", "Phone", "MoU", "Plan"]
  }
}

#[derive(Record, Clone, Serialize, Deserialize, Debug)]
pub struct Plan {
  d_name: String,
  d_rate: i64,
  d_minutes: i64,
  uid: i64,
}

impl Header for Plan {
  fn header() -> Vec<&'static str> {
    vec!["Name", "Rate", "Minutes"]
  }
}
