use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
  sync::RwLockReadGuard,
  time::Duration,
};

use voidmacro::{DrawableMenu, Menu};

use crate::{
  logic::ring::{Mark, Ring, RingElement, Wrap},
  render::{
    painter::{Drone, DroneFeed},
    text_table::CellStyle,
    Origin, Size, TextTable,
  },
  ring, CallbackResult, Description,
};

use super::traits::{Drawable, Transient, Widget};
use crate::widgets;

#[derive(Menu, DrawableMenu)]
pub struct Status {
  table: TextTable,
  uid: u64,
}

impl Status {
  pub unsafe fn new<R>(
    message: R,
    desc: &Description,
    drone: &Drone,
  ) -> Result<Self, widgets::Error>
  where
    R: AsRef<str>,
  {
    let mut s = DefaultHasher::new();
    message.as_ref().to_string().hash(&mut s);
    std::time::Instant::now().hash(&mut s);
    let uid = s.finish();

    let table =
      TextTable::from_text(desc, drone, 1, 1, CellStyle::Lighter, &[message])?;

    Ok(Self { table, uid })
  }

  pub unsafe fn create<R>(
    message: R,
    desc: &Description,
    drone: &Drone,
  ) -> CallbackResult
  where
    R: AsRef<str>,
  {
    Self::new(message, desc, drone)
      .map(|f| Box::new(ring::wrap(f)))
      .map_or_else(|e| CallbackResult::Error(e), |f| CallbackResult::Push(f))
  }

  pub fn timeout(&self, dur: Duration, ring: Wrap<Ring>, drone: &Drone) {
    let uid = self.uid.clone();
    let feed = drone.feed().clone();
    std::thread::spawn(move || {
      std::thread::sleep(dur);
      ring
        .write()
        .unwrap()
        .kill_transient(&feed, Mark::Status(uid));
    });
  }

  pub fn uid(&self) -> u64 {
    self.uid
  }
}

impl RingElement for Wrap<Status> {
  fn push_to_ring(&self, mut ring: std::sync::RwLockWriteGuard<Ring>) {
    let uid = self.read().unwrap().uid().clone();
    ring.push_transient(self.clone(), Mark::Status(uid), false);
    ring.push_dynamic(self.clone(), Mark::Status(uid), Mark::StatusBox);
  }
}

impl Transient for Status {}
