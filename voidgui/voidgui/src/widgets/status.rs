use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
  sync::RwLockReadGuard,
};

use voidmacro::{DrawableMenu, Menu};

use crate::{
  logic::ring::{self, Mark, Ring, Wrap},
  render::{
    painter::{Description, Drone, DroneFeed},
    text_table::CellStyle,
    Origin, Size, TextTable,
  },
};

use super::traits::{Drawable, Transient, Widget};
use crate::widgets;

#[derive(Menu, DrawableMenu)]
pub struct Status {
  table: TextTable,
}

impl Status {
  pub unsafe fn new<R>(
    message: R,
    desc: &Description,
    ring: Wrap<Ring>,
    drone: &Drone,
  ) -> Result<(Self, u64), widgets::Error>
  where
    R: AsRef<str>,
  {
    let mut s = DefaultHasher::new();
    message.as_ref().to_string().hash(&mut s);
    std::time::Instant::now().hash(&mut s);
    let uid = s.finish();

    {
      let uid = uid.clone();
      let feed = drone.feed().clone();
      std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_secs(3));
        ring
          .write()
          .unwrap()
          .kill_transient(&feed, Mark::Status(uid));
      });
    }

    let table =
      TextTable::from_text(desc, drone, 1, 1, CellStyle::Lighter, &[message])?;

    Ok((Self { table }, uid))
  }

  pub fn push_to_ring(self, uid: u64, ring: &mut crate::logic::Ring) {
    let rc = ring::wrap(self);
    ring.push_transient(rc.clone(), Mark::Status(uid), false);
    ring.push_dynamic(rc, Mark::Status(uid), Mark::StatusBox);
  }
}

impl Transient for Status {}
