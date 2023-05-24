use std::{
  ffi::{c_char, CStr},
  io::Cursor,
};

use ciborium::de::from_reader;
use serde::{Deserialize, Serialize};
use voidgui::{
  backend::Backend,
  core::*,
  logic::{ring, File, Header},
  widgets::{self, toolbar::Toolbar, OrientedLayout, Spreadsheet, Status},
  Record,
};

struct Instance {
  c: Core,
  b: Backend,
}

enum Tags {
  Subscribers,
}

#[derive(Record, Serialize, Deserialize)]
struct Subscriber {
  d_name: String,
  d_phone: String,
  d_mou: i64,
  uid: i64,
}

// TODO: add this to proc macro
impl From<(i64, Vec<String>)> for Subscriber {
  fn from((uid, fs): (i64, Vec<String>)) -> Self {
    Self {
      d_name: fs[0].to_string(),
      d_phone: fs[1].to_string(),
      d_mou: fs[2].parse().unwrap(),
      uid,
    }
  }
}

impl Header for Subscriber {
  fn header() -> Vec<&'static str> {
    vec!["Name", "Phone", "MoU"]
  }
}

#[no_mangle]
extern "C" fn void_gui_init() -> Box<Instance> {
  let w = unsafe {
    let mut b = Backend::new(800, 600);
    let mut c = Core::new();
    populate(&mut c, &mut b);
    Instance { b, c }
  };
  Box::new(w)
}

unsafe fn populate(c: &mut Core, b: &mut Backend) {
  let desc = b.desc.read().unwrap();

  let f = ring::wrap(File::new::<Subscriber>());
  c.add_data(Tags::Subscribers as u32, f.clone());

  let win = widgets::Window::new(&desc);
  let ssheet = Spreadsheet::new::<Subscriber>(
    &desc,
    &mut b.drone,
    Tags::Subscribers as u32,
  )
  .unwrap();
  let toolbar = Toolbar::new(&desc, &mut b.drone).unwrap();
  let statusbox = OrientedLayout::new();

  let ssheet = {
    let mut ring = c.ring().write().unwrap();
    win.push_to_ring(&mut ring);
    toolbar.push_to_ring(&mut ring);

    let statusbox = ring::wrap(statusbox);
    ring.push_parent(statusbox.clone(), ring::Mark::StatusBox);
    ring.push_static(statusbox, ring::Mark::StatusBox, ring::Mark::Window, 3);

    ring.push_key_sink(f, ring::Mark::File(Tags::Subscribers as u32));

    ssheet.push_to_ring(&mut &mut ring)
  };

  c.with_data(Tags::Subscribers as u32, |d| {
    d.subscribe(ssheet, voidgui::logic::Mark::Spreadsheet)
  });
}

#[no_mangle]
unsafe extern "C" fn void_gui_exec(w: &mut Instance) -> u64 {
  w.c.on_exec(&mut w.b)
}

#[repr(C)]
struct CStringLen(u32, *mut u8);

#[no_mangle]
extern "C" fn void_gui_drain_damage(w: &mut Instance) -> Box<CStringLen> {
  let mut xs = w.c.pull_damage::<Subscriber>();
  let len = xs.len();
  let res = xs.as_mut_ptr();
  std::mem::forget(xs);
  w.c.wipe_damage_tracker();
  Box::new(CStringLen(len as u32, res))
}

#[no_mangle]
unsafe extern "C" fn void_gui_free_damage(_: Box<CStringLen>) -> i64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_status(msg: *const c_char, w: &mut Instance) {
  let msg = unsafe { CStr::from_ptr(msg) }.to_string_lossy();
  unsafe {
    Status::new(
      msg,
      &w.b.desc.read().unwrap(),
      w.c.ring().clone(),
      &w.b.drone,
    )
  }
  .map(|(s, uid)| s.push_to_ring(uid, &mut w.c.ring().write().unwrap()))
  .unwrap_or_default();
}

#[no_mangle]
extern "C" fn void_gui_finish(_: Box<Instance>) -> i64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_add(w: &mut Instance, len: i64, entry: *const u8) {
  let data = unsafe { std::slice::from_raw_parts(entry, len as usize) };
  let e: Subscriber = from_reader(Cursor::new(data)).unwrap();

  w.c.with_data(Tags::Subscribers as u32, |d| {
    d.put::<Subscriber>(&w.b.desc.read().unwrap(), &mut w.b.drone, e)
  });
}

#[no_mangle]
extern "C" fn void_gui_drop(w: &mut Instance) {
  w.c.with_data(Tags::Subscribers as u32, |d| {
    d.drop(&w.b.desc.read().unwrap(), &w.b.drone)
  });
  w.c.wipe_damage_tracker();
}
