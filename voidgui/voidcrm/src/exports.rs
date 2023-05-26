use std::{
  ffi::{c_char, CStr},
  io::Cursor,
};

use crate::data::{Plan, Subscriber};
use ciborium::de::from_reader;
use voidgui::{
  backend::Backend,
  core::*,
  data::{File, GenericFile, Record},
  ring,
  widgets::{self, OrientedLayout, Spreadsheet, Status, Toolbar},
  RingElement, Wrap,
};

struct Instance {
  c: Core,
  b: Backend,
  subs: Wrap<File<Subscriber>>,
  plans: Wrap<File<Plan>>,
}

enum Tags {
  Subscribers,
  Plans,
}

#[no_mangle]
extern "C" fn void_gui_init() -> Box<Instance> {
  let w = unsafe {
    let mut b = Backend::new(800, 600);
    let mut c = Core::new();

    let subs = ring::wrap(File::<Subscriber>::new());
    let plans = ring::wrap(File::<Plan>::new());

    populate(&mut c, &mut b, subs.clone(), plans.clone());
    Instance { b, c, subs, plans }
  };
  Box::new(w)
}

unsafe fn populate(
  c: &mut Core,
  b: &mut Backend,
  subs: Wrap<File<Subscriber>>,
  plans: Wrap<File<Plan>>,
) {
  let desc = b.desc.read().unwrap();

  c.add_data(Tags::Subscribers as u32, subs.clone());
  c.add_data(Tags::Plans as u32, plans.clone());

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

    ring
      .push_key_sink(subs.clone(), ring::Mark::File(Tags::Subscribers as u32));
    ssheet.push_to_ring(&mut &mut ring)
  };

  subs
    .write()
    .unwrap()
    .subscriber(ssheet, voidgui::logic::Mark::Spreadsheet)
}

#[no_mangle]
unsafe extern "C" fn void_gui_exec(w: &mut Instance) -> u64 {
  w.c.on_exec(&mut w.b)
}

#[repr(C)]
struct CStringLen(u32, *mut u8);

#[no_mangle]
extern "C" fn void_gui_drain_damage(w: &mut Instance) -> Box<CStringLen> {
  let mut xs = w.c.pull_damage::<Subscriber>(&w.subs);
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
  unsafe { Status::new(msg, &w.b.desc.read().unwrap(), &w.b.drone) }
    .map(|s| {
      s.timeout(
        std::time::Duration::from_secs(3),
        w.c.ring().clone(),
        &w.b.drone,
      );
      ring::wrap(s).push_to_ring(w.c.ring().write().unwrap());
    })
    .unwrap_or_default();
}

#[no_mangle]
extern "C" fn void_gui_finish(_: Box<Instance>) -> i64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_parse(len: i64, entry: *const u8) -> u64 {
  let data = unsafe { std::slice::from_raw_parts(entry, len as usize) };
  let e: Result<Subscriber, _> = from_reader(Cursor::new(data));
  if e.is_ok() {
    0
  } else {
    1
  }
}

#[no_mangle]
extern "C" fn void_gui_add(
  w: &mut Instance,
  tag: i64,
  len: i64,
  entry: *const u8,
) {
  let data = unsafe { std::slice::from_raw_parts(entry, len as usize) };

  match tag {
    0 => {
      let s: Subscriber = from_reader(Cursor::new(data)).unwrap();
      let uid = *s.uid();
      w.subs
        .write()
        .unwrap()
        .put(&w.b.desc.read().unwrap(), &mut w.b.drone, s);

      w.c.job(
        "join",
        w.subs.write().unwrap().join(Tags::Subscribers as u32, uid),
      );
    }
    1 => {
      let p: Plan = from_reader(Cursor::new(data)).unwrap();
      w.plans.write().unwrap().put(
        &w.b.desc.read().unwrap(),
        &mut w.b.drone,
        p,
      );
    }
    n => {
      panic!("{n}")
    }
  };
}

#[no_mangle]
extern "C" fn void_gui_drop(w: &mut Instance) {
  File::drop(
    &mut w.subs.write().unwrap(),
    &w.b.desc.read().unwrap(),
    &w.b.drone,
  );
  File::drop(
    &mut w.plans.write().unwrap(),
    &w.b.desc.read().unwrap(),
    &w.b.drone,
  );
  w.c.wipe_damage_tracker();
}
