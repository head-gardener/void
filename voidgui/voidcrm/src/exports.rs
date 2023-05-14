use std::ffi::{c_char, CStr};

use voidgui::{
  backend::Backend,
  core::*,
  widgets::{self, toolbar::Toolbar, Spreadsheet},
  Entry,
};

struct Instance {
  c: Core,
  b: Backend,
}

#[derive(Entry)]
struct Entry<'a> {
  d_name: &'a str,
  d_phone: &'a str,
  uuid: u64,
}

impl<'a> Entry<'a> {
  fn new(d_name: &'a str, d_phone: &'a str, uid: u64) -> Self {
    Self {
      d_name,
      d_phone,
      uuid: uid,
    }
  }
}

impl Default for Entry<'_> {
  fn default() -> Self {
    Self {
      d_name: "Name",
      d_phone: "Phone",
      uuid: 0,
    }
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
  let painter = b.desc.read().unwrap();

  let win = widgets::Window::new(&painter);
  let ssheet = Spreadsheet::new::<Entry>(&painter, &mut b.drone).unwrap();
  let toolbar = Toolbar::new(&painter, &mut b.drone).unwrap();

  win.push_to_ring(c.ring_mut());
  ssheet.push_to_ring(c.ring_mut());
  toolbar.push_to_ring(c.ring_mut());
}

#[no_mangle]
unsafe extern "C" fn void_gui_exec(w: &mut Instance) -> u64 {
  w.c.on_exec(&mut w.b)
}

#[repr(C)]
struct CStringLen(u32, *mut u8);

#[no_mangle]
extern "C" fn void_gui_pull_damage(w: &mut Instance) -> Box<CStringLen> {
  let mut xs = w.c.pull_damage();
  let len = xs.len();
  let res = xs.as_mut_ptr();
  std::mem::forget(xs);
  Box::new(CStringLen(len as u32, res))
}

#[no_mangle]
unsafe extern "C" fn void_gui_free_damage(_: Box<CStringLen>) -> u64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_finish(_: Box<Instance>) -> u64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_add(
  uuid: u64,
  name: *const c_char,
  phone: *const c_char,
  w: &mut Instance,
) -> u64 {
  let n = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let p = unsafe { CStr::from_ptr(phone) }.to_string_lossy();
  let e = Entry::new(&n, &p, uuid);

  w.c.with_ssheet_mut(|ssheet| {
    if let Err(e) =
      ssheet.push::<Entry>(&w.b.desc.read().unwrap(), &mut w.b.drone, e)
    {
      println!("Push failed: {}", e);
      ssheet.drop();
      2
    } else {
      0
    }
  })
}

#[no_mangle]
extern "C" fn void_gui_drop(w: &mut Instance) -> u64 {
  w.c.with_ssheet_mut(|s| {
    s.drop();
    0
  })
}
