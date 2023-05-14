use std::ffi::{c_char, CStr};

use voidgui::{
  backend::Backend,
  core::*,
  widgets::{self, toolbar::Toolbar, Spreadsheet},
};

struct Window {
  c: Core,
  b: Backend,
}

#[no_mangle]
extern "C" fn void_gui_init() -> Box<Window> {
  let w = unsafe {
    let mut b = Backend::new(800, 600);
    let mut c = Core::new();
    populate(&mut c, &mut b);
    Window { b, c }
  };
  Box::new(w)
}

unsafe fn populate(c: &mut Core, b: &mut Backend) {
  let painter = b.desc.read().unwrap();

  let win = widgets::Window::new(&painter);
  let ssheet = Spreadsheet::new(&painter, &mut b.drone).unwrap();
  let toolbar = Toolbar::new(&painter, &mut b.drone).unwrap();

  win.push_to_ring(c.ring_mut());
  ssheet.push_to_ring(c.ring_mut());
  toolbar.push_to_ring(c.ring_mut());
}

#[no_mangle]
unsafe extern "C" fn void_gui_exec(w: &mut Window) -> u64 {
  w.c.on_exec(&mut w.b)
}

#[repr(C)]
struct CStringLen(u32, *mut u8);

#[no_mangle]
extern "C" fn void_gui_pull_damage(w: &mut Window) -> Box<CStringLen> {
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
extern "C" fn void_gui_finish(_: Box<Window>) -> u64 {
  0
}

#[no_mangle]
extern "C" fn void_gui_add(
  name: *const c_char,
  phone: *const c_char,
  w: &mut Window,
) -> u64 {
  let n = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let p = unsafe { CStr::from_ptr(phone) }.to_string_lossy();

  w.c.with_ssheet_mut(|ssheet| {
    if let Err(e) =
      ssheet.push(&w.b.desc.read().unwrap(), &mut w.b.drone, &n, &p)
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
extern "C" fn void_gui_drop(w: &mut Window) -> u64 {
  w.c.with_ssheet_mut(|s| {
    s.drop();
    0
  })
}
