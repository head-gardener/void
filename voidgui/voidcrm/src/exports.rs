use std::ffi::{c_char, CStr};

use voidgui::{
  backend::Backend,
  core::*,
  widgets::{self, toolbar::Toolbar, Spreadsheet, traits::Error},
};

struct Window {
  c: Core,
  b: Backend,
}

#[no_mangle]
extern "C" fn void_gui_init() -> Box<Window> {
  let w = unsafe {
    let b = Backend::new(800, 600);
    let mut c = Core::new();
    populate(&mut c, &b).unwrap();
    Window { b, c }
  };
  Box::new(w)
}

unsafe fn populate(c: &mut Core, b: &Backend) -> Result<(), Error> {
  let win = widgets::Window::new(&b.painter);
  let ssheet = Spreadsheet::new(&b.painter)?;
  let toolbar = Toolbar::new(&b.painter)?;

  win.push_to_ring(c.ring_mut());
  ssheet.push_to_ring(c.ring_mut());
  toolbar.push_to_ring(c.ring_mut());

  Ok(())
}

#[no_mangle]
unsafe extern "C" fn void_gui_exec(w: &mut Window) -> u64 {
  w.c.on_exec(&mut w.b)
}

#[no_mangle]
extern "C" fn void_gui_finish(_: Box<Window>) {}

#[no_mangle]
extern "C" fn void_gui_add(
  name: *const c_char,
  phone: *const c_char,
  w: &mut Window,
) -> u64 {
  let n = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let p = unsafe { CStr::from_ptr(phone) }.to_string_lossy();
  let mut res = 1;

  w.c.with_ssheet_mut(&w.b, |ssheet, painter| {
    if let Err(e) = ssheet.push(painter, &n, &p) {
      println!("Push failed: {}", e);
      res = 2;
      ssheet.drop();
    }
  });

  res
}

#[no_mangle]
extern "C" fn void_gui_drop(w: &mut Window) -> u64 {
  w.c.with_ssheet_mut(&w.b, |s, _| s.drop());

  1
}
