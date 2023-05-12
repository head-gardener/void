use std::ffi::{c_char, CStr};

use voidgui::{
  backend::Backend,
  core::*,
  widgets::{self, toolbar::Toolbar, traits::Error, Spreadsheet},
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
    populate(&mut c, &mut b).unwrap();
    Window { b, c }
  };
  Box::new(w)
}

unsafe fn populate(c: &mut Core, b: &mut Backend) -> Result<(), Error> {
  let painter = b.painter.read().unwrap();
  let win = widgets::Window::new(&painter);
  let ssheet = Spreadsheet::new(&painter, &mut b.drone)?;
  let toolbar = Toolbar::new(&painter, &mut b.drone)?;

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

  w.c.with_ssheet_mut(|ssheet| {
    if let Err(e) =
      ssheet.push(&w.b.painter.read().unwrap(), &mut w.b.drone, &n, &p)
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
