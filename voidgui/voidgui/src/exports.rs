use std::ffi::{c_char, CStr};

use crate::window::*;

#[no_mangle]
pub extern "C" fn void_gui_init() -> Box<VoidWindow> {
  unsafe { Box::new(VoidWindow::new(800, 600)) }
}

#[no_mangle]
pub unsafe extern "C" fn void_gui_exec(win: &mut VoidWindow) -> u64 {
  win.on_exec()
}

#[no_mangle]
pub extern "C" fn void_gui_finish(_: Box<VoidWindow>) {}

#[no_mangle]
pub extern "C" fn void_gui_add(
  name: *const c_char,
  phone: *const c_char,
  w: &mut VoidWindow,
) -> u64 {
  let n = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let p = unsafe { CStr::from_ptr(phone) }.to_string_lossy();
  let mut res = 1;

  w.with_ssheet_mut(|ssheet, painter| {
    if let Err(e) = ssheet.push(painter, &n, &p) {
      println!("Push failed: {}", e);
      res = 2;
      ssheet.drop();
    }
  });

  res
}

#[no_mangle]
pub extern "C" fn void_gui_drop(w: &mut VoidWindow) -> u64 {
  w.with_ssheet_mut(|s, _| s.drop());

  1
}
