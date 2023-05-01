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
  let p = w.painter() as *const crate::render::painter::Painter;
  let na = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let ph = unsafe { CStr::from_ptr(phone) }.to_string_lossy();

  w.with_ssheet_mut(|ssheet| {
    ssheet
      .transaction(|s, push| {
        push(s, unsafe { p.as_ref().unwrap() }, &na, &ph)?;
        Ok(())
      })
      .unwrap()
  });

  1
}

#[no_mangle]
pub extern "C" fn void_gui_drop(w: &mut VoidWindow) -> u64 {
  w.with_ssheet_mut(|s| s.drop());

  1
}
