use std::ffi::{c_char, CStr};

use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::window::*;

#[no_mangle]
pub extern "C" fn void_gui_init() -> Box<VoidWindow> {
  unsafe { Box::new(VoidWindow::new(800, 600)) }
}

#[no_mangle]
pub extern "C" fn void_gui_exec(w: &mut VoidWindow) -> u64 {
  loop {
    if w.should_close() {
      return 1;
    }

    w.poll_events();
    for (_, event) in glfw::flush_messages(&w.events()) {
      // println!("{:?}", event);
      match event {
        WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
          return 1;
        }
        WindowEvent::Key(Key::P, _, Action::Press, m)
          if m == Modifiers::Control | Modifiers::Shift =>
        {
          return 2;
        }
        _ => {}
      }
    }

    unsafe {
      gl::Clear(gl::COLOR_BUFFER_BIT);
    }

    crate::render::painter::pop_gl_error();
    w.draw();
    crate::render::painter::pop_gl_error();
    w.swap_buffers();
  }
}

#[no_mangle]
pub extern "C" fn void_gui_finish(_: Box<VoidWindow>) {}

#[no_mangle]
pub extern "C" fn void_gui_add(
  name: *const c_char,
  phone: *const c_char,
  w: &mut VoidWindow,
) -> u64 {
  // pointer magic to escape borrow checker
  let p = w.painter() as *const crate::render::painter::SPainter;
  let ssheet = w.ssheet_mut();

  let na = unsafe { CStr::from_ptr(name) }.to_string_lossy();
  let ph = unsafe { CStr::from_ptr(phone) }.to_string_lossy();

  ssheet
    .transaction(|s, push| {
      push(s, unsafe { p.as_ref().unwrap() }, &na, &ph)?;
      Ok(())
    })
    .unwrap();

  1
}

#[no_mangle]
pub extern "C" fn void_gui_drop(
  w: &mut VoidWindow,
) -> u64 {
  // pointer magic to escape borrow checker
  let p = w.painter() as *const crate::render::painter::SPainter;
  let ssheet = w.ssheet_mut();

  ssheet.drop();

  1
}
