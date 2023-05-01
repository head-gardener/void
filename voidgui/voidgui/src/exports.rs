use std::ffi::{c_char, CStr};

use glfw::{Action, Key, Modifiers, WindowEvent};

use crate::window::*;

#[no_mangle]
pub extern "C" fn void_gui_init() -> Box<VoidWindow> {
  unsafe { Box::new(VoidWindow::new(800, 600)) }
}

#[no_mangle]
pub extern "C" fn void_gui_exec(win: &mut VoidWindow) -> u64 {
  let win_cpy = win as *mut VoidWindow;

  loop {
    if win.should_close() {
      return 1;
    }

    win.poll_events();
    for (_, event) in glfw::flush_messages(&win.events()) {
      println!("{:?}", event);
      match event {
        WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
          return 1;
        }
        WindowEvent::Key(Key::P, _, Action::Press, m)
          if m == Modifiers::Control | Modifiers::Shift =>
        {
          return 2;
        }

        WindowEvent::Size(w, h) => {
          unsafe { win_cpy.as_mut().unwrap().on_resize(w, h) };
        }
        _ => {}
      }
    }

    unsafe {
      gl::Clear(gl::COLOR_BUFFER_BIT);
    }

    crate::render::painter::pop_gl_error();
    win.draw();
    crate::render::painter::pop_gl_error();
    win.swap_buffers();
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
