use glfw::{Action, Key, WindowEvent};

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
        _ => {}
      }
    }

    unsafe {
      gl::Clear(gl::COLOR_BUFFER_BIT);
    }

    w.draw();
    crate::render::painter::pop_gl_error();
    w.swap_buffers();
  }
}

#[no_mangle]
pub extern "C" fn void_gui_finish(_: Box<VoidWindow>) {}

#[no_mangle]
pub extern "C" fn void_gui_add() {}
