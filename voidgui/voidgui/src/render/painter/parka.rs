use std::sync::{mpsc::Receiver, Arc, RwLock};

use glfw::{Context, Window};

use crate::colorscheme::BACKGROUND;

use super::Painter;

pub struct Parka(u32, u32);

impl Parka {
  pub fn new(w: u32, h: u32) -> Self {
    Self(w, h)
  }

  pub unsafe fn tear(
    self,
  ) -> (
    Window,
    Receiver<(f64, glfw::WindowEvent)>,
    Arc<RwLock<Painter>>,
  ) {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw
      .window_hint(glfw::WindowHint::ClientApi(glfw::ClientApiHint::OpenGlEs));
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 2));
    glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

    let (mut window, events) = glfw
      .create_window(self.0, self.1, "Void", glfw::WindowMode::Windowed)
      .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_key_polling(true);
    window.set_size_polling(true);
    window.set_char_polling(true);
    window.set_mouse_button_polling(true);
    window.set_cursor_pos_polling(true);

    let _gl = gl::load_with(|s| glfw.get_proc_address_raw(s));

    gl::Viewport(0, 0, self.0 as i32, self.1 as i32);
    let (r, g, b, a) = BACKGROUND;
    gl::ClearColor(r, g, b, a);
    gl::Enable(gl::BLEND);
    gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

    let painter =
      Arc::new(RwLock::new(Painter::new(self.0 as u16, self.1 as u16)));

    (window, events, painter)
  }
}
