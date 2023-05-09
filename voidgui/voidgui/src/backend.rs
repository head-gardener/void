use glfw::Context;

use crate::{colorscheme::BACKGROUND, render::painter::Painter};

pub struct Backend {
  pub painter: Painter,
  pub events: std::sync::mpsc::Receiver<(f64, glfw::WindowEvent)>,

  hw_window: glfw::Window,
  glfw: glfw::Glfw,
}

impl Backend {
  pub unsafe fn new(w: u16, h: u16) -> Self {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw
      .window_hint(glfw::WindowHint::ClientApi(glfw::ClientApiHint::OpenGlEs));
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 2));
    glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

    let (mut window, events) = glfw
      .create_window(w as u32, h as u32, "Void", glfw::WindowMode::Windowed)
      .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_key_polling(true);
    window.set_size_polling(true);
    window.set_char_polling(true);
    window.set_mouse_button_polling(true);
    window.set_cursor_pos_polling(true);
    let _gl = gl::load_with(|s| glfw.get_proc_address_raw(s));

    gl::Viewport(0, 0, w as i32, h as i32);
    let (r, g, b, a) = BACKGROUND;
    gl::ClearColor(r, g, b, a);
    gl::Enable(gl::BLEND);
    gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

    let painter = Painter::new(w, h);

    Self {
      painter,
      hw_window: window,
      events,
      glfw,
    }
  }

  pub fn should_close(&self) -> bool {
    self.hw_window.should_close()
  }

  pub fn swap_buffers(&mut self) {
    self.hw_window.swap_buffers()
  }

  pub fn poll_events(&mut self) {
    self.glfw.poll_events()
  }
}
