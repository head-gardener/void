use glfw::Context;

use crate::{logic::Ring, render::painter::SPainter, widgets::Spreadsheet};

pub struct VoidWindow {
  painter: SPainter,
  ring: Ring,
  events: std::sync::mpsc::Receiver<(f64, glfw::WindowEvent)>,
  hw_window: glfw::Window,
  glfw: glfw::Glfw,
}

impl VoidWindow {
  pub unsafe fn new(w: u16, h: u16) -> Self {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    glfw.window_hint(glfw::WindowHint::ClientApi(glfw::ClientApiHint::OpenGlEs));
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 2));
    glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

    let (mut window, events) = glfw
      .create_window(
        w as u32,
        h as u32,
        "Void",
        glfw::WindowMode::Windowed,
      )
      .expect("Failed to create GLFW window.");

    window.make_current();
    window.set_key_polling(true);
    let _gl = gl::load_with(|s| glfw.get_proc_address_raw(s));

    gl::Viewport(0, 0, w as i32, h as i32);
    gl::ClearColor(0.9, 0.9, 0.9, 1.0);
    gl::Enable(gl::BLEND);
    gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

    let painter = SPainter::new(w, h);
    let ssheet = Box::new(Spreadsheet::new(&painter).unwrap());
    let mut ring = Ring::new();
    ring.push(ssheet, crate::logic::ring::Mark::Spreadsheet);

    Self {
      hw_window: window,
      painter,
      ring,
      events,
      glfw,
    }
  }

  pub fn draw(&mut self) {
    self.ring.draw(&(self.painter));
  }

  pub fn painter(&self) -> &SPainter {
    &self.painter
  }

  pub fn should_close(&self) -> bool {
    self.hw_window.should_close()
  }

  pub fn swap_buffers(&mut self) {
    self.hw_window.swap_buffers();
  }

  pub fn poll_events(&mut self) {
    self.glfw.poll_events()
  }

  pub fn events(&self) -> &std::sync::mpsc::Receiver<(f64, glfw::WindowEvent)> {
    &self.events
  }
}
