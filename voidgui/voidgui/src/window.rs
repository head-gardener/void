use glfw::{Action, Context, Key, Modifiers, MouseButton, WindowEvent};

use crate::{
  logic::Ring,
  render::{painter::Painter, Point},
  widgets::{toolbar::Toolbar, Spreadsheet},
};

pub struct VoidWindow {
  painter: Painter,
  ring: Ring,
  events: std::sync::mpsc::Receiver<(f64, glfw::WindowEvent)>,
  hw_window: glfw::Window,
  glfw: glfw::Glfw,
  cursor: Point,
}

impl VoidWindow {
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
    gl::ClearColor(0.9, 0.9, 0.9, 1.0);
    gl::Enable(gl::BLEND);
    gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

    let painter = Painter::new(w, h);

    let ssheet = Spreadsheet::new(&painter).unwrap();
    let toolbar = Toolbar::new(&painter).unwrap();

    let mut ring = Ring::new();
    ssheet.push_to_ring(&mut ring);
    toolbar.push_to_ring(&mut ring);

    Self {
      hw_window: window,
      painter,
      ring,
      events,
      glfw,
      cursor: Point::new(0, 0),
    }
  }

  pub unsafe fn on_exec(&mut self) -> u64 {
    loop {
      if self.should_close() {
        return 1;
      }

      self.poll_events();
      let events = glfw::flush_messages(&self.events);
      for (_, event) in events {
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
            self.painter.resize(w as u16, h as u16);
            self.ring.for_each(|mut w| w.request_plot());
            gl::Viewport(0, 0, w, h);
          }

          WindowEvent::CursorPos(x, y) => {
            self.cursor = Point::new(x as u16, y as u16);
          }

          WindowEvent::MouseButton(
            MouseButton::Button1,
            Action::Press,
            _mods,
          ) => {
            self.ring.catch_click(self.cursor);
          }
          _ => {
            println!("{:?}", event);
          }
        }
      }

      gl::Clear(gl::COLOR_BUFFER_BIT);

      self.draw();
      crate::render::painter::pop_gl_error();
      self.swap_buffers();
    }
  }

  pub fn with_ssheet_mut<F>(&mut self, f: F)
  where
    F: Fn(&mut Spreadsheet),
  {
    f(self
      .ring
      .pull(crate::logic::ring::Mark::Spreadsheet)
      .expect("Spreadsheet should always be on the ring")
      .borrow_mut()
      .downcast_mut()
      .expect("Only spreadsheet should be marked as spreadsheet in the ring"))
  }

  pub fn draw(&mut self) {
    self
      .ring
      .draw(&(self.painter))
      .iter()
      .for_each(|e| println!("{}", e));
  }

  pub fn painter(&self) -> &Painter {
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
