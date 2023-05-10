use glfw::{Action, Key, Modifiers, MouseButton, WindowEvent};

use crate::{
  backend::Backend,
  logic::Ring,
  render::{painter::Painter, Point},
  widgets::{traits::InputEvent, Spreadsheet},
};

pub struct Core {
  ring: Ring,
  cursor: Point,
}

impl Core {
  pub fn new() -> Self {
    let ring = Ring::new();

    Self {
      ring,
      cursor: Point::new(0, 0),
    }
  }

  pub unsafe fn on_exec(&mut self, b: &mut Backend) -> u64 {
    loop {
      if b.should_close() {
        return 1;
      }

      b.poll_events();
      let events = glfw::flush_messages(&b.events);
      for (_, event) in events {
        let r = self.handle_event(&mut b.painter, event);
        if r != 0 {
          return r;
        }
      }
      gl::Clear(gl::COLOR_BUFFER_BIT);

      self.ring.drain_damage_tracker(&b.painter);
      self.draw(b);
      crate::render::painter::pop_gl_error();
      b.swap_buffers();
    }
  }

  pub unsafe fn handle_event(
    &mut self,
    p: &mut Painter,
    event: WindowEvent,
  ) -> u64 {
    if self.ring.handle_transient_control_event(p, &event) {
      return 0;
    }
    if self.ring.handle_key(p, &event) {
      return 0;
    }

    match event {
      // Hotkeys
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        return 1;
      }
      WindowEvent::Key(Key::P, _, Action::Press, m)
        if m == Modifiers::Control | Modifiers::Shift =>
      {
        return 2;
      }

      // Resize
      WindowEvent::Size(w, h) => {
        p.resize(w as u16, h as u16);
        self.ring.for_each(|mut w| w.request_plot());
        gl::Viewport(0, 0, w, h);
      }

      // Mouse input
      WindowEvent::CursorPos(x, y) => {
        self.cursor = Point::new(x as u16, y as u16);
      }
      WindowEvent::MouseButton(MouseButton::Button1, Action::Press, _mods) => {
        self.ring.catch_click(p, self.cursor);
      }

      // Text input
      WindowEvent::Char(c) => {
        self.ring.catch_input_event(p, InputEvent::Char(c));
      }
      WindowEvent::Key(Key::Left, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(p, InputEvent::Left);
      }
      WindowEvent::Key(Key::Right, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(p, InputEvent::Right);
      }
      WindowEvent::Key(
        Key::Backspace,
        _,
        Action::Press | Action::Repeat,
        _,
      ) => {
        self.ring.catch_input_event(p, InputEvent::Backspace);
      }
      WindowEvent::Key(Key::Delete, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(p, InputEvent::Delete);
      }
      WindowEvent::Key(Key::Home, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(p, InputEvent::Home);
      }
      WindowEvent::Key(Key::End, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(p, InputEvent::End);
      }
      WindowEvent::Key(
        Key::Enter,
        _,
        Action::Press | Action::Repeat,
        Modifiers::Control,
      ) => {
        self.ring.catch_input_event(p, InputEvent::Newline);
      }

      _ => {
        println!("{:?}", event);
      }
    };

    0
  }

  pub fn with_ssheet_mut<F>(&mut self, b: &Backend, f: F)
  where
    F: FnOnce(&mut Spreadsheet, &Painter),
  {
    f(
      self
        .ring
        .pull(&crate::logic::ring::Mark::Spreadsheet)
        .expect("Spreadsheet should always be on the ring")
        .write()
        .unwrap()
        .downcast_mut()
        .expect("Only spreadsheet should be marked as spreadsheet in the ring"),
      &b.painter,
    )
  }

  pub fn draw(&mut self, b: &Backend) {
    self
      .ring
      .draw(&b.painter)
      .iter()
      .for_each(|e| println!("{}", e));
  }

  pub fn ring(&self) -> &Ring {
    &self.ring
  }

  pub fn ring_mut(&mut self) -> &mut Ring {
    &mut self.ring
  }
}
