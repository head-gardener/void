use glfw::{Action, Key, Modifiers, MouseButton, WindowEvent};

use crate::{
  backend::Backend,
  logic::Ring,
  render::{painter::Drone, Point},
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
      if b.drone.should_close() {
        return 1;
      }

      b.drone.poll_events();
      let events = glfw::flush_messages(&b.events);
      for (_, event) in events {
        let r = self.handle_event(&b.drone, event);
        if r != 0 {
          return r;
        }
      }
      gl::Clear(gl::COLOR_BUFFER_BIT);

      // self.ring.drain_damage_tracker(&b.painter);
      self.draw(b);
      b.drone.swap_buffers();
    }
  }

  pub unsafe fn handle_event(
    &mut self,
    drone: &Drone,
    event: WindowEvent,
  ) -> u64 {
    if self.ring.handle_transient_control_event(drone, &event) {
      return 0;
    }
    if self.ring.handle_key(drone, &event) {
      return 0;
    }

    match event {
      // Hotkeys
      WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
        drone.kill();
        return 1;
      }
      WindowEvent::Key(Key::P, _, Action::Press, m)
        if m == Modifiers::Control | Modifiers::Shift =>
      {
        return 2;
      }

      // Resize
      WindowEvent::Size(w, h) => {
        // p.resize(w as u16, h as u16);
        self.ring.into_iter().for_each(|w| {
          w.0.write().unwrap().request_plot();
        });
        gl::Viewport(0, 0, w, h);
      }

      // Mouse input
      WindowEvent::CursorPos(x, y) => {
        self.cursor = Point::new(x as u16, y as u16);
      }
      WindowEvent::MouseButton(MouseButton::Button1, Action::Press, _mods) => {
        self.ring.catch_click(drone, self.cursor);
      }

      // Text input
      WindowEvent::Char(c) => {
        self.ring.catch_input_event(drone, InputEvent::Char(c));
      }
      WindowEvent::Key(Key::Left, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(drone, InputEvent::Left);
      }
      WindowEvent::Key(Key::Right, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(drone, InputEvent::Right);
      }
      WindowEvent::Key(
        Key::Backspace,
        _,
        Action::Press | Action::Repeat,
        _,
      ) => {
        self.ring.catch_input_event(drone, InputEvent::Backspace);
      }
      WindowEvent::Key(Key::Delete, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(drone, InputEvent::Delete);
      }
      WindowEvent::Key(Key::Home, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(drone, InputEvent::Home);
      }
      WindowEvent::Key(Key::End, _, Action::Press | Action::Repeat, _) => {
        self.ring.catch_input_event(drone, InputEvent::End);
      }
      WindowEvent::Key(
        Key::Enter,
        _,
        Action::Press | Action::Repeat,
        Modifiers::Control,
      ) => {
        self.ring.catch_input_event(drone, InputEvent::Newline);
      }

      _ => {
        println!("{:?}", event);
      }
    };

    0
  }

  pub fn with_ssheet_mut<F, R>(&mut self, f: F) -> R
  where
    F: FnOnce(&mut Spreadsheet) -> R,
  {
    f(self
      .ring
      .pull(&crate::logic::ring::Mark::Spreadsheet)
      .expect("Spreadsheet should always be on the ring")
      .write()
      .unwrap()
      .downcast_mut()
      .expect("Only spreadsheet should be marked as spreadsheet in the ring"))
  }

  pub fn draw(&mut self, b: &mut Backend) {
    self
      .ring
      .draw(b.painter.clone(), &mut b.drone)
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
