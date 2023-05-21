use std::{
  io::Cursor,
  sync::{Arc, RwLock, RwLockReadGuard},
};

use glfw::{Action, Key, Modifiers, MouseButton, WindowEvent};

use crate::{
  backend::Backend,
  logic::{
    ring::{self, CallbackResult, Mark, Wrap},
    DamageTracker, Ring,
  },
  render::{
    painter::{Description, Drone},
    Point,
  },
  widgets::{spreadsheet::Record, traits::InputEvent, Spreadsheet},
};

#[macro_export]
macro_rules! debug {
  ($($arg: tt), *) => {
    if std::env::var("DEBUG_MSGS").is_ok() {
      println!($($arg,) *);
    }
  }
}

pub struct Core {
  ring: Wrap<Ring>,
  damage_tracker: Wrap<DamageTracker>,

  cursor: Point,
}

impl Core {
  pub fn new() -> Self {
    let mut ring = Ring::new();

    let dt = DamageTracker::new();
    let damage_tracker = dt.push_to_ring(&mut ring);

    Self {
      ring: ring::wrap(ring),
      cursor: Point::new(0, 0),
      damage_tracker,
    }
  }

  pub unsafe fn on_exec(&mut self, b: &mut Backend) -> u64 {
    let mut mods = Modifiers::empty();

    loop {
      if b.drone.step() {
        return 1;
      }

      let events = glfw::flush_messages(&b.events);
      for (_, event) in events {
        let r = self.handle_event(&b.desc, &b.drone, event, &mut mods);
        if r != 0 {
          return r;
        }
      }
      self.drain_damage_tracker(&b.desc.read().unwrap(), &b.drone);

      self.draw(b);
    }
  }

  pub unsafe fn handle_event(
    &mut self,
    desc: &Arc<RwLock<Description>>,
    drone: &Drone,
    event: WindowEvent,
    mods: &mut Modifiers,
  ) -> u64 {
    let res = self.ring.write().unwrap().catch_transient_control_event(
      &desc.read().unwrap(),
      drone.feed(),
      &event,
    );
    match self.handle_callback_result_mut(
      &mut desc.write().unwrap(),
      drone,
      res,
      "Transient control",
    ) {
      Ok(true) => return 0,
      Err(c) => return c,
      _ => {}
    }
    let res = self.ring.write().unwrap().catch_key(
      &desc.read().unwrap(),
      drone,
      &event,
    );
    match self.handle_callback_result_mut(
      &mut desc.write().unwrap(),
      drone,
      res,
      "Click",
    ) {
      Ok(true) => return 0,
      Err(c) => return c,
      _ => {}
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

      // Mods
      WindowEvent::Key(
        Key::LeftControl | Key::RightControl,
        _,
        Action::Press,
        _,
      ) => {
        mods.set(Modifiers::Control, true);
      }
      WindowEvent::Key(
        Key::LeftControl | Key::RightControl,
        _,
        Action::Release,
        _,
      ) => {
        mods.set(Modifiers::Control, false);
      }

      // Resize
      WindowEvent::Size(w, h) => {
        desc.write().unwrap().resize(w, h);
        drone.resize(w, h);
        self.ring.write().unwrap().into_iter().for_each(|w| {
          w.0.write().unwrap().request_plot();
        });
      }

      // Mouse input
      WindowEvent::CursorPos(x, y) => {
        self.cursor = Point::new(x as i32, y as i32);
      }
      WindowEvent::MouseButton(MouseButton::Button1, Action::Press, _mods) => {
        let res = self.ring.write().unwrap().catch_click(
          &desc.read().unwrap(),
          drone,
          self.cursor,
        );
        if let Err(c) = self.handle_callback_result_mut(
          &mut desc.write().unwrap(),
          drone,
          res,
          "Click",
        ) {
          return c;
        }
      }

      // Scroll
      WindowEvent::Scroll(_x, y) => {
        if mods.contains(Modifiers::Control) {
          self.with_ssheet_mut(|s| s.scale(1.0 + y as f32 / 30.0));
        } else {
          self.with_ssheet_mut(|s| s.scroll(-y as i32 * 3));
        }
      }

      // Text input
      WindowEvent::Char(c) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Char(c),
        );
      }
      WindowEvent::Key(Key::Left, _, Action::Press | Action::Repeat, _) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Left,
        );
      }
      WindowEvent::Key(Key::Right, _, Action::Press | Action::Repeat, _) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Right,
        );
      }
      WindowEvent::Key(
        Key::Backspace,
        _,
        Action::Press | Action::Repeat,
        _,
      ) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Backspace,
        );
      }
      WindowEvent::Key(Key::Delete, _, Action::Press | Action::Repeat, _) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Delete,
        );
      }
      WindowEvent::Key(Key::Home, _, Action::Press | Action::Repeat, _) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Home,
        );
      }
      WindowEvent::Key(Key::End, _, Action::Press | Action::Repeat, _) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::End,
        );
      }
      WindowEvent::Key(
        Key::Enter,
        _,
        Action::Press | Action::Repeat,
        Modifiers::Control,
      ) => {
        self.ring.write().unwrap().catch_input_event(
          &desc.read().unwrap(),
          drone,
          InputEvent::Newline,
        );
      }

      _ => {
        println!("{:?}", event);
      }
    };

    0
  }

  pub fn with_ssheet_mut<R>(
    &mut self,
    f: impl FnOnce(&mut Spreadsheet) -> R,
  ) -> R {
    f(self
      .ring
      .write()
      .unwrap()
      .pull(&crate::logic::ring::Mark::Spreadsheet)
      .expect("Spreadsheet should always be on the ring")
      .write()
      .unwrap()
      .downcast_mut()
      .expect("Only spreadsheet should be marked as spreadsheet in the ring"))
  }

  pub fn draw(&mut self, b: &Backend) {
    b.drone.clear();
    self
      .ring
      .write()
      .unwrap()
      .draw(b.desc.clone(), &b.drone)
      .iter()
      .for_each(|e| println!("{}", e));
  }

  pub fn ring(&self) -> &Wrap<Ring> {
    &self.ring
  }

  pub fn pull_damage<E: Record>(&self) -> Vec<u8> {
    let mut buf = Cursor::new(vec![]);
    self
      .ring
      .read()
      .unwrap()
      .pull(&Mark::Spreadsheet)
      .unwrap()
      .read()
      .unwrap()
      .downcast_ref()
      .map(|ss| {
        self
          .damage_tracker
          .read()
          .unwrap()
          .serialize::<_, E>(&mut buf, ss);
      })
      .unwrap();
    buf.into_inner()
  }

  pub fn drain_damage_tracker(
    &mut self,
    desc: &RwLockReadGuard<Description>,
    drone: &Drone,
  ) {
    let mut t = self.damage_tracker.write().unwrap();
    t.drain().for_each(|r| {
      self
        .handle_callback_result(desc, drone, (r, Mark::DamageTracker), "Damage")
        .unwrap();
    });
  }

  pub fn wipe_damage_tracker(&mut self) {
    self.damage_tracker.write().unwrap().wipe();
  }

  /// Act on callback result.
  ///
  /// # Return
  ///
  /// Returns `Ok(false)` if `r` is [CallbackResult::Pass], `Ok(true)` otherwise.
  /// `Err(code)` is returned wher `r` is [CallbackResult::ExitCode(code)],
  /// specifically `code` will never be 0.
  /// `what` and `who` are used to describe callback in case of error:
  /// `what` describes the type of callback, `who` - marked object, sourcing
  /// the callback.
  fn handle_callback_result_mut(
    &mut self,
    desc: &mut Description,
    drone: &Drone,
    (r, who): (CallbackResult, Mark),
    what: &str,
  ) -> Result<bool, u64> {
    if r.is_pass() {
      return Ok(false);
    }

    match r {
      CallbackResult::Error(e) => {
        println!("{} callback by {:?} failed: {}", what, who, e);
      }
      CallbackResult::Push(x) => x.push_to_ring(self.ring.write().unwrap()),
      CallbackResult::Modify(m, f) => {
        f(self.ring.write().unwrap().pull(&m), desc, drone)
      }
      CallbackResult::Damage(f) => f(&mut self.damage_tracker.write().unwrap()),
      CallbackResult::ExitCode(c) => {
        debug_assert_ne!(c, 0);
        return Err(c);
      }
      CallbackResult::Mode(m) => {
        desc.set_mode(m);
      }

      _ => {}
    }
    return Ok(true);
  }

  /// Same as [Ring::handle_callback_result_mut], but [CallbackResult::Push]
  /// and [CallbackResult::Damage] are considered errors.
  fn handle_callback_result(
    &self,
    desc: &Description,
    drone: &Drone,
    (r, who): (CallbackResult, Mark),
    what: &str,
  ) -> Result<bool, u64> {
    if r.is_pass() {
      return Ok(false);
    }

    match r {
      CallbackResult::Error(e) => {
        println!("{} callback by {:?} failed: {}", what, who, e);
      }
      CallbackResult::Damage(_) => {
        println!("{} immutable callback by {:?} returned Damage", what, who);
      }
      CallbackResult::Mode(_) => {
        println!("{} immutable callback by {:?} returned Mode", what, who);
      }
      CallbackResult::Push(_) => {
        println!("{} immutable callback by {:?} returned Push", what, who);
      }
      CallbackResult::Modify(m, f) => {
        f(self.ring.read().unwrap().pull(&m), desc, drone)
      }
      CallbackResult::ExitCode(c) => {
        debug_assert_ne!(c, 0);
        return Err(c);
      }

      _ => {}
    }
    return Ok(true);
  }
}
