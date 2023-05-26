use std::{
  collections::VecDeque,
  io::Cursor,
  sync::{Arc, RwLock},
};

use glfw::{Action, Key, Modifiers, MouseButton, WindowEvent};

use crate::{
  backend::Backend,
  data::{File, FileCallback, GenericFile, Record, Tag},
  logic::{
    ring::{self, Mark, Wrap},
    CallbackResult, DamageTracker, Ring,
  },
  render::{painter::Drone, Point},
  widgets::{traits::InputEvent, Spreadsheet},
  Description,
};

#[macro_export]
macro_rules! debug {
  ($($arg: tt), *) => {
    if std::env::var("DEBUG_MSGS").is_ok() {
      println!($($arg,) *);
    }
  }
}

type Files = Vec<(Tag, Wrap<dyn GenericFile>)>;

pub struct Core {
  ring: Wrap<Ring>,
  damage_tracker: Wrap<DamageTracker>,
  data: Files,
  ext_queue: VecDeque<(CallbackResult, &'static str)>,

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
      data: vec![],
      ext_queue: VecDeque::new(),
    }
  }

  pub unsafe fn on_exec(&mut self, b: &mut Backend) -> u64 {
    let mut mods = Modifiers::empty();

    loop {
      if b.drone.step() {
        return 1;
      }

      let q: Vec<(CallbackResult, &str)> = self.ext_queue.drain(..).collect();
      q.into_iter().for_each(|(r, what)| {
        self
          .handle_callback_result(
            &mut b.desc.write().unwrap(),
            &b.drone,
            (r, Mark::None),
            what,
          )
          .expect("jobs can't request exits");
      });

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
    match self.handle_callback_result(
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
    match self.handle_callback_result(
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
          mods,
          self.cursor,
        );
        if let Err(c) = self.handle_callback_result(
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

  /// Arbitrry actions can be queued in form of callback results.
  /// In such form they will be marked as [Mark::None] and executed
  /// in an asyncronous maner.
  pub fn job(&mut self, what: &'static str, r: CallbackResult) {
    self.ext_queue.push_back((r, what));
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

  pub fn pull_damage<R: Record>(&self, f: &Wrap<File<R>>) -> Vec<u8> {
    let mut buf = Cursor::new(vec![]);
    let dt = self.damage_tracker.read().unwrap();
    dt.serialize::<_, R>(&mut buf, &f.read().unwrap());
    buf.into_inner()
  }

  pub fn drain_damage_tracker(&mut self, desc: &Description, drone: &Drone) {
    let mut t = self.damage_tracker.write().unwrap();
    let mut d = &mut self.data;
    t.drain().for_each(|r| {
      if let CallbackResult::File(t, f) = r {
        handle_file_callback(&mut d, desc, drone, t, f);
      } else {
        panic!("unexpected result from dtracker: {:?}", r);
      }
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
  fn handle_callback_result(
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
      CallbackResult::Read(t, f) => {
        let file = self
          .data
          .iter()
          .find(|(_t, _)| *_t == t)
          .map(|f| &f.1)
          .map(|l| l.clone());
        let w = self.ring.read().unwrap().pull(&who);
        let r = f(w, file, desc, drone);
        self
          .handle_callback_result(desc, drone, (r, who), what)
          .unwrap();
      }
      CallbackResult::Push(x) => x.push_to_ring(self.ring.write().unwrap()),
      CallbackResult::File(t, f) => {
        handle_file_callback(&mut self.data, desc, drone, t, f)
      }
      CallbackResult::Damage(f) => f(&mut self.damage_tracker.write().unwrap()),
      CallbackResult::ExitCode(c) => {
        debug_assert_ne!(c, 0);
        return Err(c);
      }
      CallbackResult::Mode(m) => {
        desc.set_mode(m);
      }
      CallbackResult::Join(a, b) => {
        self.handle_callback_result(desc, drone, (*a, who), what)?;
        self.handle_callback_result(desc, drone, (*b, who), what)?;
      }
      CallbackResult::Die => {
        self
          .ring()
          .write()
          .unwrap()
          .kill_transient(drone.feed(), who);
      }

      _ => {}
    }
    return Ok(true);
  }

  pub fn add_data(&mut self, t: Tag, f: Wrap<dyn GenericFile>) {
    self.data.push((t, f));
  }
}

fn handle_file_callback(
  data: &mut Files,
  desc: &Description,
  drone: &Drone,
  t: Tag,
  f: FileCallback,
) {
  data
    .iter_mut()
    .find(|(_t, _)| *_t == t)
    .map(|f| &f.1)
    .as_mut()
    .map(|x| f(x.clone(), desc, drone));
}
