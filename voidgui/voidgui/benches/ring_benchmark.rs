use std::sync::RwLockReadGuard;

use rand::Rng;

// use rayon::prelude::*;

use criterion::{criterion_group, criterion_main, Criterion};
use voidgui::logic::ring::Mark;
use voidgui::logic::CallbackResult;
use voidgui::render::painter::{Description, DroneFeed};
use voidgui::render::Point;
use voidgui::widgets;
use voidgui::widgets::traits::Parent;
use voidgui::{
  backend::Backend,
  core::Core,
  logic::ring,
  render::{
    painter::Drone, text_table::CellStyle::Normal, Area, Origin, TextTable,
  },
  widgets::traits::{ClickSink, Clickable, Drawable, Widget},
};
use voidmacro::{ClickableMenu, Menu};

#[derive(Menu, ClickableMenu)]
struct W {
  table: TextTable,
  _ind: usize,
}

impl W {
  fn new(
    ind: usize,
    desc: RwLockReadGuard<Description>,
    drone: &Drone,
  ) -> Self {
    Self {
      table: unsafe {
        TextTable::from_text(
          &desc,
          drone,
          5,
          2,
          Normal,
          &["A", "B", "C", "D", "E", "F", "G", "H", "J", "I"],
        )
        .unwrap()
      },
      _ind: ind,
    }
  }
}

impl Drawable for W {
  fn plot(
    &mut self,
    desc: RwLockReadGuard<Description>,
    feed: DroneFeed,
  ) -> Result<(), widgets::Error> {
    // println!("plot {}", self.ind);
    self.table.plot(&desc, &feed)
  }

  unsafe fn draw(&mut self, feed: DroneFeed) -> Result<(), widgets::Error> {
    self.table.draw(&feed)
  }
}

impl ClickSink for W {
  fn onclick(
    &self,
    _: &RwLockReadGuard<Description>,
    _: &Drone,
    _: Point,
  ) -> CallbackResult {
    CallbackResult::None
  }
}

impl Parent for W {
  fn nth_child(&self, n: usize) -> Option<Origin> {
    Some(Origin::new(
      n as u16 * 10,
      n as u16 * 10,
      voidgui::render::OriginPole::TopLeft,
    ))
  }
}

pub fn draw_bench_plot_all(c: &mut Criterion) {
  let (back, mut core, _) = setup();

  c.bench_function("plot a lot", |b| {
    b.iter(|| {
      core.ring_mut().into_iter().for_each(|w| {
        w.0.write().unwrap().request_plot();
      });
      core.draw(&back);

      back.drone.sync();
    })
  });
}

pub fn draw_bench_plot_one(c: &mut Criterion) {
  let (back, mut core, mut rng) = setup();

  c.bench_function("plot one", |b| {
    b.iter(|| {
      core
        .ring_mut()
        .into_iter()
        .nth(rng.gen_range(0..10 as usize))
        .map(|w| {
          w.0.write().unwrap().request_plot();
        })
        .unwrap();
      core.draw(&back);
      back.drone.sync();
    })
  });
}

pub fn maintenance_bench(c: &mut Criterion) {
  let (_, mut core, _) = setup();

  c.bench_function("request plot for all", |b| {
    b.iter(|| {
      core.ring_mut().into_iter().for_each(|w| {
        w.0.write().unwrap().request_plot();
      });
    })
  });
}

pub fn event_bench(c: &mut Criterion) {
  let (back, mut core, mut rng) = setup();

  c.bench_function("handle click", |b| {
    b.iter(|| {
      unsafe {
        core.handle_event(
          &back.desc,
          &back.drone,
          glfw::WindowEvent::CursorPos(
            rng.gen_range(0..=200) as f64,
            rng.gen_range(0..=200) as f64,
          ),
        );
        core.handle_event(
          &back.desc,
          &back.drone,
          glfw::WindowEvent::MouseButton(
            glfw::MouseButton::Button1,
            glfw::Action::Press,
            glfw::Modifiers::empty(),
          ),
        );
      };
    })
  });
}

fn setup() -> (Backend, Core, rand::rngs::ThreadRng) {
  let back = unsafe { Backend::new(400, 400) };
  let mut core = Core::new();
  let rng = rand::thread_rng();

  // push parents
  for m in [Mark::_Test1, Mark::_Test2] {
    let mut w = W::new(m as usize, back.desc.read().unwrap(), &back.drone);
    w.set_origin(&Origin::new(0, 0, voidgui::render::OriginPole::TopLeft));
    let r = ring::wrap(w);
    core.ring_mut().push_click_sink(r.clone(), m);
    core.ring_mut().push_parent(r.clone(), m);
    core.ring_mut().push(r, m, Mark::None, 0);
  }

  for m in [Mark::_Test1, Mark::_Test2] {
    for i in 0..4 {
      let w = W::new(
        2 + i + 4 * m as usize,
        back.desc.read().unwrap(),
        &back.drone,
      );
      let r = ring::wrap(w);
      core.ring_mut().push_click_sink(r.clone(), Mark::None);
      core.ring_mut().push(r, Mark::None, m, i as usize);
    }
  }

  (back, core, rng)
}

criterion_group! {
    name = hard;
    config = Criterion::default().significance_level(0.08).sample_size(5000);
    targets = draw_bench_plot_one, draw_bench_plot_all
}
criterion_group! {
    name = easy;
    config = Criterion::default().sample_size(500);
    targets = event_bench, maintenance_bench
}
criterion_main!(hard, easy,);
