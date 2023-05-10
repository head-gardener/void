use rand::Rng;

use criterion::{criterion_group, criterion_main, Criterion};
use voidgui::logic::ring::Mark;
use voidgui::logic::CallbackResult;
use voidgui::widgets;
use voidgui::widgets::traits::Parent;
use voidgui::{
  backend::Backend,
  core::Core,
  logic::ring,
  render::{
    painter::Painter, text_table::CellStyle::Normal, Area, Origin, TextTable,
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
  fn new(ind: usize, p: &Painter) -> Self {
    Self {
      table: unsafe {
        TextTable::from_text(
          p,
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
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), widgets::Error> {
    // println!("plot {}", self.ind);
    let r = self.table.plot(painter);
    self.table.request_plot();
    r
  }

  fn draw(&self, _: &Painter) -> Result<(), widgets::Error> {
    Ok(())
  }
}

impl ClickSink for W {
  fn onclick(&self, _: &Painter, _: voidgui::render::Point) -> CallbackResult {
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

pub fn draw_bench(c: &mut Criterion) {
  let (back, mut core, _) = setup();

  c.bench_function("draw a lot", |b| {
    b.iter(|| {
      // println!("from");
      core.draw(&back);
      // println!("to");
    })
  });
}

pub fn event_bench(c: &mut Criterion) {
  let (mut back, mut core, mut rng) = setup();

  c.bench_function("handle click", |b| {
    b.iter(|| {
      unsafe {
        core.handle_event(
          &mut back.painter,
          glfw::WindowEvent::CursorPos(
            rng.gen_range(0..=200) as f64,
            rng.gen_range(0..=200) as f64,
          ),
        );
        core.handle_event(
          &mut back.painter,
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
    let mut w = W::new(m as usize, &back.painter);
    w.set_origin(&Origin::new(0, 0, voidgui::render::OriginPole::TopLeft));
    unsafe { w.plot(&back.painter).unwrap() };
    let r = ring::wrap(w);
    core.ring_mut().push_click_sink(r.clone(), m);
    core.ring_mut().push_parent(r.clone(), m);
    core.ring_mut().push(r, m, Mark::None, 0);
  }

  for m in [Mark::_Test1, Mark::_Test2] {
    for i in 0..6 {
      let w = W::new(2 + i + 6 * m as usize, &back.painter);
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
    targets = draw_bench
}
criterion_group! {
    name = easy;
    config = Criterion::default().sample_size(500);
    targets = event_bench
}
criterion_main!(hard, easy,);
