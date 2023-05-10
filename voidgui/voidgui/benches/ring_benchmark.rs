use rand::Rng;

use criterion::{criterion_group, criterion_main, Criterion};
use voidgui::logic::CallbackResult;
use voidgui::widgets;
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
}

impl W {
  fn new(p: &Painter) -> Self {
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
    }
  }
}

impl Drawable for W {
  unsafe fn plot(&mut self, painter: &Painter) -> Result<(), widgets::Error> {
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

pub fn draw_bench(c: &mut Criterion) {
  let (back, mut core, _) = setup();

  c.bench_function("draw a lot", |b| b.iter(|| core.draw(&back)));
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
  let back = unsafe { Backend::new(200, 200) };
  let mut core = Core::new();
  let rng = rand::thread_rng();

  for i in 0..10 {
    let mut w = W::new(&back.painter);
    w.set_origin(&Origin::new(
      10 * i,
      10 * i,
      voidgui::render::OriginPole::TopLeft,
    ));
    let r = ring::wrap(w);
    core
      .ring_mut()
      .push_click_sink(r.clone(), voidgui::logic::ring::Mark::None);
    core.ring_mut().push(
      r,
      voidgui::logic::ring::Mark::None,
      voidgui::logic::ring::Mark::None,
      0,
    );
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
