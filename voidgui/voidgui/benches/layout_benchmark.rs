use criterion::{black_box, criterion_group, criterion_main, Criterion};
use voidgui::{logic::Layout, render::Size};

pub fn layout_bench(c: &mut Criterion) {
  let ss = vec![
    Size::new(100, 40),
    Size::new(200, 30),
    Size::new(50, 50),
    Size::new(150, 60),
    Size::new(100, 40),
    Size::new(200, 30),
    Size::new(50, 50),
    Size::new(150, 60),
    Size::new(50, 50),
    Size::new(150, 60),
  ];
  c.bench_function("new layout", |b| {
    b.iter(|| {
      Layout::from_sizes(5, 2, black_box(ss.as_slice()));
    })
  });
}

criterion_group!(benches, layout_bench);
criterion_main!(benches);
