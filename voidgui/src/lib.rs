extern crate gl;
extern crate sdl2;

use std::time::Instant;

use render::{shapes::Texture, Area, Point, TextTable};
use widgets::window::*;

mod render;
mod widgets;

#[no_mangle]
pub extern "C" fn void_gui_init() -> u64 {
  let sdl = sdl2::init().unwrap();
  let video_subsystem = sdl.video().unwrap();
  let gl_attr = video_subsystem.gl_attr();
  gl_attr.set_context_profile(sdl2::video::GLProfile::Core);
  gl_attr.set_context_version(1, 0);
  let window = video_subsystem
    .window("Void", 800, 600)
    .opengl()
    // .resizable()
    .build()
    .unwrap();

  let _gl_context: sdl2::video::GLContext = window.gl_create_context().unwrap();
  let _gl = gl::load_with(|s| {
    video_subsystem.gl_get_proc_address(s) as *const std::os::raw::c_void
  });

  let (w, table) = unsafe {
    let w = VoidWindow::new(window);

    {
      let (h, w) = w.size();
      gl::Viewport(0, 0, h as i32, w as i32);
      gl::ClearColor(0.9, 0.9, 0.9, 1.0);
    }

    let mut table = TextTable::from_text(
      w.painter(),
      2,
      2,
      vec!["A1", "B", "C", "D"].as_slice(),
    )
    .unwrap();
    table.set_origin(Point::new(100, 100));
    // table.plot(w.painter()).unwrap();

    //     let rect = Rectangle::new((0.0, 0.0, 0.0, 1.0));
    //     rect
    //       .plot(w.painter(), &Area::new(100, 100, 200, 400))
    //       .unwrap();

    //     let grid = Grid::new((0.7, 0.7, 0.7, 1.0));
    //     grid
    //       .plot(
    //         w.painter(),
    //         2,
    //         2,
    //         &vec![0.3, 0.7],
    //         &vec![0.5, 0.5],
    //         &Area::new(110, 110, 180, 380),
    //       )
    //       .unwrap();

    (w, table)
  };

  let mut event_pump = sdl.event_pump().unwrap();
  'main: loop {
    for event in event_pump.poll_iter() {
      match event {
        sdl2::event::Event::Quit { .. } => break 'main,
        _ => {}
      }
    }

    unsafe {
      gl::Clear(gl::COLOR_BUFFER_BIT);
      table.draw(w.painter()).unwrap();
    }

    render::painter::pop_gl_error();

    w.swap();
  }

  1
}
