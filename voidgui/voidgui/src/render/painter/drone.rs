use std::{
  sync::{
    mpsc::{self, Receiver, SyncSender},
    Arc, RwLock,
  },
  thread,
};

use glfw::{Context, Window};

use crate::render::{
  painter::comres::CommonResources,
  shaders::{Shader, Shaders},
  shapes::{
    rectangle::{self, Style},
    Rectangle,
  },
  Color,
};
use crate::{
  colorscheme::BACKGROUND,
  render::shapes::{Grid, TextureData},
};
use crate::{
  debug,
  render::{painter::buffer::Buffer, shapes::Texture},
};

use super::Description;

#[derive(Debug)]
enum Command {
  Step,
  Clear,
  Kill,

  NewRectangles(usize, rectangle::Style),
  PlotRectangle(usize, [f32; 8]),
  DrawRectangle(usize),

  NewGrids(usize, Color),
  PlotGrid(usize, Vec<f32>),
  DrawGrid(usize),

  NewTextures(usize),
  WriteTexture(usize, TextureData),
  PlotTexture(usize, [f32; 16]),
  DrawTexture(usize),
}

enum Response {
  InitComplete(Events),

  ShouldClose(bool),

  NewShapes(Vec<usize>),
}

macro_rules! receive {
  ($rx:expr, $pat:pat, $if:expr) => {
    receive_or!($rx, $pat, $if, panic!())
  };
}

macro_rules! receive_or {
  ($rx:expr, $pat:pat, $if:expr, $else:expr) => {
    if let Some($pat) = $rx.iter().peekable().find(|m| matches!(m, $pat)) {
      $if
    } else {
      $else
    }
  };
}

type Events = Receiver<(f64, glfw::WindowEvent)>;

pub struct Drone {
  resp: Receiver<Response>,
  dir_feed: SyncSender<Command>,
  ctr_feed: DroneFeed,
}

impl Drone {
  pub unsafe fn new(
    desc: Arc<RwLock<Description>>,
    w: u32,
    h: u32,
  ) -> Result<(Self, Events), &'static str> {
    let (feed, rx) = mpsc::sync_channel::<Command>(100);
    let (tx, resp) = mpsc::channel();

    let ctr = {
      let (_tx, rx) = mpsc::sync_channel(100);
      let tx = feed.clone();

      thread::Builder::new()
        .name("overseer".into())
        .spawn(move || loop {
          let c: Command = rx.try_recv().unwrap_or_else(|_| {
            let _from = std::time::Instant::now();
            debug!("overs idle...");
            let c = rx.recv().unwrap();
            debug!("overs idle for {:?}", (std::time::Instant::now() - _from));
            c
          });
          if c.is_kill() {
            break;
          }
          debug!("overs received {:?}", c);
          tx.send(c).unwrap();
        })
        .unwrap();

      _tx
    };

    thread::Builder::new()
      .name("drone".into())
      .spawn(move || {
        let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

        glfw.window_hint(glfw::WindowHint::ClientApi(
          glfw::ClientApiHint::OpenGlEs,
        ));
        glfw.window_hint(glfw::WindowHint::ContextVersion(3, 2));
        glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

        let (mut win, events) = glfw
          .create_window(w, h, "Void", glfw::WindowMode::Windowed)
          .expect("Failed to create GLFW window.");

        win.make_current();
        win.set_key_polling(true);
        win.set_size_polling(true);
        win.set_char_polling(true);
        win.set_mouse_button_polling(true);
        win.set_cursor_pos_polling(true);

        let _gl = gl::load_with(|s| glfw.get_proc_address_raw(s));

        gl::Viewport(0, 0, w as i32, h as i32);
        let (r, g, b, a) = BACKGROUND;
        gl::ClearColor(r, g, b, a);
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

        let shaders = Shaders::new();
        let common = CommonResources::allocate();
        let mut resources = (Buffer::new(), Buffer::new(), Buffer::new());

        tx.send(Response::InitComplete(events)).unwrap();

        loop {
          let c = rx.try_recv().unwrap_or_else(|_| {
            let _from = std::time::Instant::now();
            debug!("drone idle...");
            let c = rx.recv().unwrap();
            debug!("drone idle for {:?}", (std::time::Instant::now() - _from));
            c
          });
          debug!("drone received {:?}", c);
          if c.is_kill() {
            break;
          }
          c.handle(&shaders, &common, &desc, &mut resources, &mut win)
            .map(|r| tx.send(r));

          let error = gl::GetError();
          if error != gl::NO_ERROR {
            println!("Gl error: {}", error);
          }
        }
      })
      .unwrap();

    receive_or!(
      resp,
      Response::InitComplete(_es),
      Ok((
        Self {
          dir_feed: feed,
          ctr_feed: DroneFeed(ctr),
          resp,
        },
        _es,
      )),
      Err("Drone start failed")
    )
  }

  pub fn new_feed(&self) -> DroneFeed {
    self.ctr_feed.clone()
  }

  pub fn feed(&self) -> &DroneFeed {
    &self.ctr_feed
  }

  pub fn kill(&self) {
    self.ctr_feed.0.send(Command::Kill).unwrap();
    self.dir_feed.send(Command::Kill).unwrap();
  }

  pub fn get_rectangles(
    &self,
    n: usize,
    style: rectangle::Style,
  ) -> Option<Vec<usize>> {
    self
      .dir_feed
      .send(Command::NewRectangles(n, style))
      .unwrap();
    receive_or!(self.resp, Response::NewShapes(_ids), Some(_ids), None)
  }

  pub fn get_grids(&self, n: usize, color: Color) -> Option<Vec<usize>> {
    self.dir_feed.send(Command::NewGrids(n, color)).unwrap();
    receive_or!(self.resp, Response::NewShapes(_ids), Some(_ids), None)
  }

  pub fn get_textures(&self, n: usize) -> Option<Vec<usize>> {
    self.dir_feed.send(Command::NewTextures(n)).unwrap();
    receive_or!(self.resp, Response::NewShapes(_ids), Some(_ids), None)
  }

  pub fn step(&self) -> bool {
    self.ctr_feed.0.send(Command::Step).unwrap();
    let s = receive!(self.resp, Response::ShouldClose(_should), _should);
    s
  }

  pub fn clear(&self) {
    self.dir_feed.send(Command::Clear).unwrap();
  }
}

pub struct DroneFeed(SyncSender<Command>);

impl DroneFeed {
  pub fn plot_rectangle(&self, rect: usize, coords: [f32; 8]) {
    self
      .0
      .try_send(Command::PlotRectangle(rect, coords))
      .unwrap();
  }

  pub fn draw_rectangle(&self, rect: usize) {
    self.0.try_send(Command::DrawRectangle(rect)).unwrap();
  }

  pub fn plot_grid(&self, grid: usize, coords: Vec<f32>) {
    self.0.try_send(Command::PlotGrid(grid, coords)).unwrap();
  }

  pub fn draw_grid(&self, grid: usize) {
    self.0.try_send(Command::DrawGrid(grid)).unwrap();
  }

  pub fn plot_tex(&self, tex: usize, coords: [f32; 16]) {
    self.0.try_send(Command::PlotTexture(tex, coords)).unwrap();
  }

  pub fn bind_text(&self, tex: usize, data: TextureData) {
    self.0.try_send(Command::WriteTexture(tex, data)).unwrap();
  }

  pub fn draw_tex(&self, tex: usize) {
    self.0.try_send(Command::DrawTexture(tex)).unwrap();
  }
}

impl Clone for DroneFeed {
  fn clone(&self) -> Self {
    Self(self.0.clone())
  }
}

impl Command {
  #[inline]
  unsafe fn handle(
    self,
    shaders: &Shaders,
    common: &CommonResources,
    _: &Arc<RwLock<Description>>,
    res: &mut (Buffer<Rectangle>, Buffer<Texture>, Buffer<Grid>),
    win: &mut Window,
  ) -> Option<Response> {
    match self {
      Command::NewRectangles(n, style) => Some(Response::NewShapes(
        (0..n)
          .map(|_| res.0.put(Rectangle::new(style.clone())))
          .collect(),
      )),
      Command::PlotRectangle(n, vs) => {
        let rect = res.0.get(n);
        rect.res.bind();
        rect.res.bind_buffers();
        common.bind_rect_ebo();

        let id = match &mut rect.style {
          Style::Solid(_) => {
            shaders.common().enable_attribs();
            shaders.common().pos().id()
          }
          Style::Lit(_, a) => {
            *a = Some(crate::render::Area::new(0, 0, 100, 100));
            shaders.grad().enable_attribs();
            shaders.grad().pos().id()
          }
        };

        gl::BufferData(
          gl::ARRAY_BUFFER,
          (vs.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
          vs.as_ptr() as *const gl::types::GLvoid,
          gl::STATIC_DRAW,
        );
        gl::VertexAttribPointer(
          id as u32,
          2,
          gl::FLOAT,
          gl::FALSE,
          0,
          std::ptr::null(),
        );
        gl::BindVertexArray(0);

        None
      }
      Command::DrawRectangle(n) => {
        let rect = res.0.get(n);
        match rect.style {
          Style::Solid(c) => {
            shaders.common().set_used();
            shaders.common().set_color(&c);
          }
          Style::Lit(c, a) => {
            shaders.grad().set_used();
            shaders.grad().set_color(&c);
            shaders.grad().set_constr(&a.unwrap());
          }
        };

        rect.res.bind();
        gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

        None
      }
      Command::NewGrids(n, color) => Some(Response::NewShapes(
        (0..n).map(|_| res.2.put(Grid::new(color))).collect(),
      )),
      Command::PlotGrid(n, vs) => {
        let grid = res.2.get(n);
        grid.vertices = vs.len() as i32;

        grid.res.bind();
        grid.res.bind_buffers();
        shaders.common().enable_attribs();

        gl::BufferData(
          gl::ARRAY_BUFFER,
          (vs.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
          vs.as_ptr() as *const gl::types::GLvoid,
          gl::STATIC_DRAW,
        );

        gl::VertexAttribPointer(
          shaders.common().pos().id() as u32,
          2,
          gl::FLOAT,
          gl::FALSE,
          0,
          std::ptr::null(),
        );
        gl::BindVertexArray(0);

        None
      }
      Command::DrawGrid(n) => {
        let grid = res.2.get(n);
        shaders.common().set_used();
        shaders.common().set_color(&grid.color);
        grid.res.bind();
        gl::DrawArrays(gl::LINES, 0, grid.vertices);

        None
      }
      Command::NewTextures(n) => Some(Response::NewShapes(
        (0..n).map(|_| res.1.put(Texture::new())).collect(),
      )),
      Command::WriteTexture(n, d) => {
        let tex = res.1.get(n);
        tex.texture.map(|s| gl::DeleteTextures(1, &s));
        let mut t: gl::types::GLuint = 0;
        gl::GenTextures(1, &mut t);
        tex.texture = Option::Some(t);

        tex.res.bind();
        gl::BindTexture(gl::TEXTURE_2D, t);
        gl::TexParameteri(
          gl::TEXTURE_2D,
          gl::TEXTURE_MIN_FILTER,
          gl::LINEAR as i32,
        );
        gl::TexParameteri(
          gl::TEXTURE_2D,
          gl::TEXTURE_MAG_FILTER,
          gl::LINEAR as i32,
        );
        gl::TexImage2D(
          gl::TEXTURE_2D,
          0,
          gl::RGBA as i32,
          d.0,
          d.1,
          0,
          gl::BGRA,
          gl::UNSIGNED_BYTE,
          d.2.as_ptr() as *const gl::types::GLvoid,
        );
        gl::BindVertexArray(0);

        None
      }
      Command::PlotTexture(n, vs) => {
        let tex = res.1.get(n);
        tex.res.bind();
        tex.res.bind_buffers();
        common.bind_rect_ebo();
        shaders.tex().enable_attribs();
        gl::BufferData(
          gl::ARRAY_BUFFER,
          (vs.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
          vs.as_ptr() as *const gl::types::GLvoid,
          gl::STATIC_DRAW,
        );
        gl::VertexAttribPointer(
          shaders.tex().pos().id() as u32,
          2,
          gl::FLOAT,
          gl::FALSE,
          (4 * std::mem::size_of::<f32>()) as i32,
          std::ptr::null() as *const gl::types::GLvoid,
        );
        gl::VertexAttribPointer(
          shaders.tex().tex().id() as u32,
          2,
          gl::FLOAT,
          gl::FALSE,
          (4 * std::mem::size_of::<f32>()) as i32,
          (2 * std::mem::size_of::<f32>()) as *const gl::types::GLvoid,
        );
        gl::BindVertexArray(0);

        None
      }
      Command::DrawTexture(n) => {
        let tex = res.1.get(n);
        let t = tex.texture?;

        shaders.tex().set_used();
        tex.res.bind();
        gl::BindTexture(gl::TEXTURE_2D, t);
        gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

        None
      }
      Command::Step => {
        win.swap_buffers();
        win.glfw.poll_events();
        Some(Response::ShouldClose(win.should_close()))
      }
      Command::Kill => panic!(),
      Command::Clear => {
        gl::Clear(gl::COLOR_BUFFER_BIT);
        None
      }
    }
  }

  /// Returns `true` if the command is [`Kill`].
  ///
  /// [`Kill`]: Command::Kill
  #[must_use]
  fn is_kill(&self) -> bool {
    matches!(self, Self::Kill)
  }
}
