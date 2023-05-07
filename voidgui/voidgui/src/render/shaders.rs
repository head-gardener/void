use std::ffi::{CStr, CString};

use super::{Area, Color};

pub struct Shaders {
  common: CommonShader,
  grad: GradShader,
  tex: TexShader,
}

impl Shaders {
  pub fn new() -> Self {
    let common = CommonShader::new(
      Program::from_files(
        include_str!("shaders/common.vert"),
        include_str!("shaders/common.frag"),
      )
      .expect("common shader to compile"),
    );
    let grad = GradShader::new(
      Program::from_files(
        include_str!("shaders/common.vert"),
        include_str!("shaders/grad.frag"),
      )
      .expect("grad shader to compile"),
    );
    let tex = TexShader::new(
      Program::from_files(
        include_str!("shaders/tex.vert"),
        include_str!("shaders/tex.frag"),
      )
      .expect("tex shader to compile"),
    );
    Self { common, tex, grad }
  }

  pub fn tex(&self) -> &TexShader {
    &self.tex
  }

  pub fn common(&self) -> &CommonShader {
    &self.common
  }

  pub fn grad(&self) -> &GradShader {
    &self.grad
  }
}

pub struct CommonShader {
  prog: Program,
  color: Uniform,
  pos: Attrib,
}

impl CommonShader {
  pub fn set_color(&self, color: &Color) {
    unsafe {
      gl::Uniform4f(self.color.id, color.0, color.1, color.2, color.3);
    };
  }

  pub fn pos(&self) -> &Attrib {
    &self.pos
  }
}

pub struct GradShader {
  prog: Program,
  color1: Uniform,
  color2: Uniform,
  constr: Uniform,
  pos: Attrib,
}

impl GradShader {
  pub fn set_color(&self, color: &Color) {
    unsafe {
      gl::Uniform4f(self.color1.id, color.0, color.1, color.2, color.3);
      gl::Uniform4f(self.color2.id, color.0 + 0.1, color.1 + 0.1, color.2 + 0.1, color.3);
    };
  }

  pub fn set_constr(&self, a: &Area) {
    unsafe {
      gl::Uniform4f(
        self.constr.id,
        a.x as f32,
        a.y as f32,
        a.width as f32,
        a.height as f32,
      );
    };
  }

  pub fn pos(&self) -> &Attrib {
    &self.pos
  }
}

pub struct TexShader {
  prog: Program,
  pos: Attrib,
  tex: Attrib,
}

impl TexShader {
  pub fn pos(&self) -> &Attrib {
    &self.pos
  }

  pub fn tex(&self) -> &Attrib {
    &self.tex
  }
}

impl Shader for CommonShader {
  fn new(prog: Program) -> Self {
    Self {
      color: Uniform::get_uniform(&prog, "color"),
      pos: Attrib::get_attrib(&prog, "pos"),
      prog,
    }
  }

  fn prog(&self) -> &Program {
    &self.prog
  }

  fn attribs(&self) -> Vec<i32> {
    vec![self.pos.id()]
  }
}

impl Shader for GradShader {
  fn new(prog: Program) -> Self {
    Self {
      constr: Uniform::get_uniform(&prog, "constr"),
      color1: Uniform::get_uniform(&prog, "color1"),
      color2: Uniform::get_uniform(&prog, "color2"),
      pos: Attrib::get_attrib(&prog, "pos"),
      prog,
    }
  }

  fn prog(&self) -> &Program {
    &self.prog
  }

  fn attribs(&self) -> Vec<i32> {
    vec![self.pos.id()]
  }
}

impl Shader for TexShader {
  fn new(prog: Program) -> Self {
    Self {
      tex: Attrib::get_attrib(&prog, "texcoord"),
      pos: Attrib::get_attrib(&prog, "pos"),
      prog,
    }
  }

  fn prog(&self) -> &Program {
    &self.prog
  }

  fn attribs(&self) -> Vec<i32> {
    vec![self.pos.id(), self.tex.id()]
  }
}

pub trait Shader {
  fn new(prog: Program) -> Self;
  fn prog(&self) -> &Program;
  fn attribs(&self) -> Vec<i32>;

  fn set_used(&self) -> () {
    unsafe {
      gl::UseProgram(self.prog().id);
    }
  }

  fn enable_attribs(&self) -> () {
    self
      .attribs()
      .iter()
      .for_each(|a| unsafe { gl::EnableVertexAttribArray(*a as u32) });
  }
}

pub struct Uniform {
  id: gl::types::GLint,
}

impl Uniform {
  pub fn get_uniform(prog: &Program, uniform: &str) -> Self {
    let str = CString::new(uniform).expect("valid argument.");
    Self {
      id: unsafe { gl::GetUniformLocation(prog.id, str.as_ptr()) },
    }
  }
}

pub struct Attrib {
  id: gl::types::GLint,
}

impl Attrib {
  pub fn get_attrib(prog: &Program, attrib: &str) -> Self {
    let str = CString::new(attrib).expect("valid argument.");
    Self {
      id: unsafe { gl::GetAttribLocation(prog.id, str.as_ptr()) },
    }
  }

  pub fn id(&self) -> i32 {
    self.id
  }
}

pub struct Program {
  id: gl::types::GLuint,
}

impl Program {
  pub fn from_files(vert: &str, frag: &str) -> Result<Self, String> {
    let vert_shader =
      GlShader::from_vert_source(&CString::new(vert).unwrap()).unwrap();

    let frag_shader =
      GlShader::from_frag_source(&CString::new(frag).unwrap()).unwrap();

    Program::from_shaders(&[vert_shader, frag_shader])
  }

  pub fn from_shaders(shaders: &[GlShader]) -> Result<Self, String> {
    let program_id = unsafe { gl::CreateProgram() };

    for shader in shaders {
      unsafe {
        gl::AttachShader(program_id, shader.id());
      }
    }

    unsafe {
      gl::LinkProgram(program_id);
    }

    let mut success: gl::types::GLint = 1;
    unsafe {
      gl::GetProgramiv(program_id, gl::LINK_STATUS, &mut success);
    }

    if success == 0 {
      let mut len: gl::types::GLint = 0;
      unsafe {
        gl::GetProgramiv(program_id, gl::INFO_LOG_LENGTH, &mut len);
      }

      let error = create_whitespace_cstring_with_len(len as usize);

      unsafe {
        gl::GetProgramInfoLog(
          program_id,
          len,
          std::ptr::null_mut(),
          error.as_ptr() as *mut gl::types::GLchar,
        );
      }

      return Err(error.to_string_lossy().into_owned());
    }

    for shader in shaders {
      unsafe {
        gl::DetachShader(program_id, shader.id());
      }
    }

    Ok(Self { id: program_id })
  }

  pub fn id(&self) -> gl::types::GLuint {
    self.id
  }
}

impl Drop for Program {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteProgram(self.id);
    }
  }
}

pub struct GlShader {
  id: gl::types::GLuint,
}

impl GlShader {
  pub fn from_source(
    source: &CStr,
    kind: gl::types::GLenum,
  ) -> Result<GlShader, String> {
    let id = shader_from_source(source, kind)?;
    Ok(GlShader { id })
  }

  pub fn from_vert_source(source: &CStr) -> Result<GlShader, String> {
    GlShader::from_source(source, gl::VERTEX_SHADER)
  }

  pub fn from_frag_source(source: &CStr) -> Result<GlShader, String> {
    GlShader::from_source(source, gl::FRAGMENT_SHADER)
  }

  pub fn id(&self) -> gl::types::GLuint {
    self.id
  }
}

impl Drop for GlShader {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteShader(self.id);
    }
  }
}

fn shader_from_source(
  source: &CStr,
  kind: gl::types::GLenum,
) -> Result<gl::types::GLuint, String> {
  let id = unsafe { gl::CreateShader(kind) };
  unsafe {
    gl::ShaderSource(id, 1, &source.as_ptr(), std::ptr::null());
    gl::CompileShader(id);
  }

  let mut success: gl::types::GLint = 1;
  unsafe {
    gl::GetShaderiv(id, gl::COMPILE_STATUS, &mut success);
  }

  if success == 0 {
    let mut len: gl::types::GLint = 0;
    unsafe {
      gl::GetShaderiv(id, gl::INFO_LOG_LENGTH, &mut len);
    }

    let error = create_whitespace_cstring_with_len(len as usize);

    unsafe {
      gl::GetShaderInfoLog(
        id,
        len,
        std::ptr::null_mut(),
        error.as_ptr() as *mut gl::types::GLchar,
      );
    }

    return Err(error.to_string_lossy().into_owned());
  }

  Ok(id)
}

fn create_whitespace_cstring_with_len(len: usize) -> CString {
  // allocate buffer of correct size
  let mut buffer: Vec<u8> = Vec::with_capacity(len + 1);
  // fill it with len spaces
  buffer.extend([b' '].iter().cycle().take(len));
  // convert buffer to CString
  unsafe { CString::from_vec_unchecked(buffer) }
}
