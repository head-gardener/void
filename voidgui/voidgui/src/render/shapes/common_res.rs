pub struct CommonRes {
  vao: gl::types::GLuint,
  vbo: gl::types::GLuint,
}

#[cfg(not(test))]
impl CommonRes {
  pub unsafe fn allocate() -> Self {
    let mut vao: gl::types::GLuint = 0;
    let mut vbo: gl::types::GLuint = 0;
    gl::GenVertexArrays(1, &mut vao);
    gl::GenBuffers(1, &mut vbo);
    Self { vao, vbo }
  }

  pub unsafe fn bind(&self) -> () {
    gl::BindVertexArray(self.vao);
  }

  pub unsafe fn bind_buffers(&self) -> () {
    gl::BindVertexArray(self.vao);
    gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
  }
}

#[cfg(not(test))]
impl Drop for CommonRes {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteVertexArrays(1, &self.vao);
      gl::DeleteBuffers(1, &self.vbo);
    }
  }
}

#[cfg(test)]
impl CommonRes {
  pub unsafe fn allocate() -> Self {
    Self { vao: 0, vbo: 0 }
  }

  pub unsafe fn bind(&self) -> () {}

  pub unsafe fn bind_buffers(&self) -> () {}
}

#[cfg(test)]
impl Drop for CommonRes {
  fn drop(&mut self) {}
}
