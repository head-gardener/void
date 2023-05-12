pub struct CommonResources {
  rect_ebo: gl::types::GLuint,
}

impl CommonResources {
  pub unsafe fn allocate() -> Self {
    let mut rect_ebo: gl::types::GLuint = 0;
    let elements: Vec<i32> = vec![0, 1, 2, 2, 3, 0];
    gl::GenBuffers(1, &mut rect_ebo);
    gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, rect_ebo);
    gl::BufferData(
      gl::ELEMENT_ARRAY_BUFFER,
      (elements.len() * std::mem::size_of::<i32>()) as gl::types::GLsizeiptr,
      elements.as_ptr() as *const gl::types::GLvoid,
      gl::STATIC_DRAW,
    );
    Self { rect_ebo }
  }

  pub unsafe fn bind_rect_ebo(&self) -> () {
    gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.rect_ebo);
  }
}

impl Drop for CommonResources {
  fn drop(&mut self) {
    unsafe {
      gl::DeleteBuffers(1, &self.rect_ebo);
    }
  }
}
