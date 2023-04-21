#include "painter.h"
#include "macros.h"
#include "window.h"
#include <GLES3/gl3.h>
#include <SDL2/SDL.h>
#include <cairo/cairo.h>
#include <pango/pangocairo.h>

int init_painter(int width, int height, struct painter *painter) {
  painter->window_box.x = 0;
  painter->window_box.y = 0;
  painter->window_box.width = width;
  painter->window_box.height = height;

  failure_condition(init_shaders(&painter->shaders));
  failure_condition(init_shape_buffer(&painter->shape_buffer));

  glGenBuffers(1, &painter->common.rectangle_ebo);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, painter->common.rectangle_ebo);
  GLuint elements[] = {0, 1, 2, 2, 3, 0};
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(elements), elements,
               GL_STATIC_DRAW);

  float bg_color[] = {0.9f, 0.9f, 0.9f};
  memcpy(painter->opts.bg_color, bg_color, sizeof(bg_color));

  // OpenGL configs
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

  return 0;

failed:
  return 1;
}

int prepare_rectangle(struct painter *painter) {
  glUseProgram(painter->shaders.common.prog);

  return 0;
}

int draw_rectangle(struct painter *painter, shape_ptr shape_ptr,
                   float color[4]) {
  glUniform4f(painter->shaders.common.color, slice4(color));

  struct shape shape = painter->shape_buffer.shapes[shape_ptr];

  /* #ifdef VOID_GUI_SANER */
  if (!shape.vao || !shape.vbo) {
    printf("Invalid shape passed to draw_rectangle: vao %u vbo %u\n", shape.vao,
           shape.vbo);
  }
  /* #endif */

  glBindVertexArray(shape.vao);
  glEnableVertexAttribArray(painter->shaders.common.posAttrib);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  return 0;
}

int prepare_grid(struct painter *painter) {
  glUseProgram(painter->shaders.common.prog);

  return 0;
}

int draw_grid(struct painter *painter, shape_ptr shape_ptr, float color[4]) {
  glUniform4f(painter->shaders.common.color, slice4(color));

  struct shape shape = painter->shape_buffer.shapes[shape_ptr];

  /* #ifdef VOID_GUI_SANER */
  if (!shape.vao || !shape.vbo) {
    printf("Invalid shape passed to draw_grid: vao %u vbo %u\n", shape.vao,
           shape.vbo);
  }
  /* #endif */

  glBindVertexArray(shape.vao);
  glEnableVertexAttribArray(painter->shaders.common.posAttrib);

  // FIXME: 16?
  glDrawArrays(GL_LINES, 0, 16);

  return 0;
}

int prepare_texture(struct painter *painter) {
  glUseProgram(painter->shaders.tex.prog);

  return 0;
}

int draw_texture(struct painter *painter, shape_ptr shape_ptr) {
  struct shape shape = painter->shape_buffer.shapes[shape_ptr];

  /* #ifdef VOID_GUI_SANER */
  if (!shape.vao || !shape.vbo || !shape.params) {
    printf("Invalid shape passed to draw_texture: vao %u vbo %u tex %p\n",
           shape.vao, shape.vbo, shape.params);
  }
  /* #endif */

  glBindVertexArray(shape.vao);
  glBindTexture(GL_TEXTURE_2D, *(GLuint *)shape.params);
  glEnableVertexAttribArray(painter->shaders.tex.posAttrib);
  glEnableVertexAttribArray(painter->shaders.tex.texAttrib);

  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  return 0;
}

void clear(struct painter *painter) {
  glClearColor(slice3(painter->opts.bg_color), 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
}

void free_painter(struct painter *painter) {
  // TODO: more!!
  glDeleteProgram(painter->shaders.common.prog);
  glDeleteProgram(painter->shaders.tex.prog);
}
