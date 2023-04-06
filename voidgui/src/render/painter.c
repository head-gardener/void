#include "painter.h"
#include "macros.h"
#include "window.h"
#include <GLES3/gl3.h>
#include <SDL2/SDL.h>
#include <cairo/cairo.h>
#include <pango/pangocairo.h>

struct painter *init_painter(int width, int height) {
  struct painter *painter = calloc(1, sizeof(struct painter));

  painter->window_box.x = 0;
  painter->window_box.y = 0;
  painter->window_box.width = width;
  painter->window_box.height = height;

  fail_condition(init_shaders(&painter->shaders));
  fail_condition(init_shape_buffer(&painter->shape_buffer));

  glGenBuffers(1, &painter->shaders.common.vbo);
  glGenBuffers(1, &painter->shaders.common.ebo);
  glGenBuffers(1, &painter->shaders.tex.vbo);
  glGenBuffers(1, &painter->shaders.tex.ebo);
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

  return painter;

failed:
  free(painter);
  return 0;
}

int prepare_text(struct painter *painter) {
  glActiveTexture(GL_TEXTURE0);
  glUseProgram(painter->shaders.tex.prog);

  return 0;
}

int draw_text(struct painter *painter, shape_ptr shape_ptr) {
  struct shape shape = painter->shape_buffer.shapes[shape_ptr];

  /* #ifdef VOID_GUI_SANER */
  if (!shape.vao || !shape.vbo || !shape.params) {
    printf("Invalid shape passed to draw_text: vao %u vbo %u tex %p", shape.vao,
           shape.vbo, shape.params);
  }
  /* #endif */

  glBindVertexArray(shape.vao);
  glEnableVertexAttribArray(painter->shaders.tex.posAttrib);
  glEnableVertexAttribArray(painter->shaders.tex.texAttrib);

  cairo_t *layout_context;
  cairo_t *render_context;
  cairo_surface_t *tmp_surface;
  cairo_surface_t *out_surface;
  unsigned char *surface_data = NULL;
  PangoFontDescription *desc;
  PangoLayout *layout;

  tmp_surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 0, 0);
  layout_context = cairo_create(tmp_surface);

  /* Create a PangoLayout, set the font and text */
  layout = pango_cairo_create_layout(layout_context);
  pango_layout_set_text(layout, "hello :)", -1);

  /* Load the font */
  desc = pango_font_description_from_string("Sans Bold 17");
  pango_layout_set_font_description(layout, desc);
  pango_font_description_free(desc);

  /* Get text dimensions and create a context to render to */
  int text_width = 100;
  int text_height = 100;
  pango_layout_get_pixel_size(layout, &text_width, &text_height);
  surface_data = calloc(4 * text_width * text_height, sizeof(unsigned char));
  out_surface = cairo_image_surface_create_for_data(
      surface_data, CAIRO_FORMAT_ARGB32, text_width, text_height,
      4 * text_width);
  render_context = cairo_create(out_surface);

  /* Render */
  cairo_set_source_rgba(render_context, 1, 1, 1, 1);
  pango_cairo_show_layout(render_context, layout);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, text_width, text_height, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, surface_data);
  print_gl_error;

  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  /* Clean up */
  free(surface_data);
  g_object_unref(layout);
  cairo_destroy(layout_context);
  cairo_destroy(render_context);
  cairo_surface_destroy(tmp_surface);
  cairo_surface_destroy(out_surface);

  return 0;
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
    printf("Invalid shape passed to draw_rectangle: vao %u vbo %u", shape.vao,
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
    printf("Invalid shape passed to draw_grid: vao %u vbo %u", shape.vao,
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
    printf("Invalid shape passed to draw_texture: vao %u vbo %u tex %p", shape.vao,
           shape.vbo, shape.params);
  }
  /* #endif */

  glBindVertexArray(shape.vao);
  glEnableVertexAttribArray(painter->shaders.tex.posAttrib);
  glEnableVertexAttribArray(painter->shaders.tex.texAttrib);

  cairo_surface_t *surface = cairo_image_surface_create_from_png(
      "/home/mkultra/Code/void/voidgui/res/dog.png");
  int tex_w = cairo_image_surface_get_width(surface);
  int tex_h = cairo_image_surface_get_height(surface);
  unsigned char *data = cairo_image_surface_get_data(surface);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_w, tex_h, 0, GL_RGBA, GL_UNSIGNED_BYTE,
               data);
  print_gl_error;
  cairo_surface_destroy(surface);

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
  free(painter);
}
