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
  GLuint elements[] = {0, 1, 2, 2, 3, 0};
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(elements), elements,
               GL_STATIC_DRAW);

  glActiveTexture(GL_TEXTURE0);

  glVertexAttribPointer(painter->shaders.tex.posAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), 0);
  glVertexAttribPointer(painter->shaders.tex.texAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), (void *)(2 * sizeof(GLfloat)));

  glEnableVertexAttribArray(painter->shaders.tex.posAttrib);
  glEnableVertexAttribArray(painter->shaders.tex.texAttrib);

  glUseProgram(painter->shaders.tex.prog);
  return 0;
}

int draw_text(struct painter *painter) {
  cairo_t *layout_context;
  cairo_t *render_context;
  cairo_surface_t *surface;
  unsigned char *surface_data = NULL;
  PangoFontDescription *desc;
  PangoLayout *layout;

  int text_width = 100;
  int text_height = 100;

  surface =
      cairo_image_surface_create(CAIRO_FORMAT_ARGB32, text_width, text_height);
  layout_context = cairo_create(surface);

  /* Create a PangoLayout, set the font and text */
  layout = pango_cairo_create_layout(layout_context);
  pango_layout_set_text(layout, "hello :)", -1);

  /* Load the font */
  desc = pango_font_description_from_string("Sans Bold 7");
  pango_layout_set_font_description(layout, desc);
  pango_font_description_free(desc);

  /* Get text dimensions and create a context to render to */
  pango_layout_get_size(layout, &text_width, &text_height);
  surface_data = calloc(4 * text_width * text_height, sizeof(unsigned char));
  render_context = cairo_create(surface);

  /* Render */
  cairo_set_source_rgba(render_context, 1, 1, 1, 1);
  pango_cairo_show_layout(render_context, layout);
  int status = cairo_surface_write_to_png(surface, "/home/mkultra/img.png");
  if (status != CAIRO_STATUS_SUCCESS) {
    g_printerr("Could not save png %i'\n", status);
    return 1;
  }

  /* FILE *f; */
  /* f = fopen("/home/mkultra/stuff", "w"); */
  /* fwrite(surface_data, sizeof(unsigned char), 4*text_height*text_width, f);
   */
  /* fclose(f); */

  GLuint texture_id;
  glGenTextures(1, &texture_id);
  glBindTexture(GL_TEXTURE_2D, texture_id);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, text_width, text_height, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, surface_data);

  GLfloat vertices[] = {
      -0.5f, 0.5f,  0.0f, 0.0f, // Top-left
      0.5f,  0.5f,  1.0f, 0.0f, // Top-right
      0.5f,  -0.5f, 1.0f, 1.0f, // Bottom-right
      -0.5f, -0.5f, 0.0f, 1.0f  // Bottom-left
  };
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, painter->shaders.tex.vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, painter->shaders.tex.ebo);

  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
  glDeleteTextures(1, &texture_id);

  /* Clean up */
  free(surface_data);
  g_object_unref(layout);
  cairo_destroy(layout_context);
  cairo_destroy(render_context);
  cairo_surface_destroy(surface);

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

  glBindBuffer(GL_ARRAY_BUFFER, painter->shaders.common.vbo);

  return 0;
}

int draw_grid(struct painter *painter, shape_ptr shape_ptr, float color[4]) {
  /* int n_vertices = 4 * (rows + 1 + columns + 1); */
  /* GLfloat *vertices = calloc(n_vertices, sizeof(GLfloat)); */

  /* // {x0 .. x<rows>, y0, .. y<columns>} */
  /* float *coords = calloc(rows + columns + 2, sizeof(float)); */

  /* // fill coords */
  /* boxes_to_ratio(&painter->window_box, box, coords); */
  /* split4(GLfloat, a_x, a_y, b_x, b_y, coords); */
  /* section(a_x, b_x, rows + 1, row_ratio, coords); */
  /* section(a_y, b_y, columns + 1, column_ratio, &coords[rows + 1]); */

  /* for (int i = 0; i < rows + 1; i++) { */
  /*   spread_line_horizontal(coords[0], coords[rows], coords[rows + 1 + i], */
  /*                          &vertices[4 * i]); */
  /* } */
  /* for (int i = 0; i < columns + 1; i++) { */
  /*   spread_line_vertical(coords[rows + 1], coords[rows + 1 + columns], */
  /*                        coords[i], &vertices[4 * (rows + 1) + 4 * i]); */
  /* } */

  /* glBufferData(GL_ARRAY_BUFFER, n_vertices * sizeof(GLfloat), vertices, */
  /*              GL_STATIC_DRAW); */
  /* free(vertices); */

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

  glDrawArrays(GL_LINES, 0, 16);

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
