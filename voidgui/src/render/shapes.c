#include "shapes.h"
#include "macros.h"
#include <cairo/cairo.h>
#include <pango/pangocairo.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Spreads three coords over an `4 + pad` element array to form two vertices of
 * a horizontal line, offsetting each vertice by `pad`.
 */
void spread_line_horizontal(float x1, float x2, float y, int pad,
                            float *vertices) {
  vertices[0] = x1;
  vertices[1] = y;
  vertices[2 + pad] = x2;
  vertices[3 + pad] = y;
}

/**
 * Spreads three coords over an `4 + pad` element array to form two vertices of
 * a vertical line, offsetting each vertice by `pad`.
 */
void spread_line_vertical(float y1, float y2, float x, int pad,
                          float *vertices) {
  vertices[0] = x;
  vertices[1] = y1;
  vertices[2 + pad] = x;
  vertices[3 + pad] = y2;
}

/**
 * Spreads two points over an `8 + 3pad` element array to form all vertices of
 * a rectangle, offsetting each vertice by `pad`. `a` it top-left, `b` it
 * bottom-right. Vertices are ordered as follows: top-left, top-right,
 * bottom-right, bottom-left.
 */
void spread_rectangle_horizontal(float a_x, float a_y, float b_x, float b_y,
                                 int pad, float *vertices) {
  spread_line_horizontal(a_x, b_x, a_y, pad, vertices);
  spread_line_horizontal(b_x, a_x, b_y, pad, &vertices[4 + 2 * pad]);
}

/**
 * Spreads two points over an `8 + 3pad` element array to form all vertices of
 * a rectangle, offsetting each vertice by `pad`. `a` it top-left, `b` it
 * bottom-right. Vertices are ordered as follows: top-left, bottom-left,
 * top-right, bottom-right.
 */
void spread_rectangle_vertical(float a_x, float a_y, float b_x, float b_y,
                               int pad, float *vertices) {
  spread_line_vertical(a_y, b_y, a_x, pad, vertices);
  spread_line_vertical(b_y, a_y, b_x, pad, &vertices[4 + 2 * pad]);
}

/**
 * Sections area between `a` and `b` into `n` points according to ratios.
 */
void section(float a, float b, int n, float *ratios, float *points) {
  /* #ifdef VOID_GUI_SANER */
  float sum = 0;
  for (int i = 0; i < n - 1; i++) {
    sum += ratios[i];
  }
  if (sum != 1.0f) {
    printf("Invalid ratio sum: %f, expected 1\nRatios: ", sum);
    for (int i = 0; i < n - 1; i++) {
      printf("%f ", ratios[i]);
    }
    printf("\n");
  }
  /* #endif */

  float dist = b - a;
  points[0] = a;

  for (int i = 1; i < n; i++) {
    points[i] = points[i - 1] + dist * ratios[i - 1];
  }
}

/**
 * Convert two boxes to their relative ratio for OpenGL rendering.
 * `ratios` should have space allocated for 4 elements: {a_x, a_y, b_x, b_y},
 * where `a` is top-left and `b` is bottom-right.
 */
void boxes_to_ratio(struct box *outer, struct box *inner, GLfloat *ratios) {
  float outer_half_width = (float)outer->width / 2;
  float outer_half_height = (float)outer->height / 2;
  float outer_center_x = outer->x + outer_half_width;
  float outer_center_y = outer->y + outer_half_height;

  // Top-left
  int a_x = inner->x;
  int a_y = inner->y;
  // Bottom-right
  int b_x = inner->x + inner->width;
  int b_y = inner->y + inner->height;

  // Top-left
  ratios[0] = (a_x - outer_center_x) / outer_half_width;
  ratios[1] = -(a_y - outer_center_y) / outer_half_height;
  // Bottom-right
  ratios[2] = (b_x - outer_center_x) / outer_half_width;
  ratios[3] = -(b_y - outer_center_y) / outer_half_height;
}

// PUBLIC

int plot_rectangle(struct shaders *shaders, struct commons *common,
                   struct shape *shape, struct box *box, struct box *window) {
  glBindVertexArray(shape->vao);

  GLfloat vertices[8];
  boxes_to_ratio(window, box, vertices);
  spread_rectangle_horizontal(slice4(vertices), 0, vertices);

  glBindBuffer(GL_ARRAY_BUFFER, shape->vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, common->rectangle_ebo);

  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(shaders->common.posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);

  return 0;
}

int plot_grid(struct shaders *shaders, struct shape *shape, struct box *box,
              int rows, int columns, float *row_ratio, float *column_ratio,
              struct box *window) {
  glBindVertexArray(shape->vao);

  int n_vertices = 4 * (rows + 1 + columns + 1);
  GLfloat *vertices = calloc(n_vertices, sizeof(GLfloat));

  // {x0 .. x<rows>, y0, .. y<columns>}
  float *coords = calloc(rows + columns + 2, sizeof(float));

  // fill coords
  boxes_to_ratio(window, box, coords);
  split4(GLfloat, a_x, a_y, b_x, b_y, coords);
  section(a_y, b_y, rows + 1, row_ratio, coords);
  section(a_x, b_x, columns + 1, column_ratio, &coords[rows + 1]);

  for (int i = 0; i < rows + 1; i++) {
    spread_line_horizontal(coords[rows + 1], coords[rows + 1 + columns],
                           coords[i], 0, &vertices[4 * i]);
  }
  for (int i = 0; i < columns + 1; i++) {
    spread_line_vertical(coords[0], coords[rows], coords[rows + 1 + i], 0,
                         &vertices[4 * (rows + 1) + 4 * i]);
  }

  glBindBuffer(GL_ARRAY_BUFFER, shape->vbo);
  glBufferData(GL_ARRAY_BUFFER, n_vertices * sizeof(GLfloat), vertices,
               GL_STATIC_DRAW);
  free(vertices);

  glVertexAttribPointer(shaders->common.posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);

  return 0;
}

int plot_texture(struct shaders *shaders, struct commons *common,
                 struct shape *shape, struct box *box, struct box *window) {
  glBindVertexArray(shape->vao);

  glBindBuffer(GL_ARRAY_BUFFER, shape->vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, common->rectangle_ebo);

  GLfloat coords[4];
  GLfloat vertices[] = {
      0, 0, 0.0f, 0.0f, // Top-left
      0, 0, 1.0f, 0.0f, // Top-right
      0, 0, 1.0f, 1.0f, // Bottom-right
      0, 0, 0.0f, 1.0f, // Bottom-left
  };
  boxes_to_ratio(window, box, coords);
  spread_rectangle_horizontal(slice4(coords), 2, vertices);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(shaders->tex.posAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), 0);
  glVertexAttribPointer(shaders->tex.texAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), (void *)(2 * sizeof(GLfloat)));

  return 0;
}

int sync_texture(struct shape *shape, int width, int height,
                 unsigned char *surface_data) {
  glBindVertexArray(shape->vao);

  GLuint *texture_id = calloc(1, sizeof(GLuint));
  glGenTextures(1, texture_id);
  glBindTexture(GL_TEXTURE_2D, *texture_id);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  shape->params = texture_id;

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, surface_data);
  print_gl_error;

  return 0;
}

int read_texture(const char *png_path, int *width, int *height,
                 unsigned char **surface_data) {
  cairo_surface_t *surface = cairo_image_surface_create_from_png(png_path);
  // TODO: error handling?
  *width = cairo_image_surface_get_width(surface);
  *height = cairo_image_surface_get_height(surface);
  *surface_data = cairo_image_surface_get_data(surface);
  cairo_surface_destroy(surface);

  return 0;
}

int render_text(const char *text, int *width, int *height,
                unsigned char **surface_data) {
  cairo_t *layout_context;
  cairo_t *render_context;
  cairo_surface_t *tmp_surface;
  cairo_surface_t *out_surface;
  PangoFontDescription *desc;
  PangoLayout *layout;

  tmp_surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 0, 0);
  layout_context = cairo_create(tmp_surface);

  /* Create a PangoLayout, set the font and text */
  layout = pango_cairo_create_layout(layout_context);
  pango_layout_set_text(layout, text, -1);

  /* Load the font */
  desc = pango_font_description_from_string("Sans Bold 17");
  pango_layout_set_font_description(layout, desc);
  pango_font_description_free(desc);

  /* Get text dimensions and create a context to render to */
  pango_layout_get_pixel_size(layout, width, height);
  *surface_data = calloc(4 * *width * *height, sizeof(unsigned char));
  out_surface = cairo_image_surface_create_for_data(
      *surface_data, CAIRO_FORMAT_ARGB32, *width, *height, 4 * *width);
  render_context = cairo_create(out_surface);

  /* Render */
  cairo_set_source_rgba(render_context, .3, .3, .3, 1);
  pango_cairo_show_layout(render_context, layout);

  /* Clean up */
  g_object_unref(layout);
  cairo_destroy(layout_context);
  cairo_destroy(render_context);
  cairo_surface_destroy(tmp_surface);
  cairo_surface_destroy(out_surface);

  return 0;
}

void init_shape(struct shape *shape) {
  glGenVertexArrays(1, &shape->vao);
  glGenBuffers(1, &shape->vbo);
}

void free_shape(struct shape *shape) {
  glDeleteVertexArrays(1, &shape->vao);
  glDeleteBuffers(1, &shape->vbo);

  shape->vao = shape->vbo = 0;
  shape->params = 0;
}

void free_texture_shape(struct shape *shape) {
  if (shape->params) {
    glDeleteTextures(1, shape->params);
    free(shape->params);
  }
  free_shape(shape);
}
