#include "shapes.h"
#include "macros.h"
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
    printf("Invalid ratio sum: %f, expected 1\n", sum);
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
 * `ratios` should have space allocated for 4 elements: {a_x, a_y, b_x, b_y}.
 */
void boxes_to_ratio(struct void_box *outer, struct void_box *inner,
                    GLfloat *ratios) {
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
  ratios[1] = (a_y - outer_center_y) / outer_half_height;
  // Bottom-right
  ratios[2] = (b_x - outer_center_x) / outer_half_width;
  ratios[3] = (b_y - outer_center_y) / outer_half_height;
}

// PUBLIC

int make_rectangle(struct shaders *shaders, struct commons *common,
                   struct shape *shape, struct void_box *box,
                   struct void_box *window) {
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

int make_grid(struct shaders *shaders, struct shape *shape,
              struct void_box *box, int rows, int columns, float *row_ratio,
              float *column_ratio, struct void_box *window) {
  glBindVertexArray(shape->vao);

  int n_vertices = 4 * (rows + 1 + columns + 1);
  GLfloat *vertices = calloc(n_vertices, sizeof(GLfloat));

  // {x0 .. x<rows>, y0, .. y<columns>}
  float *coords = calloc(rows + columns + 2, sizeof(float));

  // fill coords
  boxes_to_ratio(window, box, coords);
  split4(GLfloat, a_x, a_y, b_x, b_y, coords);
  section(a_x, b_x, rows + 1, row_ratio, coords);
  section(a_y, b_y, columns + 1, column_ratio, &coords[rows + 1]);

  for (int i = 0; i < rows + 1; i++) {
    spread_line_horizontal(coords[0], coords[rows], coords[rows + 1 + i], 0,
                           &vertices[4 * i]);
  }
  for (int i = 0; i < columns + 1; i++) {
    spread_line_vertical(coords[rows + 1], coords[rows + 1 + columns], 0,
                         coords[i], &vertices[4 * (rows + 1) + 4 * i]);
  }

  glBindBuffer(GL_ARRAY_BUFFER, shape->vbo);
  glBufferData(GL_ARRAY_BUFFER, n_vertices * sizeof(GLfloat), vertices,
               GL_STATIC_DRAW);
  free(vertices);

  glVertexAttribPointer(shaders->common.posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);

  return 0;
}

int make_texture(struct shaders *shaders, struct commons *common,
                 struct shape *shape, struct void_box *box,
                 struct void_box *window) {
  glBindVertexArray(shape->vao);

  GLuint *texture_id = calloc(1, sizeof(GLuint));
  glGenTextures(1, texture_id);
  glBindTexture(GL_TEXTURE_2D, *texture_id);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  shape->params = texture_id;

  glBindBuffer(GL_ARRAY_BUFFER, shape->vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, common->rectangle_ebo);

  GLfloat coords[4];
  GLfloat vertices[] = {
      0, 0, 0.0f, 1.0f, // Bottom-left
      0, 0, 1.0f, 1.0f, // Bottom-right
      0, 0, 1.0f, 0.0f, // Top-right
      0, 0, 0.0f, 0.0f, // Top-left
  };
  boxes_to_ratio(window, box, coords);
  spread_rectangle_horizontal(slice4(coords), 2, vertices);
  printf("%f %f %f %f\n", slice4(vertices));
  printf("%f %f %f %f\n", slice4(&vertices[4]));
  printf("%f %f %f %f\n", slice4(&vertices[8]));
  printf("%f %f %f %f\n", slice4(&vertices[12]));
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(shaders->tex.posAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), 0);
  glVertexAttribPointer(shaders->tex.texAttrib, 2, GL_FLOAT, GL_FALSE,
                        4 * sizeof(GLfloat), (void *)(2 * sizeof(GLfloat)));

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
  glDeleteTextures(1, shape->params);
  free_shape(shape);
}
