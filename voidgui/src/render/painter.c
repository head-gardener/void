#include "painter.h"
#include "cairo/cairo.h"
#include "common_frag_src.h"
#include "common_vert_src.h"
#include "pango/pango.h"
#include "pango/pangocairo.h"
#include "tex_frag_src.h"
#include "tex_vert_src.h"
#include "window.h"
#include <GLES3/gl3.h>
#include <SDL2/SDL.h>

#define slice3(a) a[0], a[1], a[2]
#define slice4(a) a[0], a[1], a[2], a[3]
#define slice6(a) a[0], a[1], a[2], a[3], a[4], a[5]

#define split4(type, a, b, c, d, arr)                                          \
  type a = arr[0];                                                             \
  type b = arr[1];                                                             \
  type c = arr[2];                                                             \
  type d = arr[3];

/**
 * Spreads three coords over an 4-element array to form two vertices of a
 * horizontal line.
 */
void spread_line_horizontal(float x1, float x2, float y, float *vertices) {
  vertices[0] = x1;
  vertices[1] = y;
  vertices[2] = x2;
  vertices[3] = y;
}

/**
 * Spreads three coords over an 4-element array to form two vertices of a
 * vertical line.
 */
void spread_line_vertical(float y1, float y2, float x, float *vertices) {
  vertices[0] = x;
  vertices[1] = y1;
  vertices[2] = x;
  vertices[3] = y2;
}

/**
 * Spreads two points over an 8-element array to form all vertices of a
 * rectangle. `a` it top-left, `b` it bottom-right. Vertices are ordered
 * as follows: top-left, top-right, bottom-right, bottom-left.
 */
void spread_rectangle_horizontal(float a_x, float a_y, float b_x, float b_y,
                                 float *vertices) {
  spread_line_horizontal(a_x, b_x, a_y, vertices);
  spread_line_horizontal(b_x, a_x, b_y, &vertices[4]);
}

/**
 * Spreads two points over an 8-element array to form all vertices of a
 * rectangle. `a` it top-left, `b` it bottom-right. Vertices are ordered
 * as follows: top-left, bottom-left, top-right, bottom-right.
 */
void spread_rectangle_vertical(float a_x, float a_y, float b_x, float b_y,
                               float *vertices) {
  spread_line_vertical(a_y, b_y, a_x, vertices);
  spread_line_vertical(b_y, a_y, b_x, &vertices[4]);
}

/**
 * Sections area between `a` and `b` into `n` points according to ratios.
 */
void section(float a, float b, int n, float *ratios, float *points) {
  /* #ifdef VOID_SANER */
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

static GLuint compile_shader(GLuint type, const GLchar *src) {
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, 1, &src, NULL);
  glCompileShader(shader);

  GLint ok;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &ok);
  if (ok == GL_FALSE) {
    GLint log_len;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
    char *log = calloc(log_len, sizeof(char));
    glGetShaderInfoLog(shader, log_len, NULL, log);

    printf("Failed to compile shader\n %s\n", log);
    glDeleteShader(shader);
    shader = 0;
  }

  return shader;
}

static GLuint link_program(const GLchar *vert_src, const GLchar *frag_src) {
  GLuint vert = compile_shader(GL_VERTEX_SHADER, vert_src);
  if (!vert) {
    goto error;
  }

  GLuint frag = compile_shader(GL_FRAGMENT_SHADER, frag_src);
  if (!frag) {
    glDeleteShader(vert);
    goto error;
  }

  GLuint prog = glCreateProgram();
  glAttachShader(prog, vert);
  glAttachShader(prog, frag);
  glLinkProgram(prog);

  glDetachShader(prog, vert);
  glDetachShader(prog, frag);
  glDeleteShader(vert);
  glDeleteShader(frag);

  GLint ok;
  glGetProgramiv(prog, GL_LINK_STATUS, &ok);
  if (ok == GL_FALSE) {
    ;
    printf("Failed to link shader\n");
    glDeleteProgram(prog);
    goto error;
  }

  return prog;

error:
  return 0;
}

struct painter *init_painter(int width, int height) {
  struct painter *painter = calloc(1, sizeof(struct painter));

  painter->window_box.x = 0;
  painter->window_box.y = 0;
  painter->window_box.width = width;
  painter->window_box.height = height;

  // shaders
  GLuint prog = link_program(common_vert_src, common_frag_src);
  if (!prog) {
    free(painter);
    return 0;
  }
  painter->shaders.common.prog = prog;
  painter->shaders.common.posAttrib = glGetAttribLocation(prog, "pos");
  glGenBuffers(1, &painter->shaders.common.vbo);
  glGenBuffers(1, &painter->shaders.common.ebo);

  prog = link_program(tex_vert_src, tex_frag_src);
  if (!prog) {
    free(painter);
    return 0;
  }
  painter->shaders.tex.prog = prog;
  painter->shaders.tex.texAttrib = glGetAttribLocation(prog, "texcoord");
  painter->shaders.tex.posAttrib = glGetAttribLocation(prog, "pos");
  glGenBuffers(1, &painter->shaders.tex.vbo);
  glGenBuffers(1, &painter->shaders.tex.ebo);

  float bg_color[] = {0.9f, 0.9f, 0.9f};
  memcpy(painter->opts.bg_color, bg_color, sizeof(bg_color));

  // OpenGL configs
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

  return painter;
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

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, painter->shaders.common.ebo);

  GLuint elements[] = {0, 1, 2, 2, 3, 0};
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(elements), elements,
               GL_STATIC_DRAW);

  return 0;
}

int draw_rectangle(struct painter *painter, struct void_box *box) {
  GLfloat *vertices = calloc(8, sizeof(GLfloat));
  boxes_to_ratio(&painter->window_box, box, vertices);
  spread_rectangle_horizontal(slice4(vertices), vertices);

  glBindBuffer(GL_ARRAY_BUFFER, painter->shaders.common.vbo);
  glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(GLfloat), vertices, GL_STATIC_DRAW);
  free(vertices);

  glEnableVertexAttribArray(painter->shaders.common.posAttrib);
  glVertexAttribPointer(painter->shaders.common.posAttrib, 2, GL_FLOAT,
                        GL_FALSE, 0, 0);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

  return 0;
}

int prepare_grid(struct painter *painter) {
  glUseProgram(painter->shaders.common.prog);

  glBindBuffer(GL_ARRAY_BUFFER, painter->shaders.common.vbo);

  return 0;
}

int draw_grid(struct painter *painter, struct void_box *box, int rows,
              int columns, float *row_ratio, float *column_ratio) {
  int n_vertices = 4 * (rows + 1 + columns + 1);
  GLfloat *vertices = calloc(n_vertices, sizeof(GLfloat));

  // {x0 .. x<rows>, y0, .. y<columns>}
  float *coords = calloc(rows + columns + 2, sizeof(float));

  // fill coords
  boxes_to_ratio(&painter->window_box, box, coords);
  split4(GLfloat, a_x, a_y, b_x, b_y, coords);
  section(a_x, b_x, rows + 1, row_ratio, coords);
  section(a_y, b_y, columns + 1, column_ratio, &coords[rows + 1]);

  for (int i = 0; i < rows + 1; i++) {
    spread_line_horizontal(coords[0], coords[rows], coords[rows + 1 + i],
                           &vertices[4 * i]);
  }
  for (int i = 0; i < columns + 1; i++) {
    spread_line_vertical(coords[rows + 1], coords[rows + 1 + columns],
                         coords[i], &vertices[4 * (rows + 1) + 4 * i]);
  }

  glBufferData(GL_ARRAY_BUFFER, n_vertices * sizeof(GLfloat), vertices,
               GL_STATIC_DRAW);
  free(vertices);

  glVertexAttribPointer(painter->shaders.common.posAttrib, 2, GL_FLOAT,
                        GL_FALSE, 0, 0);
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
