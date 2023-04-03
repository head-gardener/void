#ifndef VOID_GUI_PAINTER
#define VOID_GUI_PAINTER

#include <GLES3/gl3.h>

struct void_box {
  int x;
  int y;
  int width;
  int height;
};

/**
 * Contains information necessary for painting to OpenGL context
 */
struct painter {
  struct {
    struct {
      GLuint prog;
      GLint color;
      GLuint vbo;
      GLuint ebo;
      GLint posAttrib;
    } common;
    struct {
      GLuint prog;
      GLuint vbo;
      GLuint ebo;
      GLint posAttrib;
      GLint texAttrib;
    } tex;
  } shaders;

  struct void_box window_box;

  struct {
    float bg_color[3];
  } opts;
};

/**
 * Allocate a painter and prepare OpenGL tools
 */
struct painter *init_painter(int width, int height);

int prepare_rectangle(struct painter *painter);
int draw_rectangle(struct painter *painter, struct void_box *box,
                   float color[4]);

int prepare_grid(struct painter *painter);
int draw_grid(struct painter *painter, struct void_box *box, int rows,
              int columns, float *row_ratio, float *column_ratio,
              float color[4]);

int prepare_text(struct painter *painter);
int draw_text(struct painter *painter);

void clear(struct painter *painter);
/**
 * Free pointer and OpenGL tools
 */
void free_painter(struct painter *painter);

#endif
