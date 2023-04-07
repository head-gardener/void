#ifndef VOID_GUI_SHAPES
#define VOID_GUI_SHAPES

#include "commons.h"
#include "shaders.h"
#include <GLES3/gl3.h>

struct shape {
  GLuint vao;
  GLuint vbo;
  void *params;
};

struct box {
  int x;
  int y;
  int width;
  int height;
};

struct point {
  int x;
  int y;
};

struct size {
  int width;
  int height;
};

int make_rectangle(struct shaders *shaders, struct commons *common,
                   struct shape *shape, struct box *box,
                   struct box *window);
int make_grid(struct shaders *shaders, struct shape *shape,
              struct box *box, int rows, int columns, float *row_ratio,
              float *column_ratio, struct box *window);
int make_texture(struct shaders *shaders, struct commons *common,
                 struct shape *shape, struct box *box,
                 struct box *window);

int render_texture(struct shape *shape, const char *path);
int render_text(struct shape *shape, const char *text);

void init_shape(struct shape *);
void free_shape(struct shape *);

void free_texture_shape(struct shape *);

#endif
