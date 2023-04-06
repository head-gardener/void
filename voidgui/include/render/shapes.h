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

struct void_box {
  int x;
  int y;
  int width;
  int height;
};

int make_rectangle(struct shaders *shaders, struct commons *common,
                   struct shape *shape, struct void_box *box,
                   struct void_box *window);
int make_grid(struct shaders *shaders, struct shape *shape,
              struct void_box *box, int rows, int columns, float *row_ratio,
              float *column_ratio, struct void_box *window);
int make_texture(struct shaders *shaders, struct commons *common,
                 struct shape *shape, struct void_box *box,
                 struct void_box *window);

void init_shape(struct shape *);
void free_shape(struct shape *);

void free_texture_shape(struct shape *);

#endif
