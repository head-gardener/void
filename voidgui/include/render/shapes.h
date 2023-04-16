#ifndef VOID_GUI_SHAPES
#define VOID_GUI_SHAPES

#include "commons.h"
#include "shaders.h"
#include <GLES3/gl3.h>
#include <wchar.h>

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

int plot_rectangle(struct shaders *shaders, struct commons *common,
                   struct shape *shape, struct box *box, struct box *window);
int plot_grid(struct shaders *shaders, struct shape *shape, struct box *box,
              int rows, int columns, float *row_ratio, float *column_ratio,
              struct box *window);
int plot_texture(struct shaders *shaders, struct commons *common,
                 struct shape *shape, struct box *box, struct box *window);

int sync_texture(struct shape *shape, int width, int height,
                   unsigned char *surface_data);

int read_texture(const char *png_path, int *width, int *height,
                 unsigned char **surface_data);
int render_text(const wchar_t *text, int *width, int *height,
                unsigned char **surface_data);
int get_text_size(const wchar_t *text, int *width, int *height);

void init_shape(struct shape *);
void free_shape(struct shape *);

void free_texture_shape(struct shape *);

#endif
