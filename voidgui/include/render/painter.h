#ifndef VOID_GUI_PAINTER
#define VOID_GUI_PAINTER

#include "commons.h"
#include "shaders.h"
#include "shape_buffer.h"
#include "shapes.h"

/**
 * Contains information necessary for painting to OpenGL context
 */
struct painter {
  struct shaders shaders;
  struct shape_buffer shape_buffer;

  struct box window_box;

  struct {
    float bg_color[3];
  } opts;

  struct commons common;
};

/**
 * Allocate a painter and prepare OpenGL tools
 */
int init_painter(int width, int height, struct painter *painter);

int prepare_rectangle(struct painter *painter);
int draw_rectangle(struct painter *painter, shape_ptr shape, float color[4]);

int prepare_grid(struct painter *painter);
int draw_grid(struct painter *painter, shape_ptr shape, float color[4]);

int prepare_texture(struct painter *painter);
int draw_texture(struct painter *painter, shape_ptr shape);

void clear(struct painter *painter);
/**
 * Free pointer and OpenGL tools
 */
void free_painter(struct painter *painter);

#endif
