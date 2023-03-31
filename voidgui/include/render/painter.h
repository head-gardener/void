#ifndef VOID_GUI_PAINTER
#define VOID_GUI_PAINTER

#include <GLES3/gl3.h>

/**
 * Contains information necessary for painting to OpenGL context
 */
struct painter {
  struct {
    GLuint common;
  } shaders;
};

/**
 * Allocate a painter and prepare OpenGL tools
 */
struct painter *init_painter();
/**
 * Prepare to draw a table
 */
int prepare_table(struct painter *painter);
/**
 * Draw whatever has been prepared
 */
void draw(struct painter *painter);
/**
 * Free pointer and OpenGL tools
 */
void free_painter(struct painter *painter);

#endif
