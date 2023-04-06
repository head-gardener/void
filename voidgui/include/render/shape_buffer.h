#ifndef VOID_GUI_SHAPE_BUFFER
#define VOID_GUI_SHAPE_BUFFER

#include "shapes.h"
#include <GLES3/gl3.h>

#ifndef SHAPE_BUFFER_INNITIAL_CAPACITY
#define SHAPE_BUFFER_INNITIAL_CAPACITY 20
#endif

typedef unsigned shape_ptr;

struct shape_buffer {
  struct shape *shapes;
  unsigned int capacity;
};

int init_shape_buffer(struct shape_buffer *buffer);
int get_new_shape(struct shape_buffer *buffer, unsigned int *ind);
// don't forget to free the shape once you are done

#endif
