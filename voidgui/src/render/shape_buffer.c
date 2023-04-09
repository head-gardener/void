#include "shape_buffer.h"
#include "macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int init_shape_buffer(struct shape_buffer *buffer) {
  buffer->shapes = calloc(SHAPE_BUFFER_INNITIAL_CAPACITY, sizeof(struct shape));
  failure_condition(!buffer->shapes);
  buffer->capacity = SHAPE_BUFFER_INNITIAL_CAPACITY;

  return 0;

failed:
  if (buffer) {
    if (buffer->shapes)
      free(buffer->shapes);

    free(buffer);
  }
  return 2;
}

int get_new_shape(struct shape_buffer *buffer, unsigned int *ind) {
  unsigned int i = 0;

  for (i = 0; i < buffer->capacity; i++)
    if (!buffer->shapes[i].vao)
      goto succeeded;

  array_expand(struct shape, buffer->shapes, buffer->capacity, 2, goto failed);
  buffer->capacity *= 2;
  i = buffer->capacity - 1;

succeeded:
  init_shape(&buffer->shapes[i]);
  *ind = i;

  return 0;

failed:
  return 2;
}
