#include "click_sink.h"
#include "macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int catch_click(void *painter, struct click_sink *sink, int x, int y) {
  for (int i = 0; i < sink->size; i++) {
    int dist_x = x - sink->funnels[i].box.x;
    int dist_y = y - sink->funnels[i].box.y;
    if (dist_x >= 0 && dist_x <= sink->funnels[i].box.width && dist_y >= 0 &&
        dist_y <= sink->funnels[i].box.height) {
      struct funnel_opts opts = {painter, sink->funnels[i].closure};

      sink->funnels[i].callback(&opts);
    }
  }

  return 0;
}

int register_click_funnel(struct click_sink *sink, struct funnel *funnel) {
  if (sink->size >= sink->capacity) {
    array_expand(struct funnel, sink->funnels, sink->capacity, 2, goto failed);
    sink->capacity *= 2;
  }

  memcpy(&sink->funnels[sink->size], funnel, sizeof(struct funnel));
  sink->size += 1;

  return 0;

failed:
  return 2;
}

int init_click_sink(struct click_sink *sink) {
  failure_condition(!(init_array(struct funnel, sink->funnels, 10)));

  sink->size = 0;
  sink->capacity = 10;

  return 0;

failed:
  if (sink->funnels)
    free(sink->funnels);

  return 2;
}

void free_click_sink(struct click_sink *sink) {
  free(sink->funnels);
}
