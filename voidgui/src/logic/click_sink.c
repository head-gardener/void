#include "click_sink.h"
#include "macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int catch_click(void *painter, struct click_sink *sink, int x, int y) {
  struct funnel_opts opts = {painter};

  for (int i = 0; i < sink->size; i++) {
    int dist_x = x - sink->areas[i].x;
    int dist_y = y - sink->areas[i].y;
    if (dist_x >= 0 && dist_x <= sink->areas[i].width && dist_y >= 0 &&
        dist_y <= sink->areas[i].height) {
      sink->funnels[i](&opts);
    }
  }

  return 0;
}

int register_click_funnel(struct click_sink *sink, struct box *box,
                          void (*on_click)(struct funnel_opts *opts)) {
  if (sink->size >= sink->capacity) {
    array_expand(struct box, sink->areas, sink->capacity, 2, goto failed);
    sink->capacity *= 2;
  }

  memcpy(&sink->areas[sink->size], box, sizeof(struct box));
  sink->funnels[sink->size] = on_click;
  sink->size += 1;

  return 0;

failed:
  return 2;
}

int init_click_sink(struct click_sink *sink) {
  fail_condition(!(init_array(struct box, sink->areas, 10)));
  fail_condition(!(sink->funnels = calloc(10, sizeof(void *))));

  sink->size = 0;
  sink->capacity = 10;

  return 0;

failed:
  if (sink->areas)
    free(sink->areas);
  if (sink->funnels)
    free(sink->funnels);

  return 2;
}

void free_click_sink(struct click_sink *sink) {
  free(sink->areas);
  free(sink->funnels);
}
