#include "sink.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>

int catch (struct painter *painter, struct ui_node **queue, struct store *store,
           struct sink *sink, void *attribs) {
  for (int i = 0; i < sink->size; i++) {
    if (sink->check_funnel(&sink->funnels[i], attribs)) {
      struct funnel_opts opts = {painter, queue, store,
                                 sink->funnels[i].closure};
      sink->funnels[i].callback(&opts);
      return 0;
    }
  }

  return 1;
}

int register_funnel(struct sink *sink, struct funnel *funnel) {
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

int init_sink(struct sink *sink,
              bool (*check_funnel)(struct funnel *funnel, void *attribs)) {

  failure_condition(!(init_array(struct funnel, sink->funnels, 10)));

  sink->size = 0;
  sink->capacity = 10;
  sink->check_funnel = check_funnel;

  return 0;

failed:
  if (sink->funnels)
    free(sink->funnels);

  return 2;
}

void free_sink(struct sink *sink) {
  for (int i = 0; i < sink->size; i++) {
    free(sink->funnels[i].specs);
  }

  free(sink->funnels);
}
