#include "sink.h"
#include "state.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>

void snk_free_wrapper(void *closure, void *obj) {
  struct funnel *funnel = obj;
  free(funnel->specs);
  if (funnel->free_closure)
    free(funnel->closure);
  free(funnel);
}

int catch (struct state *state, struct sink *sink,
           void *attribs) {
  foreach_node(sink->funnels.head, {
    struct funnel *funnel = node->obj;
    funnel_callback callback = sink->check_funnel(funnel, attribs);
    if (callback) {
      callback(state, funnel->closure);
      return 0;
    }
  });

  return 1;
}

int register_funnel(struct sink *sink, int height, int mark,
                    struct funnel *funnel) {
  struct node *node = calloc(1, sizeof(struct node));
  if (!node)
    return 2;

  node->obj = funnel;
  node->free = &snk_free_wrapper;
  node->height = height;
  node->mark = mark;

  sink->funnels.head = emplace_node(sink->funnels.head, node);

  return 0;
}

int init_sink(struct sink *sink,
              funnel_callback (*check_funnel)(struct funnel *funnel,
                                              void *attribs)) {
  sink->check_funnel = check_funnel;

  return 0;
}

void free_sink(struct sink *sink) { free_list(&sink->funnels); }
