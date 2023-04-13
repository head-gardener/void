#ifndef VOID_GUI_CLICK_SINK
#define VOID_GUI_CLICK_SINK

#include "array.h"
#include "draw_queue.h"
#include "shapes.h"
#include "store.h"
#include <stdbool.h>

struct funnel_opts {
  struct painter *painter;
  struct ui_node **queue;
  struct store *store;
  void *closure;
};

struct funnel {
  struct box box;
  bool inverted;
  void *closure;
  void (*callback)(struct funnel_opts *opts);
};

struct click_sink {
  // TODO: make this a list idk
  array(struct funnel, funnels);

  int size, capacity;
  int ca_size, ca_capacity;
};

int catch_click(struct painter *painter, struct ui_node **queue,
                struct store *store, struct click_sink *sink, int x, int y);

/**
 * Remember area of effect and closure to call whenever a click is caught
 * in the area.
 */
int register_click_funnel(struct click_sink *sink, struct funnel *funnel);

int init_click_sink(struct click_sink *sink);
void free_click_sink(struct click_sink *sink);

#endif
