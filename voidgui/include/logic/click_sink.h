#ifndef VOID_GUI_CLICK_SINK
#define VOID_GUI_CLICK_SINK

#include "array.h"
#include "shapes.h"

struct funnel_opts {
  struct painter *painter;
  void *closure;
};

struct funnel {
  struct box box;
  void *closure;
  void (*callback)(struct funnel_opts *opts);
};

struct click_sink {
  array(struct funnel, funnels);

  int size, capacity;
};

int catch_click(void *painter, struct click_sink *sink, int x,
                int y);

int register_click_funnel(struct click_sink *sink, struct funnel *funnel);

int init_click_sink(struct click_sink *sink);
void free_click_sink(struct click_sink *sink);

#endif
