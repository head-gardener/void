#ifndef VOID_GUI_CLICK_SINK
#define VOID_GUI_CLICK_SINK

#include "array.h"
#include "shapes.h"

struct funnel_opts {
  struct painter *painter;
};

struct click_sink {
  array(struct box, areas);
  void (**funnels)(struct funnel_opts *opts);

  int size, capacity;
};

int catch_click(void *painter, struct click_sink *sink, int x, int y);

int register_click_funnel(struct click_sink *sink, struct box *box,
                          void (*on_click)(struct funnel_opts *opts));

int init_click_sink(struct click_sink *sink);
void free_click_sink(struct click_sink *sink);

#endif
