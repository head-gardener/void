#ifndef VOID_GUI_SINK
#define VOID_GUI_SINK

#include "array.h"
#include "draw_queue.h"
#include "painter.h"
#include "store.h"
#include <stdbool.h>

struct funnel_opts {
  struct painter *painter;
  struct ui_node **queue;
  struct store *store;
  void *closure;
};

struct funnel {
  void *closure; // doesn't get freed
  void (*callback)(struct funnel_opts *opts);
  void *specs;
};

struct sink {
  // TODO: make this a list idk
  array(struct funnel, funnels);

  bool (*check_funnel)(struct funnel *funnel, void *attribs);
  int size, capacity;
};

int catch (struct painter *painter, struct ui_node **queue, struct store *store,
           struct sink *sink, void *attribs);
int register_funnel(struct sink *sink, struct funnel *funnel);

int init_sink(struct sink *sink,
              bool (*check_funnel)(struct funnel *funnel, void *attribs));
void free_sink(struct sink *sink);

#endif
