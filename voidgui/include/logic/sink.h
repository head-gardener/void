#ifndef VOID_GUI_SINK
#define VOID_GUI_SINK

#include "array.h"
#include "draw_queue.h"
#include "list.h"
#include "painter.h"
#include "store.h"
#include <stdbool.h>

struct funnel_opts {
  struct painter *painter;
  struct list *queue;
  struct store *store;
  struct sink *click_sink;
  struct sink *text_input_sink;
  void *closure;
};

typedef void (*funnel_callback)(struct funnel_opts *);

struct funnel {
  void *closure;            // passed to callback
  funnel_callback callback; // gets called with closure when
                            // funnel passes check
  void *specs;              // passed to checker
  bool free_closure;        // whether closure needs to be freed
};

struct sink {
  struct list funnels;

  funnel_callback (*check_funnel)(struct funnel *funnel, void *attribs);
};

int catch (struct painter *painter, struct sink *click_sink,
           struct sink *text_input_sink, struct list *queue,
           struct store *store, struct sink *sink, void *attribs);
int register_funnel(struct sink *sink, int height, int mark,
                    struct funnel *funnel);

int init_sink(struct sink *sink,
              funnel_callback (*check_funnel)(struct funnel *funnel,
                                              void *attribs));
void free_sink(struct sink *sink);

#endif
