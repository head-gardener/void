#ifndef VOID_GUI_DRAW_QUEUE
#define VOID_GUI_DRAW_QUEUE

#include "list.h"
#include "painter.h"
#include "sink.h"
#include <stdbool.h>

typedef void ui_callback(struct state *, void *);

/**
 * An UI element, used to form a queue for drawing and plotting all high level
 * objects on the screen.
 * Doesn't cover initialization and syncing, those should be handled by funnels.
 */
struct ui_node_obj {
  ui_callback *plot;
  ui_callback *draw;
  ui_callback *free;

  bool plotted;

  void *ui_obj;
  bool free_obj;
};

void init_draw_queue(struct painter *painter, struct list *draw_queue);

/**
 * Create and emplace new node.
 */
struct node *register_ui_node(void *ui_object, bool free_obj, ui_callback *plot,
                              ui_callback *draw, ui_callback *free, int height,
                              int mark, struct list *draw_queue);
/**
 * Draw a node, plotting it if needed.
 */
void display_ui_node(struct state *state, struct node *node);
void request_plotting(struct node *node);

#endif
