#ifndef VOID_GUI_DRAW_QUEUE
#define VOID_GUI_DRAW_QUEUE

#include "list.h"
#include "painter.h"
#include <stdbool.h>

struct ui_node_obj {
  int (*draw)(struct painter *, void *);
  void (*free)(struct painter *, void *);
  void *ui_obj;
  bool free_obj;
};

void init_draw_queue(struct painter *painter, struct list *draw_queue);

int make_ui_node(void *ui_object, bool free_obj,
              int (*draw)(struct painter *, void *),
              void (*free)(struct painter *, void *), int height, int mark,
              struct node **node);
int draw_node(struct painter *painter, struct node *node);

#endif
