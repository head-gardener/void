#include "draw_queue.h"
#include <stdlib.h>

void dq_free_wrapper(void *painter, void *obj) {
  struct ui_node_obj *node_obj = obj;
  node_obj->free((struct painter *)painter, node_obj->ui_obj);
  if (node_obj->free_obj)
    free(node_obj->ui_obj);
}

void init_draw_queue(struct painter *painter, struct list *draw_queue) {
  draw_queue->closure = painter;
}

int make_ui_node(void *ui_object, bool free_obj,
                 int (*draw)(struct painter *, void *),
                 void (*free)(struct painter *, void *), int height, int mark,
                 struct node **node) {
  *node = calloc(1, sizeof(struct node));
  if (!node)
    return 2;

  struct ui_node_obj *obj = calloc(1, sizeof(struct ui_node_obj));
  obj->draw = draw;
  obj->ui_obj = ui_object;
  obj->free = free;
  obj->free_obj = free_obj;

  (*node)->obj = obj;
  (*node)->free = &dq_free_wrapper;
  (*node)->height = height;
  (*node)->mark = mark;

  return 0;
}

int draw_node(struct painter *painter, struct node *node) {
  struct ui_node_obj *obj = node->obj;
  return obj->draw(painter, obj->ui_obj);
}
