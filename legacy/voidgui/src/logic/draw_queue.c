#include "draw_queue.h"
#include <stdlib.h>

void dq_free_wrapper(void *state, void *obj) {
  struct ui_node_obj *node_obj = obj;
  node_obj->free(state, node_obj->ui_obj);
  if (node_obj->free_obj)
    free(node_obj->ui_obj);
}

void init_draw_queue(struct painter *painter, struct list *draw_queue) {
  draw_queue->closure = painter;
}

struct node *register_ui_node(void *ui_object, bool free_obj, ui_callback *plot,
                              ui_callback *draw, ui_callback *_free, int height,
                              int mark, struct list *draw_queue) {
  struct node *node = calloc(1, sizeof(struct node));
  if (!node)
    return 0;
  struct ui_node_obj *obj = calloc(1, sizeof(struct ui_node_obj));
  if (!obj) {
    free(node);
    return 0;
  }

  obj->draw = draw;
  obj->plot = plot;
  obj->free = _free;
  obj->ui_obj = ui_object;
  obj->free_obj = free_obj;
  obj->plotted = true;

  node->obj = obj;
  node->free = &dq_free_wrapper;
  node->height = height;
  node->mark = mark;

  draw_queue->head = emplace_node(draw_queue->head, node);

  return node;
}

void display_ui_node(struct state *state, struct node *node) {
  struct ui_node_obj *obj = node->obj;
  if (!obj->plotted) {
    obj->plot(state, obj->ui_obj);
    obj->plotted = true;
  }
  obj->draw(state, obj->ui_obj);
}

void request_plotting(struct node *node) {
  struct ui_node_obj *obj = node->obj;
  obj->plotted = false;
}
