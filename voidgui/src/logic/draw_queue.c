#include "draw_queue.h"
#include <stdlib.h>

int make_node(void *object, int (*draw)(struct painter *, void *),
              void (*free)(struct painter *, void *), int depth, int mark,
              struct ui_node **node) {
  *node = calloc(1, sizeof(struct ui_node));

  (*node)->obj = object;
  (*node)->draw = draw;
  (*node)->free = free;
  (*node)->depth = depth;
  (*node)->mark = mark;

  return 0;
}

void free_node(struct painter *painter, struct ui_node *node) {
  node->free(painter, node->obj);
  free(node);
}

// PERF: tail call optimization
struct ui_node *emplace_node(struct ui_node *to, struct ui_node *node) {
  if (to && to->depth <= node->depth) {
    to->next = emplace_node(to->next, node);
    return to;
  }

  node->next = to;

  return node;
}

int draw_node(struct painter *painter, struct ui_node *node) {
  return node->draw(painter, node->obj);
}

struct ui_node *remove_node(struct painter *painter, struct ui_node *from,
                            int mark) {
  if (from) {
    if (from->mark != mark) {
      from->next = remove_node(painter, from->next, mark);
      return from;
    } else {
      free_node(painter, from);
      return from->next;
    }
  }

  return 0;
}
