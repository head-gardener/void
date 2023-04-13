#ifndef VOID_GUI_DRAW_QUEUE
#define VOID_GUI_DRAW_QUEUE

#include "painter.h"

struct ui_node {
  void *obj;
  int (*draw)(struct painter *, void *);
  void (*free)(struct painter *, void *);

  int depth;
  int mark;

  struct ui_node *next;
};

int make_node(void *object, int (*draw)(struct painter *, void *),
              void (*free)(struct painter *, void *), int depth, int mark,
              struct ui_node **node);
void free_node(struct painter *painter, struct ui_node *node);

struct ui_node *emplace_node(struct ui_node *to, struct ui_node *node);
int draw_node(struct painter *painter, struct ui_node *node);
struct ui_node *remove_node(struct painter *painter, struct ui_node *from,
                            int mark);

#define foreach_node(_node, body)                                              \
  {                                                                            \
    struct ui_node *node = _node;                                              \
    struct ui_node *_tmp;                                                      \
    while (node) {                                                             \
      _tmp = node->next;                                                       \
      body;                                                                    \
      node = _tmp;                                                             \
    }                                                                          \
  }

#endif
