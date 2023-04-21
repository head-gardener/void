#include "list.h"
#include <stdlib.h>

void free_node(void *closure, struct node *node) {
  node->free(closure, node->obj);
  free(node);
}

void free_list(struct list *list) {
  foreach_node(list->head, free_node(list->closure, node));
}

// PERF: tail call optimization
struct node *emplace_node(struct node *to, struct node *node) {
  if (to && to->height < node->height) {
    to->next = emplace_node(to->next, node);
    return to;
  }

  node->next = to;

  return node;
}

struct node *find_node(struct node *from, int mark) {
  if (from && from->mark != mark) {
    return find_node(from->next, mark);
  }

  return from;
}

struct node *remove_node(void *closure, struct node *from, int mark) {
  if (!from)
    return 0;

  if (from->mark != mark) {
    from->next = remove_node(closure, from->next, mark);
    return from;
  } else {
    free_node(closure, from);
    return from->next;
  }
}

struct node *remove_all_nodes(void *closure, struct node *from, int mark) {
  if (!from)
    return 0;

  if (from->mark != mark) {
    from->next = remove_all_nodes(closure, from->next, mark);
    return from;
  } else {
    free_node(closure, from);
    return remove_all_nodes(closure, from->next, mark);
  }
}
