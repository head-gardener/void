#ifndef VOID_GUI_LIST
#define VOID_GUI_LIST

struct node {
  void *obj;
  void (*free)(void *, void *);

  int height;
  int mark;

  struct node *next;
};

struct list {
  void *closure;
  struct node *head;
};

void free_node(void *closure, struct node *node);
void free_list(struct list *list);

struct node *emplace_node(struct node *to, struct node *node);
struct node *find_node(struct node *from, int mark);
struct node *remove_node(void *closure, struct node *from, int mark);
struct node *remove_all_nodes(void *closure, struct node *from, int mark);

#define foreach_node(_node, body)                                              \
  {                                                                            \
    struct node *node = _node;                                                 \
    struct node *_tmp;                                                         \
    while (node) {                                                             \
      _tmp = node->next;                                                       \
      body;                                                                    \
      node = _tmp;                                                             \
    }                                                                          \
  }

#endif
