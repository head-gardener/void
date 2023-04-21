#ifndef VOID_GUI_STORE
#define VOID_GUI_STORE

#include "array.h"

struct store {
  array(void *, items);
  int capacity;
};

int init_store(struct store *store, int capacity);
void free_store(struct store *store);

int store_expand(struct store *store, int n);
int store_ensure_fits(struct store *store, int i);
void store_put(struct store *store, int i, void *elem);
void store_put_unsafe(struct store *store, int i, void *elem);
void store_get(struct store *store, int i, void **elem);

#endif
