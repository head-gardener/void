#include "store.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>

int init_store(struct store *store, int capacity) {
  failure_condition(!(init_array(void *, store->items, capacity)));
  store->capacity = capacity;

  return 0;

failed:
  if (store->items)
    free(store->items);
  return 2;
}

void free_store(struct store *store) {
  for (int i = 0; i < store->capacity; i++)
    if (store->items[i])
      free(store->items[0]);
  free(store->items);
}

int store_expand(struct store *store, int n) {
  while (store->capacity < n) {
    array_expand(void *, store->items, store->capacity, 2, goto failed);
    store->capacity *= 2;
  }

  return 0;

failed:
  return 1;
}

int store_ensure_fits(struct store *store, int i) {
  if (i < store->capacity) {
    return store_expand(store, i);
  }
  return 0;
}

void store_put(struct store *store, int i, void *elem) {
  store_ensure_fits(store, i);
  store_put_unsafe(store, i, elem);
}

void store_put_unsafe(struct store *store, int i, void *elem) {
  store->items[i] = elem;
}

void store_get(struct store *store, int i, void **elem) {
  *elem =  i < store->capacity ? store->items[i] : 0;
}
