#include "toolbar.h"
#include "consts.h"
#include "draw_queue.h"
#include "macros.h"
#include <stdlib.h>

void tb_onclick(struct funnel_opts *opts) {
  int_fast8_t *flags = opts->store->items[STORE_TOOLBAR_DROPDOWNS];

  if (*flags & MASK_TOOLBAR_DROPDOWN_TABLE) {
    opts->queue->head = remove_node(opts->painter, opts->queue->head,
                                    MARKS_TOOLBAR_DROPDOWN_TABLE);
    *flags ^= MASK_TOOLBAR_DROPDOWN_TABLE;
    return;
  }

  struct menu *menu = calloc(1, sizeof(struct menu));
  char *label_text[] = TOOLBAR_OPTIONS;
  struct box *box = opts->closure;
  int x = box->x;
  int y = box->y + box->height;
  struct node *node;

  init_menu(opts->painter, 4, x, y, menu, TABLE_ORIGIN_TOP_LEFT);
  sync_menu(opts->painter, label_text, 4, 1, menu);
  make_ui_node(menu, true, (int (*)(struct painter *, void *)) & draw_menu,
            (void (*)(struct painter *, void *)) & free_menu, 5,
            MARKS_TOOLBAR_DROPDOWN_TABLE, &node);
  opts->queue->head = emplace_node(opts->queue->head, node);
  *flags |= MASK_TOOLBAR_DROPDOWN_TABLE;
}

int init_toolbar(struct painter *painter, struct menu *toolbar) {
  return init_menu(painter, TOOLBAR_OPTION_COUNT, 800, 0, toolbar,
                   TABLE_ORIGIN_TOP_RIGHT);
}

int sync_toolbar(struct painter *painter, struct sink *click_sink,
                 struct store *store, struct menu *toolbar) {
  char *label_text[] = TOOLBAR_OPTIONS;

  int code = sync_menu(painter, label_text, 1, TOOLBAR_OPTION_COUNT, toolbar);
  if (code)
    return code;

  store_ensure_fits(store, STORE_TOOLBAR_DROPDOWNS);
  store->items[STORE_TOOLBAR_DROPDOWNS] = calloc(1, sizeof(int_fast8_t));

  struct click_funnel_specs *specs =
      calloc(1, sizeof(struct click_funnel_specs));
  specs->box = toolbar->table.layout[0];
  specs->inverted = false;
  struct funnel *funnel = calloc(1, sizeof(struct funnel));
  funnel->closure = &toolbar->table.layout[0];
  funnel->callback = &tb_onclick;
  funnel->specs = specs;
  funnel->free_closure = false;
  register_funnel(click_sink, 0, MARKS_TOOLBAR_CLICK_SINK, funnel);

  return 0;
}
