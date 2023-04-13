#include "toolbar.h"
#include "consts.h"
#include "draw_queue.h"
#include "macros.h"
#include <stdlib.h>

void onclick_table(struct funnel_opts *opts) {
  int_fast8_t *flags = opts->store->items[STORE_TOOLBAR_DROPDOWNS];

  if (*flags & MASK_TOOLBAR_DROPDOWN_TABLE) {
    *opts->queue =
        remove_node(opts->painter, *opts->queue, MARKS_TOOLBAR_DROPDOWN_TABLE);
    *flags ^= MASK_TOOLBAR_DROPDOWN_TABLE;
    return;
  }

  struct menu *menu = calloc(1, sizeof(struct menu));
  char *label_text[] = TOOLBAR_OPTIONS;
  struct box *box = opts->closure;
  int x = box->x;
  int y = box->y + box->height;
  struct ui_node *node;

  init_menu(opts->painter, 4, x, y, menu, TABLE_ORIGIN_TOP_LEFT);
  sync_menu(opts->painter, label_text, 4, 1, menu);
  make_node(menu, (int (*)(struct painter *, void *)) & draw_menu,
            (void (*)(struct painter *, void *)) & free_menu, 0,
            MARKS_TOOLBAR_DROPDOWN_TABLE, &node);
  *opts->queue = emplace_node(*opts->queue, node);
  *flags |= MASK_TOOLBAR_DROPDOWN_TABLE;
}

int init_toolbar(struct painter *painter, struct menu *toolbar) {
  return init_menu(painter, TOOLBAR_OPTION_COUNT, 800, 0, toolbar,
                   TABLE_ORIGIN_TOP_RIGHT);
}

int sync_toolbar(struct painter *painter, struct sink *sink,
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
  struct funnel funnel[] = {
      {&toolbar->table.layout[0], &onclick_table, specs},
  };
  register_funnel(sink, &funnel[0]);
  /* for (int i = 0; i < TOOLBAR_OPTION_COUNT; i++) { */
  /* register_click_funnel(sink, &funnel[i]); */
  /* } */

  return 0;
}
