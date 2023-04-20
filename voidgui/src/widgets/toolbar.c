#include "toolbar.h"
#include "consts.h"
#include "draw_queue.h"
#include "key_sink.h"
#include "macros.h"
#include "state.h"
#include <stdlib.h>

void tb_cleanup(struct state *state) {
  state->queue.head = remove_node(&state->painter, state->queue.head,
                                  MARKS_TOOLBAR_DROPDOWN_TABLE);
  state->click_sink.funnels.head =
      remove_all_nodes(&state->painter, state->click_sink.funnels.head,
                       MARKS_TOOLBAR_TEMP_CLICK_SINK);
  state->key_sink.funnels.head =
      remove_all_nodes(0, state->key_sink.funnels.head, MARKS_TOOLBAR_KEY_SINK);
}

void tb_onkey(struct state *state, void *closure) {
  int_fast8_t *flags = state->store.items[STORE_TOOLBAR_DROPDOWNS];

  if (*flags & MASK_TOOLBAR_DROPDOWN_TABLE) {
    tb_cleanup(state);
    *flags = 0;
  }
}

void tb_tbl_push_onclick(struct state *state, void *closure) {
  tb_cleanup(state);
  state->return_code = 2;

  int_fast8_t *flags = state->store.items[STORE_TOOLBAR_DROPDOWNS];
  *flags = 0;
}

void tb_tbl_onclick(struct state *state, void *closure) {
  int_fast8_t *flags = state->store.items[STORE_TOOLBAR_DROPDOWNS];

  if (*flags & MASK_TOOLBAR_DROPDOWN_TABLE) {
    tb_cleanup(state);
    *flags ^= MASK_TOOLBAR_DROPDOWN_TABLE;
    return;
  }

  struct menu *menu = calloc(1, sizeof(struct menu));
  wchar_t *label_text[] = TOOLBAR_TABLE_OPTIONS;
  struct menu *toolbar = closure;
  int *x = &toolbar->table.layout[0].x;
  int *y = &toolbar->table.box.height;

  init_menu(&state->painter, x, y, 4, 1, menu, TABLE_ORIGIN_TOP_LEFT);
  sync_menu(state, label_text, menu);
  register_ui_node(menu, true, (ui_callback *)&plot_menu,
                   (ui_callback *)&draw_menu, (ui_callback *)&free_menu, 5,
                   MARKS_TOOLBAR_DROPDOWN_TABLE, &state->queue);

  key_funnel_specs *specs = calloc(1, sizeof(key_funnel_specs));
  *specs = SDL_SCANCODE_ESCAPE;
  struct funnel *funnel = calloc(1, sizeof(struct funnel));
  funnel->closure = 0;
  funnel->callback = &tb_onkey;
  funnel->specs = specs;
  funnel->free_closure = false;
  register_funnel(&state->key_sink, 0, MARKS_TOOLBAR_KEY_SINK, funnel);

  struct click_funnel_specs *specs_push =
      calloc(1, sizeof(struct click_funnel_specs));
  specs_push->box = menu->table.layout[0];
  funnel = calloc(1, sizeof(struct funnel));
  funnel->closure = 0;
  funnel->callback = &tb_tbl_push_onclick;
  funnel->specs = specs_push;
  funnel->free_closure = false;
  register_funnel(&state->click_sink, 0, MARKS_TOOLBAR_TEMP_CLICK_SINK, funnel);

  *flags |= MASK_TOOLBAR_DROPDOWN_TABLE;
}

int init_toolbar(struct painter *painter, struct menu *toolbar) {
  return init_menu(painter, &painter->window_box.width, &painter->window_box.y,
                   1, TOOLBAR_OPTION_COUNT, toolbar, TABLE_ORIGIN_TOP_RIGHT);
}

void sync_toolbar(struct state *state, struct menu *toolbar) {
  wchar_t *label_text[] = TOOLBAR_OPTIONS;

  sync_menu(state, label_text, toolbar);

  store_ensure_fits(&state->store, STORE_TOOLBAR_DROPDOWNS);
  state->store.items[STORE_TOOLBAR_DROPDOWNS] = calloc(1, sizeof(int_fast8_t));

  plot_toolbar(state, toolbar);
}

void plot_toolbar(struct state *state, struct menu *toolbar) {
  plot_menu(state, toolbar);

  state->click_sink.funnels.head = remove_all_nodes(
      0, state->click_sink.funnels.head, MARKS_TOOLBAR_CLICK_SINK);
  struct click_funnel_specs *specs =
      calloc(1, sizeof(struct click_funnel_specs));
  specs->box = toolbar->table.layout[0];
  specs->inverted = false;
  struct funnel *funnel = calloc(1, sizeof(struct funnel));
  funnel->closure = toolbar;
  funnel->callback = &tb_tbl_onclick;
  funnel->specs = specs;
  funnel->free_closure = false;
  register_funnel(&state->click_sink, 0, MARKS_TOOLBAR_CLICK_SINK, funnel);
}
