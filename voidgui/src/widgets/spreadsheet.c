#include "spreadsheet.h"
#include "consts.h"
#include "draw_queue.h"
#include "macros.h"
#include "menu.h"
#include "state.h"
#include "text_input_sink.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct spreadsheet_click_sink_closure {
  wchar_t *text;
  int cell_n;
  struct sink *text_input_sink;
};

void if_ontext(struct state *state, wchar_t **closure) {
  struct node *node =
      find_node(state->queue.head, MARKS_SPREADSHEET_INPUT_FIELD);
  struct ui_node_obj *obj = node->obj;
  sync_menu(state, closure, obj->ui_obj);
}

void ss_input_field_cleanup(struct state *state) {
  state->queue.head = remove_node(&state->painter, state->queue.head,
                                  MARKS_SPREADSHEET_INPUT_FIELD);
  state->queue.head =
      remove_node(&state->painter, state->queue.head, MARKS_MENU_CURSOR);
  state->text_input_sink.funnels.head = remove_node(
      0, state->text_input_sink.funnels.head, MARKS_SPREADSHEET_TEXT_SINK);
}

void if_oncommit(struct state *state, wchar_t **closure) {
  int *flag = state->store.items[STORE_SPREADSHEET_INPUT_FIELD];
  if (!*flag)
    return;

  ss_input_field_cleanup(state);

  struct node *node = find_node(state->queue.head, MARKS_SPREADSHEET);
  struct ui_node_obj *obj = node->obj;
  struct spreadsheet *ssheet = obj->ui_obj;
  free(ssheet->data[*flag - 1]);
  ssheet->data[*flag - 1] = *closure;
  sync_spreadsheet(state, obj->ui_obj);

  *flag = 0;
}

void if_oncancel(struct state *state, wchar_t **closure) {
  int *flag = state->store.items[STORE_SPREADSHEET_INPUT_FIELD];
  if (!*flag)
    return;

  free(*closure);
  ss_input_field_cleanup(state);

  *flag = 0;
}

void ss_onclick(struct state *state, void *_closure) {
  int *flag = state->store.items[STORE_SPREADSHEET_INPUT_FIELD];
  struct spreadsheet_click_sink_closure *closure = _closure;

  if (*flag) {
    return;
  }

  struct menu *menu = calloc(1, sizeof(struct menu));

  struct text_funnel_specs *specs = calloc(1, sizeof(struct text_funnel_specs));
  specs->text = calloc(128, sizeof(wchar_t));
  wcscpy(specs->text, closure->text);
  int len = wcslen(specs->text);
  specs->cursor = specs->text + len;
  specs->oncommit = (funnel_callback)&if_oncommit;
  specs->oncancel = (funnel_callback)&if_oncancel;
  struct funnel *input_funnel = calloc(1, sizeof(struct funnel));
  input_funnel->closure = &specs->text;
  input_funnel->callback = (funnel_callback)&if_ontext;
  input_funnel->specs = specs;
  input_funnel->free_closure = false;
  register_funnel(closure->text_input_sink, 9, MARKS_SPREADSHEET_TEXT_SINK,
                  input_funnel);

  struct cursor_extension_closure *cursor_closure =
      calloc(1, sizeof(struct cursor_extension_closure));
  cursor_closure->cursor = &specs->cursor;
  cursor_closure->draw_queue = &state->queue;
  init_menu(&state->painter, &state->painter.window_box.x,
            &state->painter.window_box.height, 1, 1, menu,
            TABLE_ORIGIN_BOTTOM_LEFT);
  menu->onplot = (menu_extension *)&draw_menu_cursor;
  menu->closure = cursor_closure;
  menu->free_closure = true;

  sync_menu(state, &specs->text, menu);
  register_ui_node(menu, true, (ui_callback *)&plot_menu,
                   (ui_callback *)&draw_menu, (ui_callback *)&free_menu, 8,
                   MARKS_SPREADSHEET_INPUT_FIELD, &state->queue);

  *flag = closure->cell_n + 1;
}

int init_spreadsheet(struct state *state, struct spreadsheet *ssheet, int x,
                     int y) {
  int code = 0;

  failure_condition_with_code(
      init_table(&state->painter, &ssheet->table,
                 SPREADSHEET_INNITIAL_CAPACITY * DATA_FIELD_COUNT),
      1);
  failure_condition_with_code(
      !(init_array(wchar_t *, ssheet->data, SPREADSHEET_INNITIAL_CAPACITY)), 2);
  failure_condition_with_code(
      !(init_array(struct size, ssheet->sizes, SPREADSHEET_INNITIAL_CAPACITY)),
      2);
  failure_condition_with_code(
      !(init_array(int_fast8_t, ssheet->dirty, SPREADSHEET_INNITIAL_CAPACITY)),
      2);

  ssheet->size = 0;
  ssheet->capacity = SPREADSHEET_INNITIAL_CAPACITY;

  ssheet->table.horz_padding = 10;
  ssheet->table.vert_padding = 10;

  ssheet->origin.x = x;
  ssheet->origin.y = y;

  store_ensure_fits(&state->store, STORE_SPREADSHEET_INPUT_FIELD);
  state->store.items[STORE_SPREADSHEET_INPUT_FIELD] = calloc(1, sizeof(bool));

  return 0;

failed:
  free_table(&state->painter, &ssheet->table);
  if (ssheet->data)
    free(ssheet->data);
  if (ssheet->dirty)
    free(ssheet->dirty);

  return code;
}

int sync_spreadsheet(struct state *state, struct spreadsheet *ssheet) {
  unsigned char **surfaces;
  int label_count = ssheet->size * DATA_FIELD_COUNT;

  failure_condition(!(surfaces = calloc(label_count, sizeof(void *))));

  for (int i = 0; i < ssheet->size; ++i) {
    /* if (ssheet->dirty[i]) */
    failure_condition(render_text(ssheet->data[i], &ssheet->sizes[i].width,
                                  &ssheet->sizes[i].height, &surfaces[i]));

    ssheet->dirty[i] = 0;
  }

  sync_table(&state->painter, &ssheet->table, ssheet->size / 2,
             DATA_FIELD_COUNT, ssheet->sizes, surfaces);

  for (int i = 0; i < label_count; i++)
    free(surfaces[i]);

  free(surfaces);

  plot_spreadsheet(state, ssheet);
  return 0;

failed:
  // TODO: clean up
  return 1;
}

void plot_spreadsheet(struct state *state, struct spreadsheet *ssheet) {
  plot_table(&state->painter, &ssheet->table, ssheet->sizes, ssheet->origin.x,
             ssheet->origin.y, ssheet->size / 2, DATA_FIELD_COUNT);

  state->click_sink.funnels.head = remove_all_nodes(
      0, state->click_sink.funnels.head, MARKS_SPREADSHEET_CLICK_SINK);
  for (int i = 0; i < ssheet->size; ++i) {
    struct click_funnel_specs *specs =
        calloc(1, sizeof(struct click_funnel_specs));
    specs->box = ssheet->table.layout[i];
    specs->inverted = false;
    struct spreadsheet_click_sink_closure *closure =
        calloc(1, sizeof(struct spreadsheet_click_sink_closure));
    closure->text = ssheet->data[i];
    closure->text_input_sink = &state->text_input_sink;
    closure->cell_n = i;
    struct funnel *click_funnel = calloc(1, sizeof(struct funnel));
    click_funnel->closure = closure;
    click_funnel->callback = &ss_onclick;
    click_funnel->specs = specs;
    click_funnel->free_closure = true;
    register_funnel(&state->click_sink, 0, MARKS_SPREADSHEET_CLICK_SINK,
                    click_funnel);
  }
}

void draw_spreadsheet(struct state *state, struct spreadsheet *ssheet) {
  draw_table(&state->painter, &ssheet->table);
}

void free_spreadsheet(struct state *state, struct spreadsheet *ssheet) {
  free_table(&state->painter, &ssheet->table);

  for (int i = 0; i < ssheet->size; i++) {
    free(ssheet->data[i]);
  }
  free(ssheet->data);
  free(ssheet->sizes);
  free(ssheet->dirty);
}

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    wchar_t *name, wchar_t *phone) {
  if (ssheet->size + 1 >= ssheet->capacity) {
    array_expand(wchar_t *, ssheet->data, ssheet->capacity, 2, goto failed);
    array_expand(struct size, ssheet->sizes, ssheet->capacity, 2, goto failed);
    array_expand(bool, ssheet->dirty, ssheet->capacity, 2, goto failed);

    ssheet->capacity *= 2;
  }

  failure_condition(table_grow(painter, &ssheet->table, 2));

  ssheet->dirty[ssheet->size] = 0;
  ssheet->data[ssheet->size++] = name;
  ssheet->dirty[ssheet->size] = 0;
  ssheet->data[ssheet->size++] = phone;

  return 0;

failed:
  return 2;
}
