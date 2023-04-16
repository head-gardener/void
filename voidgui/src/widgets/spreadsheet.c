#include "spreadsheet.h"
#include "consts.h"
#include "draw_queue.h"
#include "macros.h"
#include "menu.h"
#include "text_input_sink.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct spreadsheet_click_sink_closure {
  wchar_t *text;
  int cell_n;
  struct sink *text_input_sink;
};

void if_ontext(struct funnel_opts *opts) {
  struct node *node =
      find_node(opts->queue->head, MARKS_SPREADSHEET_INPUT_FIELD);
  opts->queue->head =
      remove_node(opts->painter, opts->queue->head, MARKS_MENU_CURSOR);
  struct ui_node_obj *obj = node->obj;
  sync_menu(opts->painter, (wchar_t **)opts->closure, 1, 1, obj->ui_obj);
}

void ss_input_field_cleanup(struct funnel_opts *opts) {
  opts->queue->head = remove_node(opts->painter, opts->queue->head,
                                  MARKS_SPREADSHEET_INPUT_FIELD);
  opts->queue->head =
      remove_node(opts->painter, opts->queue->head, MARKS_MENU_CURSOR);
  opts->text_input_sink->funnels.head = remove_node(
      0, opts->text_input_sink->funnels.head, MARKS_SPREADSHEET_TEXT_SINK);
}

void if_oncommit(struct funnel_opts *opts) {
  int *flag = opts->store->items[STORE_SPREADSHEET_INPUT_FIELD];
  if (!*flag)
    return;

  ss_input_field_cleanup(opts);

  struct node *node = find_node(opts->queue->head, MARKS_SPREADSHEET);
  struct ui_node_obj *obj = node->obj;
  struct spreadsheet *ssheet = obj->ui_obj;
  free(ssheet->data[*flag - 1]);
  ssheet->data[*flag - 1] = *(wchar_t **)opts->closure;
  sync_spreadsheet(opts->painter, opts->click_sink, opts->text_input_sink,
                   obj->ui_obj);

  *flag = 0;
}

void if_oncancel(struct funnel_opts *opts) {
  int *flag = opts->store->items[STORE_SPREADSHEET_INPUT_FIELD];
  if (!*flag)
    return;

  free(*(wchar_t **)opts->closure);
  ss_input_field_cleanup(opts);

  *flag = 0;
}

void ss_onclick(struct funnel_opts *opts) {
  int *flag = opts->store->items[STORE_SPREADSHEET_INPUT_FIELD];
  struct spreadsheet_click_sink_closure *closure = opts->closure;

  if (*flag) {
    return;
  }

  struct menu *menu = calloc(1, sizeof(struct menu));
  struct node *node;

  struct text_funnel_specs *specs = calloc(1, sizeof(struct text_funnel_specs));
  specs->text = calloc(128, sizeof(wchar_t));
  wcscpy(specs->text, closure->text);
  int len = wcslen(specs->text);
  specs->cursor = specs->text + len;
  specs->oncommit = &if_oncommit;
  specs->oncancel = &if_oncancel;
  struct funnel *input_funnel = calloc(1, sizeof(struct funnel));
  input_funnel->closure = &specs->text;
  input_funnel->callback = &if_ontext;
  input_funnel->specs = specs;
  input_funnel->free_closure = false;
  register_funnel(closure->text_input_sink, 9, MARKS_SPREADSHEET_TEXT_SINK,
                  input_funnel);

  struct cursor_extension_closure *cursor_closure =
      calloc(1, sizeof(struct cursor_extension_closure));
  cursor_closure->cursor = &specs->cursor;
  cursor_closure->draw_queue = opts->queue;
  init_menu(opts->painter, 1, 0, opts->painter->window_box.height, menu,
            TABLE_ORIGIN_BOTTOM_LEFT);
  menu->onsync = (menu_extension *)&draw_menu_cursor;
  menu->closure = cursor_closure;
  menu->free_closure = true;

  sync_menu(opts->painter, &specs->text, 1, 1, menu);
  make_ui_node(menu, true, (int (*)(struct painter *, void *)) & draw_menu,
               (void (*)(struct painter *, void *)) & free_menu, 8,
               MARKS_SPREADSHEET_INPUT_FIELD, &node);
  opts->queue->head = emplace_node(opts->queue->head, node);
  *flag = closure->cell_n + 1;
}

int init_spreadsheet(struct painter *painter, struct store *store,
                     struct spreadsheet *ssheet, int x, int y) {
  int code = 0;

  failure_condition_with_code(
      init_table(painter, &ssheet->table,
                 SPREADSHEET_INNITIAL_CAPACITY * DATA_FIELD_COUNT, x, y),
      1);
  failure_condition_with_code(
      !(init_array(struct data, ssheet->data, SPREADSHEET_INNITIAL_CAPACITY)),
      2);
  failure_condition_with_code(
      !(init_array(int_fast8_t, ssheet->dirty, SPREADSHEET_INNITIAL_CAPACITY)),
      2);

  ssheet->size = 0;
  ssheet->capacity = SPREADSHEET_INNITIAL_CAPACITY;

  ssheet->table.horz_padding = 10;
  ssheet->table.vert_padding = 10;

  store_ensure_fits(store, STORE_SPREADSHEET_INPUT_FIELD);
  store->items[STORE_SPREADSHEET_INPUT_FIELD] = calloc(1, sizeof(bool));

  return 0;

failed:
  free_table(painter, &ssheet->table);
  if (ssheet->data)
    free(ssheet->data);
  if (ssheet->dirty)
    free(ssheet->dirty);

  return code;
}

int sync_spreadsheet(struct painter *painter, struct sink *click_sink,
                     struct sink *text_input_sink, struct spreadsheet *ssheet) {
  // table
  struct size *sizes;
  unsigned char **surfaces;
  int label_count = ssheet->size * DATA_FIELD_COUNT;

  failure_condition(!(sizes = calloc(label_count, sizeof(struct size))));
  failure_condition(!(surfaces = calloc(label_count, sizeof(void *))));

  for (int i = 0; i < ssheet->size; ++i) {
    /* if (ssheet->dirty[i]) */
    failure_condition(render_text(ssheet->data[i], &sizes[i].width,
                                  &sizes[i].height, &surfaces[i]));

    ssheet->dirty[i] = 0;
  }

  plot_table_with_sizes(&ssheet->table, ssheet->size / 2, DATA_FIELD_COUNT,
                        sizes);
  failure_condition(sync_table(painter, &ssheet->table, surfaces, sizes,
                               ssheet->size / 2, DATA_FIELD_COUNT));

  for (int i = 0; i < label_count; i++)
    free(surfaces[i]);

  free(sizes);
  free(surfaces);

  click_sink->funnels.head = remove_all_nodes(0, click_sink->funnels.head,
                                              MARKS_SPREADSHEET_CLICK_SINK);

  // click funnels
  for (int i = 0; i < ssheet->size; ++i) {
    struct click_funnel_specs *specs =
        calloc(1, sizeof(struct click_funnel_specs));
    specs->box = ssheet->table.layout[i];
    specs->inverted = false;
    struct spreadsheet_click_sink_closure *closure =
        calloc(1, sizeof(struct spreadsheet_click_sink_closure));
    closure->text = ssheet->data[i];
    closure->text_input_sink = text_input_sink;
    closure->cell_n = i;
    // TODO: write down label number to flag
    struct funnel *click_funnel = calloc(1, sizeof(struct funnel));
    click_funnel->closure = closure;
    click_funnel->callback = &ss_onclick;
    click_funnel->specs = specs;
    click_funnel->free_closure = true;
    register_funnel(click_sink, 0, MARKS_SPREADSHEET_CLICK_SINK, click_funnel);
  }

failed:
  // TODO: clean up
  return 1;
}

int draw_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  draw_table(painter, &ssheet->table);

  return 0;
}

void free_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  free_table(painter, &ssheet->table);

  for (int i = 0; i < ssheet->size; i++) {
    free(ssheet->data[i]);
  }
  free(ssheet->data);
  free(ssheet->dirty);
}

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    wchar_t *name, wchar_t *phone) {
  if (ssheet->size + 1 >= ssheet->capacity) {
    array_expand(wchar_t *, ssheet->data, ssheet->capacity, 2, goto failed);
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
