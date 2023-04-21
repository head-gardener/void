#include "menu.h"
#include "consts.h"
#include "macros.h"
#include "shape_buffer.h"
#include "state.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int init_menu(struct painter *painter, int *x, int *y, int rows, int columns,
              struct menu *menu, enum origin_position origin_pos) {
  bool table = false;
  int cap = rows * columns;

  failure_condition(init_table(painter, &menu->table, cap));
  table = true;
  failure_condition(table_grow(painter, &menu->table, cap));
  failure_condition(!(init_array(struct size, menu->sizes, cap)));
  failure_condition(!(init_array(unsigned char *, menu->surfaces, cap)));

  menu->table.horz_padding = 10;
  menu->table.vert_padding = 10;
  menu->table.origin_pos = origin_pos;
  menu->rows = rows;
  menu->columns = columns;
  menu->origin.x = x;
  menu->origin.y = y;

  return 0;

failed:
  if (menu->sizes)
    free(menu->sizes);
  if (menu->surfaces)
    free(menu->surfaces);
  if (table)
    free_table(painter, &menu->table);

  return 1;
}

int sync_menu(struct state *state, wchar_t **labels, struct menu *menu) {
  // redraw labels as requested
  for (int i = 0; i < menu->rows * menu->columns; i++) {
    if (!labels[i])
      continue;

    unsigned char *surface;
    failure_condition(render_text(labels[i], &menu->sizes[i].width,
                                  &menu->sizes[i].height, &surface));

    if (menu->surfaces[i])
      free(menu->surfaces[i]);
    menu->surfaces[i] = surface;
  }

  // ensure all surfaces exist, fill empty ones with placeholder text
  for (int i = 0; i < menu->rows * menu->columns; i++) {
    if (menu->surfaces[i])
      continue;

    unsigned char *surface;
    failure_condition(render_text(MENU_PLACEHOLDER_TEXT, &menu->sizes[i].width,
                                  &menu->sizes[i].height, &surface));

    free(menu->surfaces[i]);
    menu->surfaces[i] = surface;
  }

  failure_condition(sync_table(&state->painter, &menu->table, menu->rows,
                               menu->columns, menu->sizes, menu->surfaces));

  plot_menu(state, menu);

  return 0;

failed:

  return 1;
}

void plot_menu(struct state *state, struct menu *menu) {
  plot_table(&state->painter, &menu->table, menu->sizes, *menu->origin.x,
             *menu->origin.y, menu->rows, menu->columns);

  if (menu->onplot)
    menu->onplot(menu->closure, state, &menu->table);
}

void draw_menu(struct state *state, struct menu *menu) {
  draw_table(&state->painter, &menu->table);
}

void free_menu(struct state *state, struct menu *menu) {
  if (menu->free_closure)
    free(menu->closure);
  free_table(&state->painter, &menu->table);
}

// extensions

void mc_plot_wrapper(struct state *state, void *shape) {
  struct cursor_shape *cursor = (struct cursor_shape *)shape;
  struct shape *cursor_shape_ptr =
      &state->painter.shape_buffer.shapes[cursor->shape_ptr];
  plot_rectangle(&state->painter.shaders, &state->painter.common,
                 cursor_shape_ptr, &cursor->box, &state->painter.window_box);
}

void mc_draw_wrapper(struct state *state, void *shape) {
  struct cursor_shape *cursor = (struct cursor_shape *)shape;
  float color[] = {.2, .2, .2, 1};

  prepare_rectangle(&state->painter);
  draw_rectangle(&state->painter, cursor->shape_ptr, color);
}

void mc_free_wrapper(struct state *state, void *shape) {
  struct cursor_shape *cursor = (struct cursor_shape *)shape;
  free_shape(&state->painter.shape_buffer.shapes[cursor->shape_ptr]);
}

void draw_menu_cursor(struct cursor_extension_closure *closure,
                      struct state *state, struct table *table) {
  shape_ptr cursor;
  struct box *text = &table->layout[0];
  struct size pre_cursor;

  get_text_size(*closure->cursor, &pre_cursor.width, &pre_cursor.height);

  struct box box = {
      text->x + text->width - table->horz_padding - pre_cursor.width,
      text->y + table->vert_padding, 2, text->height - 2 * table->vert_padding};

  get_new_shape(&state->painter.shape_buffer, &cursor);
  get_new_shape(&state->painter.shape_buffer, &cursor);
  get_new_shape(&state->painter.shape_buffer, &cursor);

  struct cursor_shape *ptr = calloc(1, sizeof(struct cursor_shape));
  ptr->shape_ptr = cursor;
  memcpy(&ptr->box, &box, sizeof(struct box));

  mc_plot_wrapper(state, ptr);
  state->queue.head =
      remove_node(&state->painter, state->queue.head, MARKS_MENU_CURSOR);
  register_ui_node(ptr, true, &mc_plot_wrapper, &mc_draw_wrapper,
                   &mc_free_wrapper, 9, MARKS_MENU_CURSOR, closure->draw_queue);
}
