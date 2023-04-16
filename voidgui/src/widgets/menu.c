#include "menu.h"
#include "consts.h"
#include "macros.h"
#include "shape_buffer.h"
#include <stdbool.h>
#include <stdlib.h>

int init_menu(struct painter *painter, int capacity, int x, int y,
              struct menu *menu, enum origin_position origin_pos) {
  bool table = false;

  failure_condition(init_table(painter, &menu->table, capacity, x, y));
  table = true;
  failure_condition(table_grow(painter, &menu->table, capacity));

  menu->table.horz_padding = 10;
  menu->table.vert_padding = 10;
  menu->table.origin_pos = origin_pos;

  return 0;

failed:
  if (table)
    free_table(painter, &menu->table);

  return 1;
}

int sync_menu(struct painter *painter, wchar_t **labels, int rows, int columns,
              struct menu *menu) {
  struct size sizes[rows * columns];
  unsigned char *surfaces[rows * columns];

  for (int i = 0; i < rows * columns; i++)
    failure_condition(render_text(labels[i], &sizes[i].width, &sizes[i].height,
                                  &surfaces[i]));

  failure_condition(plot_table_with_sizes(&menu->table, rows, columns, sizes));
  failure_condition(
      sync_table(painter, &menu->table, surfaces, sizes, rows, columns));

  if (menu->onsync)
    menu->onsync(menu->closure, painter, labels, &menu->table);

  for (int i = 0; i < rows * columns; i++)
    free(surfaces[i]);

  return 0;

failed:
  for (int i = 0; i < rows * columns; i++)
    if (surfaces[i])
      free(surfaces[i]);

  return 1;
}

int draw_menu(struct painter *painter, struct menu *menu) {
  draw_table(painter, &menu->table);

  return 0;
}

void free_menu(struct painter *painter, struct menu *menu) {
  if (menu->free_closure)
    free(menu->closure);
  free_table(painter, &menu->table);
}

// extensions

int mc_draw_wrapper(struct painter *painter, void *shape) {
  shape_ptr cursor = *(shape_ptr *)shape;
  float color[] = {.2, .2, .2, 1};

  prepare_rectangle(painter);
  draw_rectangle(painter, cursor, color);

  return 0;
}

void mc_free_wrapper(struct painter *painter, void *shape) {
  shape_ptr cursor = *(shape_ptr *)shape;
  free_shape(&painter->shape_buffer.shapes[cursor]);
}

void draw_menu_cursor(struct cursor_extension_closure *closure,
                      struct painter *painter, wchar_t **labels,
                      struct table *table) {
  shape_ptr cursor;
  struct box *text = &table->layout[0];
  struct size pre_cursor;

  get_text_size(*closure->cursor, &pre_cursor.width, &pre_cursor.height);

  struct box box = {
      text->x + text->width - table->horz_padding - pre_cursor.width,
      text->y + table->vert_padding, 2, text->height - 2 * table->vert_padding};

  get_new_shape(&painter->shape_buffer, &cursor);
  get_new_shape(&painter->shape_buffer, &cursor);
  get_new_shape(&painter->shape_buffer, &cursor);
  plot_rectangle(&painter->shaders, &painter->common,
                 &painter->shape_buffer.shapes[cursor], &box,
                 &painter->window_box);

  unsigned *ptr = calloc(1, sizeof(void *));
  *ptr = cursor;

  struct node *node;
  make_ui_node(ptr, true, &mc_draw_wrapper, &mc_free_wrapper, 9,
               MARKS_MENU_CURSOR, &node);
  closure->draw_queue->head = emplace_node(closure->draw_queue->head, node);
}
