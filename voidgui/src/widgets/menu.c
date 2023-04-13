#include "menu.h"
#include "macros.h"
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

int sync_menu(struct painter *painter, char **labels, int rows, int columns,
              struct menu *menu) {
  struct size sizes[rows * columns];
  unsigned char *surfaces[rows * columns];

  for (int i = 0; i < rows * columns; i++)
    failure_condition(render_text(labels[i], &sizes[i].width, &sizes[i].height,
                                  &surfaces[i]));

  failure_condition(plot_table_with_sizes(&menu->table, rows, columns, sizes));
  failure_condition(
      sync_table(painter, &menu->table, surfaces, sizes, rows, columns));

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
  free_table(painter, &menu->table);
}
