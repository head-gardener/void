#ifndef VOID_GUI_TABLE
#define VOID_GUI_TABLE

#include "painter.h"
#include "shape_buffer.h"

/**
 * Given a bunch of textures this file allow generating a layout for putting
 * them in a table and a grid to serve as a background.
 */
struct table {
  shape_ptr grid;
  shape_ptr bg;

  struct box *layout;

  struct box box;

  float *row_ratios;
  float *column_ratios;
};

int init_table(struct painter *painter, struct table *table);
/**
 * Fill `table->layout`, ratios and `table->box` according to
 * `x`, `y` and `sizes`. The later should store `rows * columns` of
 * `struct sizes` for table contents.
 */
void generate_table_layout(struct painter *painter, struct table *table,
                           int rows, int columns, struct size *sizes, int x,
                           int y);
/**
 * Generate GL buffers necessary for drawing a grid according to
 * `table->layout` and `table->box`.
 */
int render_table(struct painter *painter, struct table *table, int rows,
                 int columns);
/**
 * Draw a grid with a background according to the layout.
 */
int draw_table(struct painter *painter, struct table *table);
void free_table(struct painter *painter, struct table *table);

#endif
