#ifndef VOID_GUI_TABLE
#define VOID_GUI_TABLE

#include "painter.h"
#include "shape_buffer.h"

/**
 * This structure assists with forming a layout out of textures and rendering
 * it.
 */
struct table {
  shape_ptr grid;
  shape_ptr bg;

  array(struct box, layout);
  array(shape_ptr, textures);
  int size;
  int capacity;

  struct box box;
  struct point origin;
  array(float, row_ratios);
  array(float, column_ratios);

  int vert_padding;
  int horz_padding;
};

int init_table(struct painter *painter, struct table *table,
               int initial_capacity, int x, int y);
/**
 * Fill `table->layout`, ratios and `table->box` according to `sizes`. The later
 * should store `rows * columns` of `struct sizes` for table contents.
 */
int plot_table_with_sizes(struct table *table, int rows, int columns,
                          struct size *sizes);
/**
 * Generate and upload GL buffers necessary for drawing a grid according to
 * `table->layout` and `table->box`, sync all the textures sourcing the from
 * `surfaces`
 */
int sync_table(struct painter *painter, struct table *table,
               unsigned char **surfaces, struct size *sizes, int rows,
               int columns);
/**
 * Pull enough `shape_ptr` to accomodate `n` new textures.
 */
int table_grow(struct painter *painter, struct table *table, int n);
/**
 * Free `n` top textures.
 */
void table_shrink(struct painter *painter, struct table *table, int n);
int draw_table(struct painter *painter, struct table *table);
void free_table(struct painter *painter, struct table *table);

#endif
