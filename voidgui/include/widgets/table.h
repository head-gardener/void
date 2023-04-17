#ifndef VOID_GUI_TABLE
#define VOID_GUI_TABLE

#include "painter.h"
#include "shape_buffer.h"

enum origin_position {
  TABLE_ORIGIN_TOP_LEFT,
  TABLE_ORIGIN_TOP_RIGHT,
  TABLE_ORIGIN_BOTTOM_LEFT,
  TABLE_ORIGIN_BOTTOM_RIGHT,
  TABLE_ORIGIN_CENTER,
};

/**
 * This structure assists with forming a layout out of textures and rendering
 * it.
 */
struct table {
  shape_ptr grid;
  shape_ptr bg;

  // textures
  array(struct box, layout);
  array(shape_ptr, textures);
  int size;
  int capacity;

  // position
  enum origin_position origin_pos;
  struct box box;

  // dimensions
  array(float, row_ratios);
  array(float, column_ratios);
  array(int, row_height);
  array(int, column_width);

  int vert_padding;
  int horz_padding;
};

int init_table(struct painter *painter, struct table *table,
               int initial_capacity);
/**
 * Recalculate dimensions and bind textures.
 */
int sync_table(struct painter *painter, struct table *table, int rows,
               int columns, struct size *sizes, unsigned char **surfaces);
/**
 * Generate and map layout to verticies, used for drawing.
 */
int plot_table(struct painter *painter, struct table *table, struct size *sizes,
               int x, int y, int rows, int columns);
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
