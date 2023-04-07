#include "table.h"
#include "macros.h"
#include "window.h"

int init_table(struct painter *painter, struct table *table, int x, int y) {
  table->box.x = x;
  table->box.y = y;

  unsigned int ind;
  get_new_shape(&painter->shape_buffer, &ind);
  table->bg = ind;

  get_new_shape(&painter->shape_buffer, &ind);
  table->grid = ind;

  return 0;
}

// FIXME: table->layout much?
/**
 * Fills `table->layout`, ratios and `table->box` according to
 * `x`, `y` and `sizes`. The later should store `rows * columns` of
 * `struct sizes` for table contents.
 */
void generate_table_layout(struct painter *painter, struct table *table,
                           int rows, int columns, struct size *sizes, int x,
                           int y) {
  if (table->layout)
    free(table->layout);
  if (table->row_ratios)
    free(table->row_ratios);
  if (table->column_ratios)
    free(table->column_ratios);
  table->row_ratios = calloc(rows, sizeof(float));
  table->column_ratios = calloc(columns, sizeof(float));
  table->layout = calloc(rows * columns, sizeof(struct box));

  int height = 0;
  // reversed
  int *row_height = calloc(rows, sizeof(int));
  int width = 0;
  int *column_width = calloc(columns, sizeof(int));

  // move in reverse order because drawing in GL is bottom to top
  int max_row_height = 0;
  for (int i = rows * columns - 1, j = 0; j < rows; i--) {
    if (sizes[i].height > max_row_height)
      max_row_height = sizes[i].height;

    if (i % columns == 0) {
      height += max_row_height;
      row_height[j++] = max_row_height;
      max_row_height = 0;
    }
  }

  int max_column_width = 0;
  for (int i = 0, j = 0; j < columns;) {
    if (sizes[i].width > max_column_width)
      max_column_width = sizes[i].width;

    i += columns;
    if (i >= rows * columns) {
      width += max_column_width;
      column_width[j++] = max_column_width;
      max_column_width = 0;
      i = i % columns + 1;
    }
  }

  int x_offset = x;
  int y_offset = y + height - row_height[rows - 1];
  for (int i = 0; i < rows * columns; i++) {
    table->layout[i].x = x_offset;
    table->layout[i].y = y_offset;
    table->layout[i].height = row_height[rows - 1 - i / columns];
    table->layout[i].width = column_width[i % columns];
    printf("%i %i %i %i\n", table->layout[i].x, table->layout[i].y,
           table->layout[i].width, table->layout[i].height);

    x_offset += column_width[i % columns];
    if (i % columns == columns - 1) {
      y_offset -= row_height[i / columns];
      x_offset = x;
    }
  }

  printf("%i %i\n", slice2(row_height));
  printf("%i %i %i\n", slice3(column_width));
  for (int i = 0; i < rows; i++)
    table->row_ratios[i] = (float)row_height[i] / height;
  for (int i = 0; i < columns; i++)
    table->column_ratios[i] = (float)column_width[i] / width;

  table->box.x = x;
  table->box.y = y;
  table->box.width = width;
  table->box.height = height;
}

/**
 * Generates GL buffers necessary for drawing a grid according to
 * `table->layout` and `table->box`.
 */
int render_table(struct painter *painter, struct table *table, int rows,
                 int columns) {
  make_rectangle(&painter->shaders, &painter->common,
                 &painter->shape_buffer.shapes[table->bg], &table->box,
                 &painter->window_box);
  make_grid(&painter->shaders, &painter->shape_buffer.shapes[table->grid],
            &table->box, rows, columns, table->row_ratios, table->column_ratios,
            &painter->window_box);

  return 0;
}

int draw_table(struct painter *painter, struct table *table) {
  float color[] = {0.7f, 0.7f, 0.7f, 1.0};
  float color1[] = {0.4f, 0.4f, 0.4f, 1.0};

  prepare_rectangle(painter);
  draw_rectangle(painter, table->bg, color);

  prepare_grid(painter);
  draw_grid(painter, table->grid, color1);

  prepare_texture(painter);

  return 0;
}

void free_table(struct painter *painter, struct table *table) {
  free_shape(&painter->shape_buffer.shapes[table->grid]);
  free_shape(&painter->shape_buffer.shapes[table->bg]);

  free(table->column_ratios);
  free(table->row_ratios);
  free(table->layout);
}
