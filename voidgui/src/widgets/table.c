#include "table.h"
#include "macros.h"
#include "window.h"

int init_table(struct painter *painter, struct table *table) {
  unsigned int ind;

  get_new_shape(&painter->shape_buffer, &ind);
  table->bg = ind;

  get_new_shape(&painter->shape_buffer, &ind);
  table->grid = ind;

  return 0;
}

void generate_table_layout(struct table *table, int rows, int columns,
                           struct size *sizes, int x, int y, int vert_padding,
                           int horz_padding) {
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
  int *row_height = calloc(rows, sizeof(int));
  int width = 0;
  int *column_width = calloc(columns, sizeof(int));

  int max_row_height = 0;
  for (int i = 0, j = 0; j < rows; i++) {
    if (sizes[i].height > max_row_height)
      max_row_height = sizes[i].height;

    if (i % columns == columns - 1) {
      max_row_height += 2 * vert_padding;
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
      max_column_width += 2 * horz_padding;
      width += max_column_width;
      column_width[j++] = max_column_width;
      max_column_width = 0;
      i = i % columns + 1;
    }
  }

  int x_offset = x;
  int y_offset = y;
  for (int i = 0; i < rows * columns; i++) {
    table->layout[i].x = x_offset;
    table->layout[i].y = y_offset;
    table->layout[i].height = row_height[i / columns];
    table->layout[i].width = column_width[i % columns];

    x_offset += column_width[i % columns];
    if (i % columns == columns - 1) {
      y_offset += row_height[i / columns];
      x_offset -= width;
    }
  }

  for (int i = 0; i < rows; i++)
    table->row_ratios[i] = (float)row_height[i] / height;
  for (int i = 0; i < columns; i++)
    table->column_ratios[i] = (float)column_width[i] / width;

  table->box.x = x;
  table->box.y = y;
  table->box.width = width;
  table->box.height = height;
}

int upload_table(struct painter *painter, struct table *table, int rows,
                 int columns) {
  fail_condition(make_rectangle(&painter->shaders, &painter->common,
                                &painter->shape_buffer.shapes[table->bg],
                                &table->box, &painter->window_box));
  fail_condition(make_grid(&painter->shaders,
                           &painter->shape_buffer.shapes[table->grid],
                           &table->box, rows, columns, table->row_ratios,
                           table->column_ratios, &painter->window_box));

  return 0;

failed:
  return 3;
}

int draw_table(struct painter *painter, struct table *table) {
  float color[] = {0.7f, 0.7f, 0.7f, 1.0};
  float color1[] = {0.4f, 0.4f, 0.4f, 1.0};

  prepare_rectangle(painter);
  draw_rectangle(painter, table->bg, color);

  prepare_grid(painter);
  draw_grid(painter, table->grid, color1);

  return 0;
}

void free_table(struct painter *painter, struct table *table) {
  free_shape(&painter->shape_buffer.shapes[table->grid]);
  free_shape(&painter->shape_buffer.shapes[table->bg]);

  free(table->column_ratios);
  free(table->row_ratios);
  free(table->layout);
}
