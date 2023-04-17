#include "table.h"
#include "macros.h"
#include "window.h"

int init_table(struct painter *painter, struct table *table,
               int initial_capacity) {
  unsigned int ind;

  get_new_shape(&painter->shape_buffer, &ind);
  table->bg = ind;
  get_new_shape(&painter->shape_buffer, &ind);
  table->grid = ind;

  failure_condition(!(init_array(struct box, table->layout, initial_capacity)));
  failure_condition(
      !(init_array(shape_ptr, table->textures, initial_capacity)));
  failure_condition(!(init_array(float, table->row_ratios, initial_capacity)));
  failure_condition(
      !(init_array(float, table->column_ratios, initial_capacity)));
  table->size = 0;
  table->capacity = initial_capacity;

  table->origin_pos = TABLE_ORIGIN_TOP_LEFT;

  return 0;

failed:
  if (table->textures)
    free(table->textures);

  free_shape(&painter->shape_buffer.shapes[table->bg]);
  free_shape(&painter->shape_buffer.shapes[table->grid]);
  return 2;
}

int sync_table(struct painter *painter, struct table *table, int rows,
               int columns, struct size *sizes, unsigned char **surfaces) {
  if (table->row_ratios)
    free(table->row_ratios);
  if (table->column_ratios)
    free(table->column_ratios);
  if (table->row_height)
    free(table->row_height);
  if (table->column_width)
    free(table->column_width);
  failure_condition(!(init_array(float, table->row_ratios, rows)));
  failure_condition(!(init_array(float, table->column_ratios, columns)));
  failure_condition(!(init_array(int, table->row_height, rows)));
  failure_condition(!(init_array(int, table->column_width, columns)));

  int height = 0;
  int width = 0;

  int max_row_height = 0;
  for (int i = 0, j = 0; j < rows; i++) {
    if (sizes[i].height > max_row_height)
      max_row_height = sizes[i].height;

    if (i % columns == columns - 1) {
      max_row_height += 2 * table->vert_padding;
      height += max_row_height;
      table->row_height[j++] = max_row_height;
      max_row_height = 0;
    }
  }

  int max_column_width = 0;
  for (int i = 0, j = 0; j < columns;) {
    if (sizes[i].width > max_column_width)
      max_column_width = sizes[i].width;

    i += columns;
    if (i >= rows * columns) {
      max_column_width += 2 * table->horz_padding;
      width += max_column_width;
      table->column_width[j++] = max_column_width;
      max_column_width = 0;
      i = i % columns + 1;
    }
  }

  for (int i = 0; i < rows; i++)
    table->row_ratios[i] = (float)table->row_height[i] / height;
  for (int i = 0; i < columns; i++)
    table->column_ratios[i] = (float)table->column_width[i] / width;

  for (int i = 0; i < table->size; i++) {
    failure_condition(
        bind_texture(&painter->shape_buffer.shapes[table->textures[i]],
                     sizes[i].width, sizes[i].height, surfaces[i]));
  }

  table->box.width = width;
  table->box.height = height;

  return 0;

failed:
  return 2;
}

int plot_table(struct painter *painter, struct table *table, struct size *sizes,
               int x, int y, int rows, int columns) {
  int adjusted_x = x;
  int adjusted_y = y;
  switch (table->origin_pos) {
  case TABLE_ORIGIN_TOP_LEFT:
    break;
  case TABLE_ORIGIN_TOP_RIGHT:
    adjusted_x -= table->box.width;
    break;
  case TABLE_ORIGIN_BOTTOM_LEFT:
    adjusted_y -= table->box.height;
    break;
  case TABLE_ORIGIN_BOTTOM_RIGHT:
    adjusted_y -= table->box.height;
    adjusted_x -= table->box.width;
    break;
  case TABLE_ORIGIN_CENTER:
    adjusted_y -= table->box.height / 2;
    adjusted_x -= table->box.width / 2;
    break;
  }

  int x_offset = adjusted_x;
  int y_offset = adjusted_y;
  for (int i = 0; i < rows * columns; i++) {
    table->layout[i].x = x_offset;
    table->layout[i].y = y_offset;
    table->layout[i].height = table->row_height[i / columns];
    table->layout[i].width = table->column_width[i % columns];

    x_offset += table->column_width[i % columns];
    if (i % columns == columns - 1) {
      y_offset += table->row_height[i / columns];
      x_offset -= table->box.width;
    }
  }

  table->box.x = adjusted_x;
  table->box.y = adjusted_y;

  failure_condition(plot_rectangle(&painter->shaders, &painter->common,
                                   &painter->shape_buffer.shapes[table->bg],
                                   &table->box, &painter->window_box));
  failure_condition(plot_grid(&painter->shaders,
                              &painter->shape_buffer.shapes[table->grid],
                              &table->box, rows, columns, table->row_ratios,
                              table->column_ratios, &painter->window_box));

  for (int i = 0; i < table->size; i++) {
    struct box box = {table->layout[i].x + table->horz_padding,
                      table->layout[i].y + table->vert_padding, sizes[i].width,
                      sizes[i].height};

    failure_condition(
        plot_texture(&painter->shaders, &painter->common,
                     &painter->shape_buffer.shapes[table->textures[i]], &box,
                     &painter->window_box));
  }

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

  prepare_texture(painter);
  for (int i = 0; i < table->size; i++)
    draw_texture(painter, table->textures[i]);

  return 0;
}

int table_grow(struct painter *painter, struct table *table, int n) {
  while (table->size + n >= table->capacity) {
    array_expand(struct box, table->layout, table->capacity, 2, goto failed);
    array_expand(shape_ptr, table->textures, table->capacity, 2, goto failed);
    array_expand(struct box, table->layout, table->capacity, 2, goto failed);

    table->capacity *= 2;
  }

  for (int i = 0; i < n; i++) {
    failure_condition(
        get_new_shape(&painter->shape_buffer, &table->textures[table->size]));
    table->size += 1;
  }

  return 0;

failed:
  return 2;
}

void table_shrink(struct painter *painter, struct table *table, int n) {
  for (int i = 0; i < n; i++)
    free_texture_shape(
        &painter->shape_buffer.shapes[table->textures[table->size - i - 1]]);

  table->size -= n;
}

void free_table(struct painter *painter, struct table *table) {
  free_shape(&painter->shape_buffer.shapes[table->grid]);
  free_shape(&painter->shape_buffer.shapes[table->bg]);

  for (int i = 0; i < table->size; i++)
    free_texture_shape(
        &painter->shape_buffer.shapes[table->textures[table->size - i - 1]]);

  free(table->column_ratios);
  free(table->row_ratios);
  free(table->layout);
  free(table->textures);
}
