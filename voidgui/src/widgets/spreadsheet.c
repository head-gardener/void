#include "spreadsheet.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>

int init_spreadsheet(struct painter *painter, struct spreadsheet *ssheet, int x,
                     int y) {
  int code = 0;

  fail_condition_with_code(init_table(painter, &ssheet->table), 1);

  fail_condition_with_code(
      !(init_array(shape_ptr, ssheet->labels,
                   DATA_FIELD_COUNT * SPREADSHEET_INNITIAL_CAPACITY)),
      2);
  fail_condition_with_code(
      !(init_array(int_fast8_t, ssheet->dirty, SPREADSHEET_INNITIAL_CAPACITY)),
      2);
  fail_condition_with_code(
      !(init_array(struct data, ssheet->data, SPREADSHEET_INNITIAL_CAPACITY)),
      2);

  ssheet->size = 0;
  ssheet->capacity = SPREADSHEET_INNITIAL_CAPACITY;
  ssheet->pole.x = x;
  ssheet->pole.y = y;

  return code;

failed:
  free_table(painter, &ssheet->table);
  if (ssheet->labels)
    free(ssheet->labels);
  if (ssheet->data)
    free(ssheet->data);
  if (ssheet->dirty)
    free(ssheet->dirty);

  return code;
}

int upload_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  struct size *sizes;
  unsigned char **surfaces;
  int label_count = ssheet->size * DATA_FIELD_COUNT;

  fail_condition(!(sizes = calloc(label_count, sizeof(struct size))));
  fail_condition(!(surfaces = calloc(label_count, sizeof(void *))));

  for (int i = 0, j = 0; j < ssheet->size; i += DATA_FIELD_COUNT, j++) {
    if (ssheet->dirty[j] & DATA_DIRTY_NAME)
      fail_condition(render_text(ssheet->data[j].name, &sizes[i].width,
                                 &sizes[i].height, &surfaces[i]));
    if (ssheet->dirty[j] & DATA_DIRTY_PHONE)
      fail_condition(render_text(ssheet->data[j].phone, &sizes[i + 1].width,
                                 &sizes[i + 1].height, &surfaces[i + 1]));
  }

  generate_table_layout(&ssheet->table, ssheet->size, DATA_FIELD_COUNT,
                        sizes, ssheet->pole.x, ssheet->pole.y, 10, 10);
  fail_condition(
      upload_table(painter, &ssheet->table, ssheet->size, DATA_FIELD_COUNT));

  for (int i = 0, j = 0; j < ssheet->size; i += DATA_FIELD_COUNT, j++) {
    // PERF: is this efficient?
    if (ssheet->dirty[j] & DATA_DIRTY_NAME) {
      struct box box = {ssheet->table.layout[i].x, ssheet->table.layout[i].y,
                        sizes[i].width, sizes[i].height};
      shape_ptr tex = ssheet->labels[i];

      fail_condition(make_texture(&painter->shaders, &painter->common,
                                  &painter->shape_buffer.shapes[tex], &box,
                                  &painter->window_box));
      fail_condition(upload_text(&painter->shape_buffer.shapes[tex], box.width,
                                 box.height, surfaces[i]));
    }
    if (ssheet->dirty[j] & DATA_DIRTY_PHONE) {
      struct box box = {ssheet->table.layout[i + 1].x,
                        ssheet->table.layout[i + 1].y, sizes[i + 1].width,
                        sizes[i + 1].height};
      shape_ptr tex = ssheet->labels[i + 1];

      fail_condition(make_texture(&painter->shaders, &painter->common,
                                  &painter->shape_buffer.shapes[tex], &box,
                                  &painter->window_box));
      fail_condition(upload_text(&painter->shape_buffer.shapes[tex], box.width,
                                 box.height, surfaces[i + 1]));
    }

    free(surfaces[i]);
    free(surfaces[i + 1]);
    ssheet->dirty[j] = 0;
  }

  free(sizes);
  free(surfaces);

  return 0;

failed:
  // TODO: clean up
  return 1;
}

int draw_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  draw_table(painter, &ssheet->table);

  prepare_texture(painter);
  for (int i = 0; i < DATA_FIELD_COUNT * ssheet->size; i++) {
    draw_texture(painter, ssheet->labels[i]);
  }

  return 0;
}

void free_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  free_table(painter, &ssheet->table);

  for (int i = 0; i < DATA_FIELD_COUNT * ssheet->size; i++) {
    free_texture_shape(&painter->shape_buffer.shapes[ssheet->labels[i]]);
  }
  for (int i = 0; i < ssheet->size; i++) {
    free(ssheet->data[i].name);
    free(ssheet->data[i].phone);
  }
  free(ssheet->labels);
  free(ssheet->data);
  free(ssheet->dirty);
}

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    struct data *data) {
  if (ssheet->size >= ssheet->capacity) {
    array_expand(struct data, ssheet->data, ssheet->capacity * 2, goto failed);
    array_expand(int_fast8_t, ssheet->dirty, ssheet->capacity * 2, goto failed);
    array_expand(shape_ptr, ssheet->labels,
                 ssheet->capacity * 2 * DATA_FIELD_COUNT, goto failed);

    ssheet->capacity *= 2;
  }

  memcpy(&ssheet->data[ssheet->size], data, sizeof(struct data));
  ssheet->dirty[ssheet->size] = DATA_DIRTY_PHONE | DATA_DIRTY_NAME;
  fail_condition(
      get_new_shape(&painter->shape_buffer,
                    &ssheet->labels[ssheet->size * DATA_FIELD_COUNT]));
  fail_condition(
      get_new_shape(&painter->shape_buffer,
                    &ssheet->labels[ssheet->size * DATA_FIELD_COUNT + 1]));

  ssheet->size++;

  return 0;

failed:
  return 2;
}
