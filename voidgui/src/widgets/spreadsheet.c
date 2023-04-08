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

int render_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  // TODO: generate the sizes
  struct size sizes[] = {{100, 150}, {200, 100}, {100, 100}, {100, 100},
                         {100, 100}, {100, 100}, {100, 100}, {100, 100}};

  generate_table_layout(painter, &ssheet->table, ssheet->size,
                        DATA_FIELD_COUNT, sizes, ssheet->pole.x,
                        ssheet->pole.y);
  render_table(painter, &ssheet->table, ssheet->size, DATA_FIELD_COUNT);

  for (int i = 0; i < ssheet->size; i++) {
    // PERF: is this efficient?
    if (ssheet->dirty[i] & DATA_DIRTY_NAME) {
      shape_ptr tex = ssheet->labels[i * DATA_FIELD_COUNT];
      fail_condition(make_texture(&painter->shaders, &painter->common,
                                  &painter->shape_buffer.shapes[tex],
                                  &ssheet->table.layout[i * DATA_FIELD_COUNT],
                                  &painter->window_box));
      fail_condition(render_text(&painter->shape_buffer.shapes[tex],
                                 ssheet->data[i].name));
    }
    if (ssheet->dirty[i] & DATA_DIRTY_PHONE) {
      shape_ptr tex = ssheet->labels[i * DATA_FIELD_COUNT + 1];
      fail_condition(
          make_texture(&painter->shaders, &painter->common,
                       &painter->shape_buffer.shapes[tex],
                       &ssheet->table.layout[i * DATA_FIELD_COUNT + 1],
                       &painter->window_box));
      fail_condition(render_text(&painter->shape_buffer.shapes[tex],
                                 ssheet->data[i].phone));
    }
    ssheet->dirty[i] = 0;
  }

  return 0;

failed:
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
