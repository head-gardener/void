#include "spreadsheet.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>

int init_spreadsheet(struct painter *painter, struct spreadsheet *ssheet, int x,
                     int y) {
  int code = 0;

  failure_condition_with_code(
      init_table(painter, &ssheet->table,
                 SPREADSHEET_INNITIAL_CAPACITY * DATA_FIELD_COUNT, x, y),
      1);
  failure_condition_with_code(
      !(init_array(struct data, ssheet->data, SPREADSHEET_INNITIAL_CAPACITY)),
      2);
  failure_condition_with_code(
      !(init_array(int_fast8_t, ssheet->dirty, SPREADSHEET_INNITIAL_CAPACITY)),
      2);

  ssheet->size = 0;
  ssheet->capacity = SPREADSHEET_INNITIAL_CAPACITY;

  ssheet->table.horz_padding = 10;
  ssheet->table.vert_padding = 10;

  return 0;

failed:
  free_table(painter, &ssheet->table);
  if (ssheet->data)
    free(ssheet->data);
  if (ssheet->dirty)
    free(ssheet->dirty);

  return code;
}

int sync_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  struct size *sizes;
  unsigned char **surfaces;
  int label_count = ssheet->size * DATA_FIELD_COUNT;

  failure_condition(!(sizes = calloc(label_count, sizeof(struct size))));
  failure_condition(!(surfaces = calloc(label_count, sizeof(void *))));

  for (int i = 0, j = 0; j < ssheet->size; i += DATA_FIELD_COUNT, j++) {
    if (ssheet->dirty[j] & DATA_DIRTY_NAME)
      failure_condition(render_text(ssheet->data[j].name, &sizes[i].width,
                                    &sizes[i].height, &surfaces[i]));
    if (ssheet->dirty[j] & DATA_DIRTY_PHONE)
      failure_condition(render_text(ssheet->data[j].phone, &sizes[i + 1].width,
                                    &sizes[i + 1].height, &surfaces[i + 1]));

    ssheet->dirty[j] = 0;
  }

  plot_table_with_sizes(&ssheet->table, ssheet->size, DATA_FIELD_COUNT, sizes);
  failure_condition(sync_table(painter, &ssheet->table, surfaces, sizes,
                               ssheet->size, DATA_FIELD_COUNT));

  for (int i = 0; i < label_count; i++)
    free(surfaces[i]);

  free(sizes);
  free(surfaces);

  return 0;

failed:
  // TODO: clean up
  return 1;
}

int draw_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  draw_table(painter, &ssheet->table);

  return 0;
}

void free_spreadsheet(struct painter *painter, struct spreadsheet *ssheet) {
  free_table(painter, &ssheet->table);

  for (int i = 0; i < ssheet->size; i++) {
    free(ssheet->data[i].name);
    free(ssheet->data[i].phone);
  }
  free(ssheet->data);
  free(ssheet->dirty);
}

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    struct data *data) {
  if (ssheet->size >= ssheet->capacity) {
    array_expand(struct data, ssheet->data, ssheet->capacity, 2, goto failed);
    array_expand(int_fast8_t, ssheet->dirty, ssheet->capacity, 2, goto failed);

    ssheet->capacity *= 2;
  }

  failure_condition(table_grow(painter, &ssheet->table, 2));

  memcpy(&ssheet->data[ssheet->size], data, sizeof(struct data));
  ssheet->dirty[ssheet->size] = DATA_DIRTY_PHONE | DATA_DIRTY_NAME;

  ssheet->size++;

  return 0;

failed:
  return 2;
}
