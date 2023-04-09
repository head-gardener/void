#include "toolbar.h"
#include "macros.h"
#include <stdlib.h>

int init_toolbar(struct painter *painter, struct toolbar *toolbar) {
  init_table(painter, &toolbar->table);
  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++) {
    fail_condition(get_new_shape(&painter->shape_buffer, &toolbar->labels[i]));
  }

  return 0;

failed:
  return 1;
}

int upload_toolbar(struct painter *painter, struct toolbar *toolbar) {
  char *label_text[] = TOOLBAR_LABELS;
  struct size sizes[TOOLBAR_LABEL_COUNT];
  unsigned char *surfaces[TOOLBAR_LABEL_COUNT];

  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++) {
    fail_condition(render_text(label_text[i], &sizes[i].width, &sizes[i].height,
                               &surfaces[i]));
  }
  generate_table_layout(&toolbar->table, 1, TOOLBAR_LABEL_COUNT, sizes, 0, 0,
                        10, 10);
  upload_table(painter, &toolbar->table, 1, TOOLBAR_LABEL_COUNT);

  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++) {
    struct box box = {toolbar->table.layout[i].x + 10,
                      toolbar->table.layout[i].y + 10, sizes[i].width,
                      sizes[i].height};

    fail_condition(
        make_texture(&painter->shaders, &painter->common,
                     &painter->shape_buffer.shapes[toolbar->labels[i]], &box,
                     &painter->window_box));
    fail_condition(
        upload_text(&painter->shape_buffer.shapes[toolbar->labels[i]],
                    box.width, box.height, surfaces[i]));
    free(surfaces[i]);
  }

  return 0;

failed:
  // TODO: clean up?
  return 1;
}

int draw_toolbar(struct painter *painter, struct toolbar *toolbar) {
  draw_table(painter, &toolbar->table);

  prepare_texture(painter);
  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++) {
    draw_texture(painter, toolbar->labels[i]);
  }

  return 0;
}

void free_toolbar(struct painter *painter, struct toolbar *toolbar) {
  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++) {
    free_texture_shape(&painter->shape_buffer.shapes[toolbar->labels[i]]);
  }
  free_table(painter, &toolbar->table);
}
