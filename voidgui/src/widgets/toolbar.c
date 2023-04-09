#include "toolbar.h"
#include "macros.h"
#include <stdlib.h>

int init_toolbar(struct painter *painter, struct toolbar *toolbar) {
  failure_condition(
      init_table(painter, &toolbar->table, TOOLBAR_LABEL_COUNT, 0, 0));
  failure_condition(table_grow(painter, &toolbar->table, TOOLBAR_LABEL_COUNT));

  toolbar->table.horz_padding = 10;
  toolbar->table.vert_padding = 10;

  return 0;

failed:
  return 1;
}

int sync_toolbar(struct painter *painter, struct toolbar *toolbar) {
  char *label_text[] = TOOLBAR_LABELS;
  struct size sizes[TOOLBAR_LABEL_COUNT];
  unsigned char *surfaces[TOOLBAR_LABEL_COUNT];

  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++)
    failure_condition(render_text(label_text[i], &sizes[i].width,
                                  &sizes[i].height, &surfaces[i]));

  failure_condition(
      plot_table_with_sizes(&toolbar->table, 1, TOOLBAR_LABEL_COUNT, sizes));
  failure_condition(sync_table(painter, &toolbar->table, surfaces, sizes, 1,
                               TOOLBAR_LABEL_COUNT));

  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++)
    free(surfaces[i]);

  return 0;

failed:
  for (int i = 0; i < TOOLBAR_LABEL_COUNT; i++)
    if (surfaces[i])
      free(surfaces[i]);

  return 1;
}

int draw_toolbar(struct painter *painter, struct toolbar *toolbar) {
  draw_table(painter, &toolbar->table);

  return 0;
}

void free_toolbar(struct painter *painter, struct toolbar *toolbar) {
  free_table(painter, &toolbar->table);
}
