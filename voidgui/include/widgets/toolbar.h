#ifndef VOID_GUI_TOOLBAR
#define VOID_GUI_TOOLBAR

#include "click_sink.h"
#include "painter.h"
#include "table.h"

#define TOOLBAR_LABEL_COUNT 4
#define TOOLBAR_LABELS {"Table", "Edit", "Tools", "About"}

struct toolbar {
  struct table table;

  shape_ptr labels[TOOLBAR_LABEL_COUNT];
};

int init_toolbar(struct painter *painter, struct toolbar *toolbar);
int upload_toolbar(struct painter *painter, struct toolbar *toolbar);
int draw_toolbar(struct painter *painter, struct toolbar *toolbar);
void free_toolbar(struct painter *painter, struct toolbar *toolbar);

#endif
