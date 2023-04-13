#ifndef VOID_GUI_TOOLBAR
#define VOID_GUI_TOOLBAR

#include "click_sink.h"
#include "menu.h"
#include "painter.h"
#include "table.h"

#define TOOLBAR_OPTION_COUNT 4
#define TOOLBAR_OPTIONS                                                        \
  { "Table", "Edit", "Tools", "About" }

int init_toolbar(struct painter *painter, struct menu *toolbar);
int sync_toolbar(struct painter *painter, struct click_sink *sink,
                 struct store *store, struct menu *toolbar);
int draw_toolbar(struct painter *painter, struct menu *toolbar);

#endif
