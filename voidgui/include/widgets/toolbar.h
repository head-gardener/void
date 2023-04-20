#ifndef VOID_GUI_TOOLBAR
#define VOID_GUI_TOOLBAR

#include "click_sink.h"
#include "menu.h"
#include "painter.h"
#include "table.h"

#define TOOLBAR_OPTION_COUNT 4
#define TOOLBAR_OPTIONS                                                        \
  { L"Table", L"Edit", L"Tools", L"About" }
#define TOOLBAR_TABLE_OPTIONS                                                  \
  { L"Pull", L"Push", L"Restore", L"Server..." }

struct store;

int init_toolbar(struct painter *painter, struct menu *toolbar);
void sync_toolbar(struct state *state, struct menu *toolbar);
void plot_toolbar(struct state *state, struct menu *toolbar);

#endif
