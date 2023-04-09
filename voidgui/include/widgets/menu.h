#ifndef VOID_GUI_MENU
#define VOID_GUI_MENU

#include "click_sink.h"
#include "painter.h"
#include "table.h"

struct menu {
  struct table table;
};

int init_menu(struct painter *painter, int capacity, int x, int y,
              struct menu *menu);
int sync_menu(struct painter *painter, char **labels, int rows,
              int columns, struct menu *menu);
int draw_menu(struct painter *painter, struct menu *menu);
void free_menu(struct painter *painter, struct menu *menu);

#endif
