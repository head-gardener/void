#ifndef VOID_GUI_MENU
#define VOID_GUI_MENU

#include "click_sink.h"
#include "painter.h"
#include "table.h"
#include <stdbool.h>

typedef void (menu_extension)(void *, struct painter *, char **,
                               struct table *);

struct menu {
  struct table table;
  void *closure;
  menu_extension *onsync;
  bool free_closure;
};

int init_menu(struct painter *painter, int capacity, int x, int y,
              struct menu *menu, enum origin_position origin_pos);
int sync_menu(struct painter *painter, char **labels, int rows, int columns,
              struct menu *menu);
int draw_menu(struct painter *painter, struct menu *menu);
void free_menu(struct painter *painter, struct menu *menu);

// extensions
struct cursor_extension_closure {
  char **cursor;
  struct list *draw_queue;
};

void draw_menu_cursor(struct cursor_extension_closure *, struct painter *,
                      char **, struct table *);

#endif
