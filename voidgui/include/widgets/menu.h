#ifndef VOID_GUI_MENU
#define VOID_GUI_MENU

#include "click_sink.h"
#include "painter.h"
#include "table.h"
#include <stdbool.h>
#include <wchar.h>

typedef void(menu_extension)(void *, struct painter *, wchar_t **,
                             struct table *);

struct menu {
  struct table table;
  void *closure;
  menu_extension *onsync;
  bool free_closure;
};

int init_menu(struct painter *painter, int capacity, int x, int y,
              struct menu *menu, enum origin_position origin_pos);
int sync_menu(struct painter *painter, wchar_t **labels, int rows, int columns,
              struct menu *menu);
int draw_menu(struct painter *painter, struct menu *menu);
void free_menu(struct painter *painter, struct menu *menu);

// extensions
struct cursor_extension_closure {
  wchar_t **cursor;
  struct list *draw_queue;
};

void draw_menu_cursor(struct cursor_extension_closure *, struct painter *,
                      wchar_t **, struct table *);

#endif
