#ifndef VOID_GUI_MENU
#define VOID_GUI_MENU

#include "click_sink.h"
#include "draw_queue.h"
#include "painter.h"
#include "table.h"
#include <stdbool.h>
#include <wchar.h>

#define MENU_PLACEHOLDER_TEXT L"..."

typedef void menu_extension(void *, struct state *, struct table *);

struct sticky_point {
  int *x;
  int *y;
};

/**
 * A table wrapper.
 */
struct menu {
  struct table table;
  void *closure;
  menu_extension *onplot;
  bool free_closure;

  struct sticky_point origin;

  int rows;
  int columns;
  array(struct size, sizes);
  array(unsigned char *, surfaces);
};

int init_menu(struct painter *painter, int *x, int *y, int rows, int columns,
              struct menu *menu, enum origin_position origin_pos);
/**
 * Draw all surfaces, sync and plot the table. Set `labels[i]` to `null` to
 * prevent rerendering this label.
 */
int sync_menu(struct state *state, wchar_t **labels, struct menu *menu);

// resize_menu ?

void plot_menu(struct state *state, struct menu *menu);
void draw_menu(struct state *state, struct menu *menu);
void free_menu(struct state *state, struct menu *menu);

// extensions
struct cursor_shape {
  shape_ptr shape_ptr;
  struct box box;
};

struct cursor_extension_closure {
  wchar_t **cursor;
  struct list *draw_queue;
};

void draw_menu_cursor(struct cursor_extension_closure *, struct state *,
                      struct table *);

#endif
