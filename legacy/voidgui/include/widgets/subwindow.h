#ifndef VOID_GUI_SUBWINDOW
#define VOID_GUI_SUBWINDOW

#include "menu.h"
#include "state.h"

struct subwindow {
  shape_ptr bg;
  struct menu ok;
  struct menu cancel;
};

int init_subwindow(struct state *state, struct subwindow *subwindow);
int struct_subwindow(struct state *state, struct subwindow *subwindow);

void push_input_field(struct state *state, wchar_t *text,
                      struct subwindow *subwindow);

#endif
