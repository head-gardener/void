#ifndef VOID_GUI
#define VOID_GUI

#include "window.h"
#include <SDL2/SDL.h>

enum void_return_codes {
  VOID_RETURN_CONTINUE = 0,
  VOID_RETURN_EXIT,
};

struct void_window *void_gui_init(void);
int void_gui_exec(struct void_window *window);
int void_gui_finish(struct void_window *window);

#endif
