#ifndef VOID_GUI
#define VOID_GUI

#include "window.h"
#include <SDL2/SDL.h>

struct void_window *void_gui_init(int time);
int void_gui_exec(struct void_window *window);
int void_gui_finish(struct void_window *window);

#endif
