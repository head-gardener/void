#ifndef VOID_GUI_WINDOW
#define VOID_GUI_WINDOW

#include "spreadsheet.h"
#include "table.h"
#include <SDL2/SDL.h>

struct void_window {
  SDL_Window *hw_window;
  SDL_GLContext context;
  struct painter painter;
  struct spreadsheet ssheet;
};

struct void_window *init_void_window(int width, int height);
void free_void_window(struct void_window *window);

#endif
