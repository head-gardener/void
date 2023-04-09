#ifndef VOID_GUI_WINDOW
#define VOID_GUI_WINDOW

#include "click_sink.h"
#include "spreadsheet.h"
#include "toolbar.h"
#include <SDL2/SDL.h>

struct void_window {
  SDL_Window *hw_window;
  SDL_GLContext context;

  struct spreadsheet ssheet;
  struct toolbar toolbar;

  struct painter painter;
  struct click_sink sink;
};

struct void_window *init_void_window(int width, int height);
void free_void_window(struct void_window *window);

#endif
