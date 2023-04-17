#ifndef VOID_GUI_WINDOW
#define VOID_GUI_WINDOW

#include "click_sink.h"
#include "draw_queue.h"
#include "key_sink.h"
#include "menu.h"
#include "spreadsheet.h"
#include "text_input_sink.h"
#include "toolbar.h"
#include <SDL2/SDL.h>

struct state;

struct void_window {
  SDL_Window *hw_window;
  SDL_GLContext context;

  struct spreadsheet ssheet;
  struct menu toolbar;

  struct state *state;
};

struct void_window *init_void_window(int width, int height);
void free_void_window(struct void_window *window);

#endif
