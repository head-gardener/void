#include "shapes.h"
#define GLEW_STATIC

#include "macros.h"
#include "painter.h"
#include "voidgui.h"
#include "window.h"
#include <SDL2/SDL.h>

#define PROJECT_NAME "voidgui"
#define VOID_SANER

struct void_window *void_gui_init(int time) {
  SDL_Init(SDL_INIT_EVERYTHING);

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

  SDL_Window *window =
      SDL_CreateWindow("Void", 100, 100, 800, 600, SDL_WINDOW_OPENGL);

  SDL_GLContext context = SDL_GL_CreateContext(window);
  SDL_GL_MakeCurrent(window, context);

  struct void_window *v_window = init_void_window(800, 600);
  if (!v_window) {
    printf("Unable to initialize window\n");
    return 0;
  }
  v_window->hw_window = window;
  v_window->context = context;

  print_gl_error;

  return v_window;
}

int void_gui_exec(struct void_window *window) {
  SDL_Event window_event;
  while (SDL_PollEvent(&window_event)) {

    clear(&window->painter);
    switch (window_event.type) {
    case SDL_QUIT:
      return 0;
    case SDL_MOUSEBUTTONUP:
      catch_click(&window->painter, &window->sink, window_event.motion.x,
                  window_event.motion.y);
    }

    draw_spreadsheet(&window->painter, &window->ssheet);
    draw_toolbar(&window->painter, &window->toolbar);
    print_gl_error;

    SDL_GL_SwapWindow(window->hw_window);
  }

  return 1;
}

int void_gui_finish(struct void_window *window) {
  SDL_GL_DeleteContext(window->context);
  SDL_Quit();
  free_void_window(window);
  return 0;
}
