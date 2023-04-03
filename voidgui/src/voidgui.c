#define GLEW_STATIC

#include "voidgui.h"
#include "painter.h"
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

  return v_window;
}

int void_gui_exec(struct void_window *window) {
  SDL_Event windowEvent;
  while (1) {
    if (SDL_PollEvent(&windowEvent)) {
      if (windowEvent.type == SDL_QUIT) {
        return 0;
      }
    }

    clear(window->painter);

    struct void_box box;
    box.x = 55;
    box.y = 105;
    box.width = 490;
    box.height = 390;

    /* prepare_rectangle(window->painter); */
    /* draw_rectangle(window->painter, &box); */

    box.x = 50;
    box.y = 100;
    box.width = 500;
    box.height = 400;
    float vert_ratio[] = {0.5f, 0.5f, 0.5f, 0.5f};
    float horz_ratio[] = {0.5f, 0.5f, 0.5f, 0.5f};

    prepare_grid(window->painter);
    draw_grid(window->painter, &box, 2, 2, vert_ratio, horz_ratio);

    /* prepare_text(window->painter); */
    /* draw_text(window->painter); */

    SDL_GL_SwapWindow(window->hw_window);
  }
}

int void_gui_finish(struct void_window *window) {
  SDL_GL_DeleteContext(window->context);
  SDL_Quit();
  free_void_window(window);
  return 0;
}
