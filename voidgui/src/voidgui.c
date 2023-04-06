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

  glGenVertexArrays(1, &v_window->painter->vao);
  glGenVertexArrays(1, &v_window->painter->vao1);
  struct void_box box;
  box.x = 50;
  box.y = 100;
  box.width = 500;
  box.height = 400;
  unsigned int ind;
  get_new_shape(&v_window->painter->shape_buffer, &ind);
  make_rectangle(&v_window->painter->shaders, &v_window->painter->common,
                 &v_window->painter->shape_buffer.shapes[ind], &box,
                 &v_window->painter->window_box);
  get_new_shape(&v_window->painter->shape_buffer, &ind);
  float row_ratio[] = {0.5f, 0.5f, 0.5f, 0.5f};
  float column_ratio[] = {0.5f, 0.5f, 0.5f, 0.5f};
  make_grid(&v_window->painter->shaders,
            &v_window->painter->shape_buffer.shapes[ind], &box, 2, 2, row_ratio,
            column_ratio, &v_window->painter->window_box);
  get_new_shape(&v_window->painter->shape_buffer, &ind);
  box.x = 50;
  box.y = 100;
  box.width = 250;
  box.height = 200;
  make_texture(&v_window->painter->shaders, &v_window->painter->common,
               &v_window->painter->shape_buffer.shapes[ind], &box,
               &v_window->painter->window_box);
  print_gl_error;

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

    float color[] = {0.7f, 0.7f, 0.7f, 1.0};
    float color1[] = {0.4f, 0.4f, 0.4f, 1.0};

    prepare_rectangle(window->painter);
    draw_rectangle(window->painter, 0, color);

    prepare_grid(window->painter);
    draw_grid(window->painter, 1, color1);

    /* prepare_texture(window->painter); */
    /* draw_texture(window->painter, 0); */

    prepare_text(window->painter);
    draw_texture(window->painter, 2);
    print_gl_error;

    SDL_GL_SwapWindow(window->hw_window);
  }
}

int void_gui_finish(struct void_window *window) {
  SDL_GL_DeleteContext(window->context);
  SDL_Quit();
  free_void_window(window);
  return 0;
}
