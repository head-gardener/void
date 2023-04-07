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

  unsigned int ind;
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[0],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "1");
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[1],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "2");
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[2],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "3");
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[3],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "4");
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[4],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "5");
  get_new_shape(&v_window->painter.shape_buffer, &ind);
  make_texture(&v_window->painter.shaders, &v_window->painter.common,
               &v_window->painter.shape_buffer.shapes[ind], &v_window->table.layout[5],
               &v_window->painter.window_box);
  render_text(&v_window->painter.shape_buffer.shapes[ind], "6");

  /*   get_new_shape(&painter->shape_buffer, &ind); */
  /*   make_texture(&painter->shaders, &painter->common, */
  /*                &painter->shape_buffer.shapes[ind], &box,
   * &painter->window_box); */
  /*   render_texture(&painter->shape_buffer.shapes[ind],
   * "voidgui/res/dog.png"); */
  /*   table->text2 = ind; */

  glGenVertexArrays(1, &v_window->painter.vao);
  glGenVertexArrays(1, &v_window->painter.vao1);
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

    clear(&window->painter);

    draw_table(&window->painter, &window->table);

    prepare_texture(&window->painter);
    draw_texture(&window->painter, 2);
    draw_texture(&window->painter, 3);
    draw_texture(&window->painter, 4);
    draw_texture(&window->painter, 5);
    draw_texture(&window->painter, 6);
    draw_texture(&window->painter, 7);
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
