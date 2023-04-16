#include "draw_queue.h"
#include "shapes.h"
#include <stdbool.h>
#define GLEW_STATIC

#include "macros.h"
#include "painter.h"
#include "voidgui.h"
#include "window.h"
#include <SDL2/SDL.h>

#define PROJECT_NAME "voidgui"
#define VOID_SANER

struct void_window *void_gui_init(int time) {
  setbuf(stdout, NULL);

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

int handle_event(struct void_window *window, SDL_Event window_event) {
  switch (window_event.type) {
  case SDL_QUIT:
    return 0;
  case SDL_MOUSEBUTTONUP: {
    struct point attribs = {window_event.motion.x, window_event.motion.y};
    catch (&window->painter, &window->click_sink, &window->text_input_sink,
           &window->draw_queue, &window->store, &window->click_sink, &attribs);
    break;
  }
  case SDL_KEYDOWN:
    switch (window_event.key.keysym.scancode) {
    case (SDL_SCANCODE_RETURN): {
      struct text_event event = {0, TEXT_EVENT_COMMIT};
      catch (&window->painter, &window->click_sink, &window->text_input_sink,
             &window->draw_queue, &window->store, &window->text_input_sink,
             &event);
      break;
    }
    case (SDL_SCANCODE_ESCAPE): {
      struct text_event event = {0, TEXT_EVENT_CANCEL};
      catch (&window->painter, &window->click_sink, &window->text_input_sink,
             &window->draw_queue, &window->store, &window->text_input_sink,
             &event);
      break;
    }
    case (SDL_SCANCODE_LEFT): {
      struct text_event event = {0, TEXT_EVENT_CURSOR_LEFT};
      catch (&window->painter, &window->click_sink, &window->text_input_sink,
             &window->draw_queue, &window->store, &window->text_input_sink,
             &event);
      break;
    }
    case (SDL_SCANCODE_RIGHT): {
      struct text_event event = {0, TEXT_EVENT_CURSOR_RIGHT};
      catch (&window->painter, &window->click_sink, &window->text_input_sink,
             &window->draw_queue, &window->store, &window->text_input_sink,
             &event);
      break;
    }
    default:;
    }
    break;
  case SDL_TEXTINPUT: {
    struct text_event event = {window_event.text.text, TEXT_EVENT_INPUT};
    catch (&window->painter, &window->click_sink, &window->text_input_sink,
           &window->draw_queue, &window->store, &window->text_input_sink,
           &event);
    break;
  }
  }

  return 1;
}

int void_gui_exec(struct void_window *window) {
  SDL_Event window_event;
  while (true) {
    Uint32 time = SDL_GetTicks();

    clear(&window->painter);

    while (SDL_PollEvent(&window_event)) {
      int code = handle_event(window, window_event);
      if (code != 1)
        return code;
    }

    foreach_node(window->draw_queue.head, draw_node(&window->painter, node));
    print_gl_error;

    SDL_GL_SwapWindow(window->hw_window);

    int rest = 18 - (SDL_GetTicks() - time);
    if (rest > 0)
      SDL_Delay(rest);
  }

  return 1;
}

int void_gui_finish(struct void_window *window) {
  SDL_GL_DeleteContext(window->context);
  SDL_Quit();
  free_void_window(window);
  return 0;
}
