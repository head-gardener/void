#include "draw_queue.h"
#include "shapes.h"
#include <stdbool.h>
#define GLEW_STATIC

#include "macros.h"
#include "painter.h"
#include "state.h"
#include "voidgui.h"
#include "window.h"
#include <SDL2/SDL.h>

#define PROJECT_NAME "voidgui"
#define VOID_SANER

struct void_window *void_gui_init(void) {
  setbuf(stdout, NULL);

  SDL_Init(SDL_INIT_EVERYTHING);

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

  /* SDL_Window *window = SDL_CreateWindow( */
  /*     "Void", 100, 100, 800, 600, */
  /*     SDL_WINDOW_RESIZABLE | SDL_WINDOW_OPENGL); */
  SDL_Window *window =
      SDL_CreateWindow("Void", 100, 100, 800, 600, SDL_WINDOW_OPENGL);

  SDL_GLContext context = SDL_GL_CreateContext(window);
  SDL_GL_MakeCurrent(window, context);

  int w;
  int h;
  SDL_GetWindowSize(window, &w, &h);
  struct void_window *v_window = init_void_window(w, h);
  if (!v_window) {
    printf("Unable to initialize window\n");
    return 0;
  }
  v_window->hw_window = window;
  v_window->context = context;

  print_gl_error;

  return v_window;
}

#define _catch_text_event(text, type)                                          \
  {                                                                            \
    struct text_event event = {text, type};                                    \
    catch (window->state, &window->state->text_input_sink, &event);            \
  }

void handle_text_event(struct void_window *window, SDL_Scancode code) {
  switch (code) {
  case (SDL_SCANCODE_RETURN):
    _catch_text_event(0, TEXT_EVENT_COMMIT);
    break;
  case (SDL_SCANCODE_ESCAPE):
    _catch_text_event(0, TEXT_EVENT_CANCEL);
    break;
  case (SDL_SCANCODE_LEFT):
    _catch_text_event(0, TEXT_EVENT_CURSOR_LEFT);
    break;
  case (SDL_SCANCODE_RIGHT):
    _catch_text_event(0, TEXT_EVENT_CURSOR_RIGHT);
    break;
  case (SDL_SCANCODE_BACKSPACE):
    _catch_text_event(0, TEXT_EVENT_BACKSPACE);
    break;
  case (SDL_SCANCODE_DELETE):
    _catch_text_event(0, TEXT_EVENT_DELETE);
    break;
  default:;
  }
}

int handle_event(struct void_window *window, SDL_Event window_event) {
  switch (window_event.type) {
  case SDL_QUIT:
    return VOID_RETURN_EXIT;
  case SDL_WINDOWEVENT:
    if (window_event.window.event == SDL_WINDOWEVENT_CLOSE)
      return VOID_RETURN_EXIT;
    if (window_event.window.event == SDL_WINDOWEVENT_RESIZED) {
      int x = window_event.window.data1;
      int y = window_event.window.data2;
      window->state->painter.window_box.width = x;
      window->state->painter.window_box.height = y;
      glViewport(0, 0, x, y);
      foreach_node(window->state->queue.head, request_plotting(node));
    }
    break;
  case SDL_MOUSEBUTTONUP: {
    struct point attribs = {window_event.motion.x, window_event.motion.y};
    catch (window->state, &window->state->click_sink, &attribs);
    break;
  }
  case SDL_KEYDOWN:
    handle_text_event(window, window_event.key.keysym.scancode);
    catch (window->state, &window->state->key_sink,
           &window_event.key.keysym.scancode);
    break;
  case SDL_TEXTINPUT:
    _catch_text_event(((wchar_t *)window_event.text.text)[0], TEXT_EVENT_INPUT);
    break;
  default:;
  }

  return VOID_RETURN_CONTINUE;
}

int void_gui_exec(struct void_window *window) {
  SDL_Event window_event;
  while (true) {
    Uint32 time = SDL_GetTicks();

    clear(&window->state->painter);

    while (SDL_PollEvent(&window_event)) {
      int code = handle_event(window, window_event);
      if (code != VOID_RETURN_CONTINUE)
        return code;
    }

    foreach_node(window->state->queue.head,
                 display_ui_node(window->state, node));
    print_gl_error;

    SDL_GL_SwapWindow(window->hw_window);

    int rest = 18 - (SDL_GetTicks() - time);
    if (rest > 0)
      SDL_Delay(rest);
  }

  return VOID_RETURN_CONTINUE;
}

int void_gui_finish(struct void_window *window) {
  SDL_GL_DeleteContext(window->context);
  SDL_Quit();
  free_void_window(window);
  return 0;
}
