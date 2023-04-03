#include "window.h"
#include "painter.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window = calloc(1, sizeof(struct void_window));
  window->painter = init_painter(width, height);

  if (!window->painter) {
    printf("Unable to initialize painter\n");
    free(window);
    return 0;
  }

  return window;
}

void free_void_window(struct void_window *window) {
  free_painter(window->painter);
  free(window);
}
