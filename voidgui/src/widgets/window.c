#include "window.h"
#include "painter.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window = calloc(1, sizeof(struct void_window));

  if (init_painter(width, height, &window->painter)) {
    printf("Unable to initialize painter\n");
    free(window);
    return 0;
  }
  int code;
  if ((code = init_spreadsheet(&window->painter, &window->ssheet, 20, 20))) {
    printf("Unable to initialize spreadsheet. Code: %i\n", code);
    free_painter(&window->painter);
    free(window);
    return 0;
  }

  struct data data;
  data.name = strdup("Igor");
  data.phone = strdup("158");
  spreadsheet_put(&window->painter, &window->ssheet, &data);
  data.name = strdup("Vasya");
  data.phone = strdup("231");
  spreadsheet_put(&window->painter, &window->ssheet, &data);
  render_spreadsheet(&window->painter, &window->ssheet);
  render_spreadsheet(&window->painter, &window->ssheet);

  return window;
}

void free_void_window(struct void_window *window) {
  free_spreadsheet(&window->painter, &window->ssheet);
  free_painter(&window->painter);
  free(window);
}
