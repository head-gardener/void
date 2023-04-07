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
  if (init_table(&window->painter, &window->table, 0, 0)) {
    free_painter(&window->painter);
    free(window);
    return 0;
  }
  struct size sizes[] = {{100, 150}, {200, 100}, {100, 100},
                         {100, 100}, {100, 100}, {100, 100}};
  generate_table_layout(&window->painter, &window->table, 2, 3, sizes, 100,
                        200);
  render_table(&window->painter, &window->table, 2, 3);

  return window;
}

void free_void_window(struct void_window *window) {
  free_painter(&window->painter);
  free(window);
}
