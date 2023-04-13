#include "window.h"
#include "macros.h"
#include "painter.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window;
  int ssheet_code = 1;
  int sink_code = 1;
  int store_code = 1;
  int painter_code = 1;
  int toolbar_code = 1;

  verbose_failure_condition(!(window = calloc(1, sizeof(struct void_window))),
                            "Allocation error at %s:%i\n", __FILE__, __LINE__);

  window->queue = calloc(1, sizeof(void *));

  verbose_failure_condition(
      (painter_code = init_painter(width, height, &window->painter)),
      "Unable to initialize painter. Code: %i\n", painter_code);
  verbose_failure_condition((sink_code = init_click_sink(&window->sink)),
                            "Unable to initialize click sink. Code: %i\n",
                            sink_code);
  verbose_failure_condition((store_code = init_store(&window->store, 10)),
                            "Unable to initialize global store. Code: %i\n",
                            sink_code);
  verbose_failure_condition(
      (ssheet_code =
           init_spreadsheet(&window->painter, &window->ssheet, 20, 20)),
      "Unable to initialize spreadsheet. Code: %i\n", ssheet_code);
  verbose_failure_condition(
      (toolbar_code = init_toolbar(&window->painter, &window->toolbar)),
      "Unable to initialize toolbar. Code: %i\n", toolbar_code);

  struct data data1 = {strdup("IGOR"), strdup("158")};
  struct data data2 = {strdup("VASYA"), strdup("623")};
  struct data data3 = {strdup("Charlie"), strdup("293")};
  struct data data4 = {strdup("Ch"), strdup("2931240")};
  spreadsheet_put(&window->painter, &window->ssheet, &data1);
  spreadsheet_put(&window->painter, &window->ssheet, &data2);
  spreadsheet_put(&window->painter, &window->ssheet, &data3);
  spreadsheet_put(&window->painter, &window->ssheet, &data4);
  sync_spreadsheet(&window->painter, &window->ssheet);

  sync_toolbar(&window->painter, &window->sink, &window->store,
               &window->toolbar);

  struct ui_node *node;
  make_node(
      &window->ssheet, (int (*)(struct painter *, void *)) & draw_spreadsheet,
      (void (*)(struct painter *, void *)) & free_spreadsheet, 0, 1, &node);
  *window->queue = emplace_node(*window->queue, node);

  make_node(&window->toolbar, (int (*)(struct painter *, void *)) & draw_menu,
            (void (*)(struct painter *, void *)) & free_menu, 0, 1, &node);
  *window->queue = emplace_node(*window->queue, node);

  return window;

failed:
  if (!toolbar_code)
    free_menu(&window->painter, &window->toolbar);
  if (!sink_code)
    free_click_sink(&window->sink);
  if (!store_code)
    free_store(&window->store);
  if (!ssheet_code)
    free_spreadsheet(&window->painter, &window->ssheet);
  if (!painter_code)
    free_painter(&window->painter);
  if (window)
    free(window);

  return 0;
}

void free_void_window(struct void_window *window) {
  foreach_node(*window->queue, free_node(&window->painter, node));
  free(window->queue);
  free_click_sink(&window->sink);
  free_painter(&window->painter);
  free(window);
}
