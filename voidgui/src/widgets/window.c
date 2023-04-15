#include "window.h"
#include "consts.h"
#include "macros.h"
#include "painter.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window;
  int ssheet_code = 1;
  int click_sink_code = 1;
  int ti_sink_code = 1;
  int store_code = 1;
  int painter_code = 1;
  int toolbar_code = 1;

  verbose_failure_condition(!(window = calloc(1, sizeof(struct void_window))),
                            "Allocation error at %s:%i\n", __FILE__, __LINE__);

  verbose_failure_condition(
      (painter_code = init_painter(width, height, &window->painter)),
      "Unable to initialize painter. Code: %i\n", painter_code);
  init_draw_queue(&window->painter, &window->draw_queue);
  verbose_failure_condition(
      (click_sink_code = init_click_sink(&window->click_sink)),
      "Unable to initialize click sink. Code: %i\n", click_sink_code);
  verbose_failure_condition(
      (ti_sink_code = init_text_input_sink(&window->text_input_sink)),
      "Unable to initialize text input sink. Code: %i\n", ti_sink_code);
  verbose_failure_condition((store_code = init_store(&window->store, 10)),
                            "Unable to initialize global store. Code: %i\n",
                            click_sink_code);
  verbose_failure_condition(
      (ssheet_code = init_spreadsheet(&window->painter, &window->store,
                                      &window->ssheet, 20, 20)),
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
  sync_spreadsheet(&window->painter, &window->click_sink,
                   &window->text_input_sink, &window->ssheet);

  sync_toolbar(&window->painter, &window->click_sink, &window->store,
               &window->toolbar);

  struct node *node;
  make_ui_node(&window->ssheet, false,
               (int (*)(struct painter *, void *)) & draw_spreadsheet,
               (void (*)(struct painter *, void *)) & free_spreadsheet, 0,
               MARKS_SPREADSHEET, &node);
  window->draw_queue.head = emplace_node(window->draw_queue.head, node);

  make_ui_node(&window->toolbar, false,
               (int (*)(struct painter *, void *)) & draw_menu,
               (void (*)(struct painter *, void *)) & free_menu, 0,
               MARKS_TOOLBAR, &node);
  window->draw_queue.head = emplace_node(window->draw_queue.head, node);

  return window;

failed:
  if (!toolbar_code)
    free_menu(&window->painter, &window->toolbar);
  if (!ti_sink_code)
    free_sink(&window->text_input_sink);
  if (!click_sink_code)
    free_sink(&window->click_sink);
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
  free_list(&window->draw_queue);
  free_sink(&window->text_input_sink);
  free_sink(&window->click_sink);
  free_painter(&window->painter);
  free(window);
}
