#include "window.h"
#include "consts.h"
#include "macros.h"
#include "state.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window;
  int ssheet_code = 1;
  int click_sink_code = 1;
  int key_sink_code = 1;
  int ti_sink_code = 1;
  int store_code = 1;
  int painter_code = 1;
  int toolbar_code = 1;

  verbose_failure_condition(!(window = calloc(1, sizeof(struct void_window))),
                            "Allocation error at %s:%i\n", __FILE__, __LINE__);
  verbose_failure_condition(!(window->state = calloc(1, sizeof(struct state))),
                            "Allocation error at %s:%i\n", __FILE__, __LINE__);

  verbose_failure_condition(
      (painter_code = init_painter(width, height, &window->state->painter)),
      "Unable to initialize painter. Code: %i\n", painter_code);
  init_draw_queue(&window->state->painter, &window->state->queue);
  verbose_failure_condition(
      (click_sink_code = init_click_sink(&window->state->click_sink)),
      "Unable to initialize click sink. Code: %i\n", click_sink_code);
  verbose_failure_condition(
      (click_sink_code = init_key_sink(&window->state->key_sink)),
      "Unable to initialize key sink. Code: %i\n", key_sink_code);
  verbose_failure_condition(
      (ti_sink_code = init_text_input_sink(&window->state->text_input_sink)),
      "Unable to initialize text input sink. Code: %i\n", ti_sink_code);
  verbose_failure_condition(
      (store_code = init_store(&window->state->store, 10)),
      "Unable to initialize global store. Code: %i\n", click_sink_code);
  verbose_failure_condition(
      (ssheet_code = init_spreadsheet(window->state, &window->ssheet, 20, 20)),
      "Unable to initialize spreadsheet. Code: %i\n", ssheet_code);
  verbose_failure_condition(
      (toolbar_code = init_toolbar(&window->state->painter, &window->toolbar)),
      "Unable to initialize toolbar. Code: %i\n", toolbar_code);

  spreadsheet_put(&window->state->painter, &window->ssheet, wcsdup(L"IGOR"),
                  wcsdup(L"158"));
  spreadsheet_put(&window->state->painter, &window->ssheet, wcsdup(L"VASYA"),
                  wcsdup(L"623"));
  spreadsheet_put(&window->state->painter, &window->ssheet, wcsdup(L"Charlie"),
                  wcsdup(L"293"));
  spreadsheet_put(&window->state->painter, &window->ssheet, wcsdup(L"Ch"),
                  wcsdup(L"2931240"));
  sync_spreadsheet(window->state, &window->ssheet);

  sync_toolbar(window->state, &window->toolbar);

  register_ui_node(&window->ssheet, false, (ui_callback *)&plot_spreadsheet,
                   (ui_callback *)&draw_spreadsheet,
                   (ui_callback *)&free_spreadsheet, 0, MARKS_SPREADSHEET,
                   &window->state->queue);
  register_ui_node(&window->toolbar, false, (ui_callback *)&plot_toolbar,
                   (ui_callback *)&draw_menu, (ui_callback *)&free_menu, 0,
                   MARKS_TOOLBAR, &window->state->queue);

  return window;

failed:
  if (!toolbar_code)
    free_menu(window->state, &window->toolbar);
  if (!ti_sink_code)
    free_sink(&window->state->text_input_sink);
  if (!key_sink_code)
    free_sink(&window->state->key_sink);
  if (!click_sink_code)
    free_sink(&window->state->click_sink);
  if (!store_code)
    free_store(&window->state->store);
  if (!ssheet_code)
    free_spreadsheet(window->state, &window->ssheet);
  if (!painter_code)
    free_painter(&window->state->painter);
  if (window)
    free(window);

  return 0;
}

void free_void_window(struct void_window *window) {
  free_list(&window->state->queue);
  free_sink(&window->state->text_input_sink);
  free_sink(&window->state->click_sink);
  free_sink(&window->state->key_sink);
  free_painter(&window->state->painter);
  free(window);
}
