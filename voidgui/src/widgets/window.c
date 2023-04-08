#include "window.h"
#include "macros.h"
#include "painter.h"
#include "table.h"

struct void_window *init_void_window(int width, int height) {
  struct void_window *window;
  int ssheet_code = 1;
  int sink_code = 1;
  int painter_code = 1;

  verbose_fail_condition(!(window = calloc(1, sizeof(struct void_window))),
                         "Allocation error at %s:%i\n", __FILE__, __LINE__);
  verbose_fail_condition(
      (painter_code = init_painter(width, height, &window->painter)),
      "Unable to initialize painter. Code: %i\n", painter_code);
  verbose_fail_condition(
      (ssheet_code =
           init_spreadsheet(&window->painter, &window->ssheet, 20, 20)),
      "Unable to initialize spreadsheet. Code: %i\n", ssheet_code);
  verbose_fail_condition((sink_code = init_click_sink(&window->sink)),
                         "Unable to initialize click sink. Code: %i\n",
                         sink_code);

  struct data data1 = {strdup("IGOR"), strdup("158")};
  struct data data2 = {strdup("VASYA"), strdup("623")};
  struct data data3 = {strdup("Charlie"), strdup("293")};
  struct data data4 = {strdup("Ch"), strdup("2931240")};
  spreadsheet_put(&window->painter, &window->ssheet, &data1);
  spreadsheet_put(&window->painter, &window->ssheet, &data2);
  spreadsheet_put(&window->painter, &window->ssheet, &data3);
  spreadsheet_put(&window->painter, &window->ssheet, &data4);
  upload_spreadsheet(&window->painter, &window->ssheet);

  return window;

failed:
  if (!sink_code)
    free_click_sink(&window->sink);
  if (!ssheet_code)
    free_spreadsheet(&window->painter, &window->ssheet);
  if (!painter_code)
    free_painter(&window->painter);
  if (window)
    free(window);

  return 0;
}

void free_void_window(struct void_window *window) {
  free_click_sink(&window->sink);
  free_spreadsheet(&window->painter, &window->ssheet);
  free_painter(&window->painter);
  free(window);
}
