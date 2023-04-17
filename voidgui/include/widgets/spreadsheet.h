#ifndef VOID_GUI_SPREADSHEET
#define VOID_GUI_SPREADSHEET

#include "array.h"
#include "click_sink.h"
#include "painter.h"
#include "shape_buffer.h"
#include "table.h"
#include "text_input_sink.h"
#include <stdbool.h>

#ifndef SPREADSHEET_INNITIAL_CAPACITY
#define SPREADSHEET_INNITIAL_CAPACITY 10
#endif

#define DATA_FIELD_COUNT 2

struct state;

struct spreadsheet {
  struct table table;

  array(wchar_t *, data);
  array(struct size, sizes);
  array(bool, dirty);
  int size;
  int capacity;

  struct point origin;
};

int init_spreadsheet(struct state *state, struct spreadsheet *ssheet, int x,
                     int y);
int sync_spreadsheet(struct state *state, struct spreadsheet *ssheet);

void plot_spreadsheet(struct state *state, struct spreadsheet *ssheet);
void draw_spreadsheet(struct state *state, struct spreadsheet *ssheet);
void free_spreadsheet(struct state *state, struct spreadsheet *ssheet);

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    wchar_t *name, wchar_t *phone);

#endif
