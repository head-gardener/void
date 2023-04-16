#ifndef VOID_GUI_SPREADSHEET
#define VOID_GUI_SPREADSHEET

#include "array.h"
#include "click_sink.h"
#include "painter.h"
#include "shape_buffer.h"
#include "table.h"
#include "text_input_sink.h"
#include <stdbool.h>

// PERF: better storage scheme?
#ifndef SPREADSHEET_INNITIAL_CAPACITY
#define SPREADSHEET_INNITIAL_CAPACITY 10
#endif

#define DATA_FIELD_COUNT 2

struct data {
  char *name;
  char *phone;
};

struct spreadsheet {
  struct table table;

  array(wchar_t *, data);
  array(bool, dirty);
  int size;
  int capacity;
};

int init_spreadsheet(struct painter *painter, struct store *store,
                     struct spreadsheet *ssheet, int x, int y);
int sync_spreadsheet(struct painter *painter, struct sink *click_sink,
                     struct sink *text_input_sink, struct spreadsheet *ssheet);
int draw_spreadsheet(struct painter *painter, struct spreadsheet *ssheet);
void free_spreadsheet(struct painter *painter, struct spreadsheet *ssheet);

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    wchar_t *name, wchar_t *phone);

#endif
