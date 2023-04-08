#ifndef VOID_GUI_SPREADSHEET
#define VOID_GUI_SPREADSHEET

#include "array.h"
#include "painter.h"
#include "shape_buffer.h"
#include "table.h"
#include <stdbool.h>

// PERF: better storage scheme?
#ifndef SPREADSHEET_INNITIAL_CAPACITY
#define SPREADSHEET_INNITIAL_CAPACITY 10
#endif

#define DATA_DIRTY_NAME 1
#define DATA_DIRTY_PHONE 2

#define DATA_FIELD_COUNT 2

struct data {
  char *name;
  char *phone;
};

struct spreadsheet {
  struct table table;
  struct point pole;

  array(shape_ptr, labels);
  array(int_fast8_t, dirty);
  array(struct data, data);
  int size;
  int capacity;
};

int init_spreadsheet(struct painter *painter, struct spreadsheet *ssheet, int x,
                     int y);
int render_spreadsheet(struct painter *painter, struct spreadsheet *ssheet);
int draw_spreadsheet(struct painter *painter, struct spreadsheet *ssheet);
void free_spreadsheet(struct painter *painter, struct spreadsheet *ssheet);

int spreadsheet_put(struct painter *painter, struct spreadsheet *ssheet,
                    struct data *data);
/* void spreadsheet_put_at(struct spreadsheet *ssheet, struct data *data, int
 * pos); */

#endif
