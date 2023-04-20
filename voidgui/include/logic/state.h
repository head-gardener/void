#ifndef VOID_GUI_STATE
#define VOID_GUI_STATE

#include "draw_queue.h"
#include "painter.h"
#include "sink.h"
#include "store.h"

struct state {
  struct painter painter;
  struct list queue;
  struct store store;

  struct sink click_sink;
  struct sink text_input_sink;
  struct sink key_sink;

  int return_code;
};

#endif
