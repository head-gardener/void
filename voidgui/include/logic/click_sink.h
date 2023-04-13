#ifndef VOID_GUI_CLICK_SINK
#define VOID_GUI_CLICK_SINK

#include "sink.h"
#include "shapes.h"

struct click_funnel_specs {
  struct box box;
  bool inverted;
};

int init_click_sink(struct sink *sink);

#endif
