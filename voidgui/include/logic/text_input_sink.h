#ifndef VOID_GUI_TEXT_INPUT_SINK
#define VOID_GUI_TEXT_INPUT_SINK

#include "sink.h"
#include <wchar.h>

enum text_event_type {
  TEXT_EVENT_INPUT,
  TEXT_EVENT_CURSOR_LEFT,
  TEXT_EVENT_CURSOR_RIGHT,
  TEXT_EVENT_BACKSPACE,
  TEXT_EVENT_DELETE,
  TEXT_EVENT_COMMIT,
  TEXT_EVENT_CANCEL,
};

struct text_event {
  wchar_t text;
  enum text_event_type type;
};

struct text_funnel_specs {
  wchar_t *cursor;
  wchar_t *text;
  funnel_callback oncommit;
  funnel_callback oncancel;
};

int init_text_input_sink(struct sink *sink);

#endif
