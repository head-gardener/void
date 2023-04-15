#include "text_input_sink.h"

funnel_callback check_text_input_funnel(struct funnel *funnel,
                                        struct text_event *event) {
  struct text_funnel_specs *specs = funnel->specs;
  switch (event->type) {
  case TEXT_EVENT_COMMIT:
    return specs->oncommit;
  case TEXT_EVENT_CANCEL:
    return specs->oncommit;
  case TEXT_EVENT_INPUT:
    **(specs->cursor) = event->text[0];
    *(specs->cursor) += sizeof(char);
    break;
  case TEXT_EVENT_CURSOR_LEFT:
    *(specs->cursor) -= sizeof(char);
    break;
  case TEXT_EVENT_CURSOR_RIGHT:
    if (*specs->cursor > specs->text)
      *(specs->cursor) += sizeof(char);
    break;
  case TEXT_EVENT_BACKSPACE:

    break;
  case TEXT_EVENT_DELETE:
    break;
  }
  return funnel->callback;
}

int init_text_input_sink(struct sink *sink) {
  return init_sink(sink,
                   (funnel_callback(*)(struct funnel * funnel, void *attribs)) &
                       check_text_input_funnel);
}
