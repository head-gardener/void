#include "text_input_sink.h"
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

void buffered_wcscpy(wchar_t *dst, wchar_t *src) {
  /* int len = wcslen(src); */
  wchar_t *buf = calloc(128, sizeof(wchar_t));
  wcscpy(buf, src);
  wcscpy(dst, buf);
  free(buf);
}

funnel_callback check_text_input_funnel(struct funnel *funnel,
                                        struct text_event *event) {
  struct text_funnel_specs *specs = funnel->specs;
  switch (event->type) {
  case TEXT_EVENT_COMMIT:
    return specs->oncommit;
  case TEXT_EVENT_CANCEL:
    return specs->oncancel;
  case TEXT_EVENT_INPUT:
    if (*specs->cursor)
      buffered_wcscpy(specs->cursor + 1, specs->cursor);
    *specs->cursor++ = event->text;
    break;
  case TEXT_EVENT_CURSOR_LEFT:
    if (specs->cursor > specs->text)
      specs->cursor--;
    break;
  case TEXT_EVENT_CURSOR_RIGHT:
    if (*specs->cursor)
      specs->cursor++;
    break;
  case TEXT_EVENT_BACKSPACE:
    if (specs->cursor > specs->text) {
      buffered_wcscpy(specs->cursor - 1, specs->cursor);
      specs->cursor--;
    }
    break;
  case TEXT_EVENT_DELETE:
    buffered_wcscpy(specs->cursor, specs->cursor + 1);
    break;
  }

  return funnel->callback;
}

int init_text_input_sink(struct sink *sink) {
  return init_sink(sink,
                   (funnel_callback(*)(struct funnel * funnel, void *attribs)) &
                       check_text_input_funnel);
}
