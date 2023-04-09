#include "toolbar.h"
#include "macros.h"
#include <stdlib.h>


int init_toolbar(struct painter *painter, struct menu *toolbar) {
  return init_menu(painter, TOOLBAR_OPTION_COUNT, 0, 0, toolbar);
}

int sync_toolbar(struct painter *painter, struct click_sink *sink,
                 struct menu *toolbar) {
  char *label_text[] = TOOLBAR_OPTIONS;

  int code = sync_menu(painter, label_text, 1, TOOLBAR_OPTION_COUNT, toolbar);
  if (code)
    return code;

  return 0;
}
