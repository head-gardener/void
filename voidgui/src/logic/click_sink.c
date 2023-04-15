#include "click_sink.h"

funnel_callback check_click_funnel(struct funnel *funnel,
                                   struct point *attribs) {
  struct click_funnel_specs *specs = funnel->specs;
  int dist_x = attribs->x - specs->box.x;
  int dist_y = attribs->y - specs->box.y;
  if ((!specs->inverted && ((dist_x >= 0 && dist_x <= specs->box.width) &&
                            (dist_y >= 0 && dist_y <= specs->box.height))) ||
      (specs->inverted && ((dist_x < 0 || dist_x > specs->box.width) &&
                           (dist_y < 0 || dist_y > specs->box.height))))
    return funnel->callback;

  return 0;
}

int init_click_sink(struct sink *sink) {
  return init_sink(sink,
                   (funnel_callback(*)(struct funnel * funnel, void *attribs)) &
                       check_click_funnel);
}
