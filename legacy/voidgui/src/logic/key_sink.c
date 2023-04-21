#include "key_sink.h"

funnel_callback check_key_funnel(struct funnel *funnel,
                                 key_funnel_specs *attribs) {
  key_funnel_specs *specs = funnel->specs;

  return *attribs == *specs ? funnel->callback : 0;
}

int init_key_sink(struct sink *sink) {
  return init_sink(sink,
                   (funnel_callback(*)(struct funnel * funnel, void *attribs)) &
                       check_key_funnel);
}
