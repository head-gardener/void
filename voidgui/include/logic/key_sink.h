#ifndef VOID_GUI_KEY_SINK
#define VOID_GUI_KEY_SINK

#include "SDL_scancode.h"
#include "sink.h"

typedef SDL_Scancode key_funnel_specs;

int init_key_sink(struct sink *sink);

#endif
