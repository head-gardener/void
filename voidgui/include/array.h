#ifndef VOID_GUI_ARRAY
#define VOID_GUI_ARRAY

#define array(type, name) type *name

#define init_array(type, array, capacity) array = calloc(capacity, sizeof(type))

#define array_expand(type, array, capacity, cap_mod, on_error)                 \
  {                                                                            \
    type *buf = calloc(capacity * cap_mod, sizeof(type));                      \
    if (!buf)                                                                  \
      on_error;                                                                \
    memcpy(buf, array, sizeof(type) * capacity * cap_mod);                     \
    free(array);                                                               \
    array = buf;                                                               \
  }

#endif
