#ifndef VOID_GUI_ARRAY
#define VOID_GUI_ARRAY

#define array(type, name) type *name

#define init_array(type, array, capacity) array = calloc(capacity, sizeof(type))

#define array_expand(type, array, new_capacity, on_error)                      \
  {                                                                            \
    type *buf = calloc(new_capacity * 2, sizeof(type));                        \
    if (!buf)                                                                  \
      on_error;                                                                \
    memcpy(buf, array, sizeof(type) * new_capacity);                           \
    free(array);                                                               \
    array = buf;                                                               \
  }

#endif
