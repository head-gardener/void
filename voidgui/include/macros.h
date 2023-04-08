#ifndef VOID_GUI_MACROS
#define VOID_GUI_MACROS

#define max(a, b) a > b ? a : b

#define fail_condition(condition)                                              \
  if (condition) {                                                             \
    goto failed;                                                               \
  }

#define fail_condition_propagate(condition)                                    \
  code = condition;                                                            \
  if (code) {                                                                  \
    goto failed;                                                               \
  }

#define fail_condition_with_code(condition, _code)                             \
  if (condition) {                                                             \
    code = _code;                                                              \
    goto failed;                                                               \
  }

#define verbose_fail_condition(condition, message, ...)                        \
  if (condition) {                                                             \
    printf(message, ##__VA_ARGS__);                                            \
    goto failed;                                                               \
  }

#define slice2(a) (a)[0], (a)[1]
#define slice3(a) (a)[0], (a)[1], (a)[2]
#define slice4(a) (a)[0], (a)[1], (a)[2], (a)[3]
#define slice6(a) (a)[0], (a)[1], (a)[2], (a)[3], (a)[4], (a)[5]

#define split4(type, a, b, c, d, arr)                                          \
  type a = arr[0];                                                             \
  type b = arr[1];                                                             \
  type c = arr[2];                                                             \
  type d = arr[3];

#define print_gl_error                                                         \
  {                                                                            \
    int error = glGetError();                                                  \
    if (error)                                                                 \
      printf("%s:%d in %s caught error %i\n", __FILE__, __LINE__, __func__,    \
             error);                                                           \
  }

#endif
