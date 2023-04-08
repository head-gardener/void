#ifndef VOID_GUI_SHADERS
#define VOID_GUI_SHADERS

#include <GLES3/gl3.h>

struct shaders {
  struct {
    GLuint prog;
    GLint color;
    GLint posAttrib;
  } common;
  struct {
    GLuint prog;
    GLint posAttrib;
    GLint texAttrib;
  } tex;
};

int init_shaders(struct shaders *shaders);

#endif
