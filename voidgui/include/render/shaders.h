#ifndef VOID_GUI_SHADERS
#define VOID_GUI_SHADERS

#include <GLES3/gl3.h>

struct shaders {
  struct {
    GLuint prog;
    GLint color;
    GLuint vbo;
    GLuint ebo;
    GLint posAttrib;
  } common;
  struct {
    GLuint prog;
    GLuint vbo;
    GLuint ebo;
    GLint posAttrib;
    GLint texAttrib;
  } tex;
};

int init_shaders(struct shaders *shaders);

#endif
