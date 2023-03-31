#include "painter.h"
#include "common_frag_src.h"
#include "common_vert_src.h"
#include "window.h"
#include <GLES3/gl3.h>
#include <SDL2/SDL.h>

static GLuint compile_shader(GLuint type, const GLchar *src) {
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, 1, &src, NULL);
  glCompileShader(shader);

  GLint ok;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &ok);
  if (ok == GL_FALSE) {
    GLint log_len;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_len);
    char *log = calloc(log_len, sizeof(char));
    glGetShaderInfoLog(shader, log_len, NULL, log);

    printf("Failed to compile shader\n %s\n", log);
    glDeleteShader(shader);
    shader = 0;
  }

  return shader;
}

static GLuint link_program(const GLchar *vert_src, const GLchar *frag_src) {
  GLuint vert = compile_shader(GL_VERTEX_SHADER, vert_src);
  if (!vert) {
    goto error;
  }

  GLuint frag = compile_shader(GL_FRAGMENT_SHADER, frag_src);
  if (!frag) {
    glDeleteShader(vert);
    goto error;
  }

  GLuint prog = glCreateProgram();
  glAttachShader(prog, vert);
  glAttachShader(prog, frag);
  glLinkProgram(prog);

  glDetachShader(prog, vert);
  glDetachShader(prog, frag);
  glDeleteShader(vert);
  glDeleteShader(frag);

  GLint ok;
  glGetProgramiv(prog, GL_LINK_STATUS, &ok);
  if (ok == GL_FALSE) {
    ;
    printf("Failed to link shader\n");
    glDeleteProgram(prog);
    goto error;
  }

  return prog;

error:
  return 0;
}

struct painter *init_painter() {
  struct painter *painter = calloc(1, sizeof(struct painter));

  GLuint common = link_program(common_vert_src, common_frag_src);
  if (!common) {
    free(painter);
    return 0;
  }
  painter->shaders.common = common;

  float vertices[] = {
      0.0f,  0.5f,  // Vertex 1 (X, Y)
      0.5f,  -0.5f, // Vertex 2 (X, Y)
      -0.5f, -0.5f  // Vertex 3 (X, Y)
  };

  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);

  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glBindFragDataLocation(painter->shaders.common, 0, "out_color");
  glUseProgram(painter->shaders.common);
  GLint posAttrib = glGetAttribLocation(painter->shaders.common, "pos");
  glVertexAttribPointer(posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(posAttrib);

  return painter;
}

void draw(struct painter *painter) { glDrawArrays(GL_TRIANGLES, 0, 3); }

void free_painter(struct painter *painter) {
  glDeleteProgram(painter->shaders.common);
  free(painter);
}
