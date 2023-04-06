#include "shaders.h"
#include "macros.h"
#include "common_frag_src.h"
#include "common_vert_src.h"
#include "tex_frag_src.h"
#include "tex_vert_src.h"
#include <stdio.h>
#include <stdlib.h>

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

int init_shaders(struct shaders *shaders) {
  shaders->common.prog = 0;
  shaders->tex.prog = 0;

  GLuint prog = link_program(common_vert_src, common_frag_src);
  fail_condition(!prog);
  shaders->common.prog = prog;
  shaders->common.posAttrib = glGetAttribLocation(prog, "pos");
  shaders->common.color = glGetUniformLocation(prog, "color");

  prog = link_program(tex_vert_src, tex_frag_src);
  fail_condition(!prog);
  shaders->tex.prog = prog;
  shaders->tex.texAttrib = glGetAttribLocation(prog, "texcoord");
  shaders->tex.posAttrib = glGetAttribLocation(prog, "pos");
  /* glEnableVertexAttribArray(shaders->tex.posAttrib); */
  return 0;

failed:
  if (shaders->common.prog)
    glDeleteProgram(shaders->common.prog);
  if (shaders->tex.prog)
    glDeleteProgram(shaders->tex.prog);
  return 1;
}
