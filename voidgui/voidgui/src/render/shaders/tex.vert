#version 320 es

in vec2 texcoord;
in vec2 pos;

out vec2 Texcoord;

void main() {
  Texcoord = texcoord;
  gl_Position = vec4(pos, 0.0, 1.0);
}
