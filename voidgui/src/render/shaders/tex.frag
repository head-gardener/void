#version 320 es

precision mediump float;

in vec2 Texcoord;

out vec4 out_color;

uniform sampler2D tex;

void main() {
  out_color = texture(tex, Texcoord);
}
