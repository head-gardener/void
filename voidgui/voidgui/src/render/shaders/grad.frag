#version 320 es

precision mediump float;

uniform vec4 color1;
uniform vec4 color2;
uniform vec4 constr;

out vec4 out_color;

void main() {
  float pos;
  pos = (gl_FragCoord.x - constr.x) / constr.z;
  out_color = color1 + (color2 - color1) * pos; 
}
