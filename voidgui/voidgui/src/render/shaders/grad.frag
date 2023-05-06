#version 320 es

precision mediump float;

uniform vec4 color;
uniform vec4 constr;

out vec4 out_color;

void main() {
  vec2 ndcPos;
  ndcPos.xy = (2. * gl_FragCoord.xy - 2. * constr.xy) / constr.wz - 1.;

  out_color = vec4(ndcPos.xy, color.ba);
}
