project(
  'voidgui',
  'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

sdl = dependency('sdl2', version : '>=2.26.0')
glew = dependency('glew', version : '>=2.2.0')

deps = [
  sdl,
  glew,
]

sources = files(
  'src/voidgui.c',

  'src/widgets/table.c',
  'src/widgets/window.c',

  'src/render/painter.c',
)

include = [
  'include', 
  'include/widgets',
  'include/render',
]

subdir('src/render/shaders')

lib = library(
  'voidgui',
  sources,
  include_directories: include,
  install : true,
  dependencies : deps
)
