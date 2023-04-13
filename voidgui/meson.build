project(
  'voidgui',
  'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

sdl = dependency('sdl2', version : '>=2.26.0')
glew = dependency('glew', version : '>=2.2.0')
pangocairo = dependency('pangocairo')

deps = [
  sdl,
  glew,
  pangocairo,
]

sources = files(
  'src/voidgui.c',
  'src/store.c',

  'src/widgets/menu.c',
  'src/widgets/table.c',
  'src/widgets/toolbar.c',
  'src/widgets/spreadsheet.c',
  'src/widgets/window.c',

  'src/render/painter.c',
  'src/render/shaders.c',
  'src/render/shapes.c',
  'src/render/shape_buffer.c',

  'src/logic/click_sink.c',
  'src/logic/draw_queue.c',
  'src/logic/sink.c',
)

include = [
  'include',
  'include/widgets',
  'include/render',
  'include/logic',
]

subdir('src/render/shaders')

lib = library(
  'voidgui',
  sources,
  include_directories: include,
  install : true,
  dependencies : deps
)
