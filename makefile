PROJECT_ROOT = $(shell pwd)
BINPATH = build/void/build/x86_64-linux/ghc-9.0.2/voidcore-0.1.0.0/x/void/build/void

C_SOURCES = $(wildcard voidgui/src/**/*.c) $(wildcard voidgui/src/*.c)
C_HEADERS = $(wildcard voidgui/include/**/*.h) $(wildcard voidgui/include/*.h)
GLSL_SOURCES = $(wildcard voidgui/src/render/shaders/*)
HASKELL_SOURCES = $(wildcard void/app/**/*.hs) $(wildcard void/app/*.hs)

run: build
	$(PROJECT_ROOT)/$(BINPATH)/void

debug: build
	gdb $(PROJECT_ROOT)/$(BINPATH)/void

build: build/voidgui build/void

build/voidgui: voidgui/meson.build $(C_SOURCES) $(C_HEADERS) $(GLSL_SOURCES)
	cd voidgui && meson compile -C ../build/voidgui

build/void: void/void.cabal $(HASKELL_SOURCES)
	cd void && cabal build --builddir=../build/void

link: uninstall
	ln -sv $(PROJECT_ROOT)/build/voidgui/libvoidgui.so /usr/lib/
	ln -sv $(PROJECT_ROOT)/voidgui/include/voidgui.h /usr/include/
	ln -sv $(PROJECT_ROOT)/voidgui/include/widgets/window.h /usr/include/
	ln -sv $(PROJECT_ROOT)/voidgui/include/widgets/table.h /usr/include/
	ln -sv $(PROJECT_ROOT)/voidgui/include/render/painter.h /usr/include/

# FIXME: rework all this stuff idk
install:
	install build/voidgui/libvoidgui.so /usr/lib/
	install voidgui/include/voidgui.h /usr/include/

uninstall:
	rm -f /usr/lib/libvoidgui.so
	rm -f /usr/include/voidgui.h
	rm -f /usr/include/window.h
	rm -f /usr/include/table.h
	rm -f /usr/include/painter.h
