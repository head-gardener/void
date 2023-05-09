PROJECT_ROOT = $(shell pwd)
BINPATH = build/void/build/x86_64-linux/ghc-9.0.2/voidcore-0.1.0.0/x/void/build/void
GDB = rust-gdb -q

# C_SOURCES = $(wildcard voidgui/src/**/*.c) $(wildcard voidgui/src/*.c)
# C_HEADERS = $(wildcard voidgui/include/**/*.h) $(wildcard voidgui/include/*.h)
GLSL_SOURCES = $(wildcard voidgui/voidgui/src/render/shaders/*)
HASKELL_SOURCES = $(wildcard void/app/**/*.hs) $(wildcard void/app/*.hs)
RUST_SOURCES = $(wildcard voidgui/**/Cargo.toml) $(wildcard voidgui/**/src/**/*.rs) $(wildcard voidgui/**/src/*.rs)

run: build
	$(PROJECT_ROOT)/$(BINPATH)/void

debug: build
	$(GDB) $(PROJECT_ROOT)/$(BINPATH)/void

test:
	cd voidgui && RUST_BACKTRACE=1 cargo test
	cd void && cabal test

bench:
	cd voidgui && RUST_BACKTRACE=1 cargo bench

clear: 
	rm -rf build/*

build: build/voidgui build/void

build/voidgui: $(RUST_SOURCES) $(GLSL_SOURCES)
	cd voidgui && cargo build

build/void: void/void.cabal $(HASKELL_SOURCES)
	cd void && cabal build --builddir=../build/void

link: uninstall
	ln -sv $(PROJECT_ROOT)/voidgui/target/debug/libvoidgui.so /usr/lib/

# FIXME: rework all this stuff idk
install:
	install voidgui/target/debug/libvoidgui.so /usr/lib/

uninstall:
	rm -f /usr/lib/libvoidgui.so

# configure: 
# 	cd voidgui && meson setup ../build
