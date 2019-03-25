BUILD_PKGS=
TEST_PKGS=oUnit
BUILD_FLAGS=-Is gen/src
DEBUG_FLAGS=-tag 'debug'
TEST_FLAGS=-use-ocamlfind -pkgs ${TEST_PKGS} -Is src

all: main

.PHONY: main
main:
	ocamlbuild ${BUILD_FLAGS} gen/src/compile.native --
	g++ src/print.cpp src/main.cpp -o main

test:
	ocamlbuild ${TEST_FLAGS} tests/test.native --

clean:
	rm -rf *.o main
	ocamlbuild -clean
