BUILD_PKGS=
TEST_PKGS=oUnit
BUILD_FLAGS=-use-menhir -Is gen/src
DEBUG_FLAGS=-tag 'debug'
TEST_FLAGS=-use-ocamlfind -pkgs ${TEST_PKGS} -Is src

all: main


# CPP CODE

.PHONY: main
main: print gc helper
	g++ print.o helper.o gc.o src/main.cpp -o main

print:
	g++ src/print.cpp -c

helper:
	g++ src/helper.cpp -c

gc:
	g++ src/gc.cpp -c


# OCAML CODE

compile:
	rm -rf *.o
	ocamlbuild ${BUILD_FLAGS} gen/src/compile.native

test:
	ocamlbuild ${TEST_FLAGS} tests/test.native --

clean:
	rm -rf *.o main
	ocamlbuild -clean
