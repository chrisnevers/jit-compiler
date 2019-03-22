all : main

main :
	g++ main.cpp -o vm
	ocamlc gen.ml -o gen

clean :
	rm -rf *.o vm gen

