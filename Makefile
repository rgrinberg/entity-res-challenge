
default: build test

time:
	make build
	time ./challenge.native

build:
	ocamlbuild -use-ocamlfind lib/challenge.native

test:
	ocamlbuild -use-ocamlfind lib_test/vector_test.native
	./vector_test.native

clean:
	ocamlbuild -clean
