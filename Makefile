.PHONY: all

all: main.native 

main.native: main.ml
	ocamlbuild -use-ocamlfind main.native
