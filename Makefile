.PHONY: all clean

SOURCES=main.ml template.ml util.ml

all: main.native 

main.native: $(SOURCES)
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean
