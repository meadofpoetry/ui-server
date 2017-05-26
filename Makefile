OCB = ocamlbuild
BUILDFLAGS = -use-ocamlfind -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

build: backend jsoo

backend:
	$(OCB) $(BUILDFLAGS) main.native

jsoo:
	$(OCB) $(BUILDFLAGS) jsoo/script.js

doc: build

test: build

all: build

clean:
	$(OCB) -clean

.PHONY: build doc test all clean
