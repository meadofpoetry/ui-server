OCB = ocamlbuild
BUILDFLAGS = -use-ocamlfind

build: 
	$(OCB) $(BUILDFLAGS) main.native

doc: build

test: build

all: build

clean:
	$(OCB) -clean

.PHONY: build doc test all clean
