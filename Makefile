BUILD = ocamlbuild
FLAGS = -use-ocamlfind -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

build: script backend
	@echo "Done"

script:
	$(BUILD) $(FLAGS) jsoo/script.js

backend:
	$(BUILD) $(FLAGS) src/main.native

doc: build

test: build

all: build

clean:
	$(BUILD) -clean

.PHONY: build doc test all clean
