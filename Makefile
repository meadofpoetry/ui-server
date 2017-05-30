BUILD = ocamlbuild
FLAGS = -use-ocamlfind -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

build: frontend backend
	@echo "Done"

script:
	$(BUILD) $(FLAGS) jsoo/script.js
	cp _build/jsoo/script.js resources/js/script.js

frontend: script

backend:
	$(BUILD) $(FLAGS) src/main.native

doc: build

test: build

all: build

clean:
	$(BUILD) -clean

.PHONY: build doc test all clean
