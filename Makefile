BUILD = ocamlbuild
FLAGS = -use-ocamlfind -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

build: frontend backend
	@echo "Done"

home:
	$(BUILD) $(FLAGS) jsoo/home.js
	cp _build/jsoo/home.js resources/js/home.js

frontend: home

backend:
	$(BUILD) $(FLAGS) src/main.native

doc: build

test: build

all: build

clean:
	$(BUILD) -clean

.PHONY: build doc test all clean
