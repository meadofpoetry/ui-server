BUILD = ocamlbuild
FLAGS = -use-ocamlfind -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

build: backend jsoo

ui_common:

backend: 
	$(BUILD) $(FLAGS) src/main.native

jsoo:    
	$(BUILD) $(FLAGS) jsoo/script.js

doc: build

test: build

all: build

clean:
	$(BUILD) -clean

.PHONY: build doc test all clean
