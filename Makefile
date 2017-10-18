BUILD = jbuilder

home:
	$(BUILD) build frontend/home.bc.js
	cp _build/default/frontend/home.bc.js dist/resources/js/home.js

frontend: home

backend:
	$(BUILD) build backend/backend.exe
	cp _build/default/backend/backend.exe dist/backend

build: backend frontend
	@echo "Done"

doc:
	echo "not implemented"

test:
	echo "not implemented"

all: build

clean:
	$(BUILD) clean

.PHONY: build doc test all frontend backend clean
