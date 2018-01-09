BUILD = jbuilder
CSS   = scss

home:
	$(BUILD) build frontend/home.bc.js
	cp _build/default/frontend/home.bc.js dist/resources/js/home.js

input:
	$(BUILD) build frontend/input.bc.js
	cp _build/default/frontend/input.bc.js dist/resources/js/input.js

pipeline:
	$(BUILD) build frontend/pipeline.bc.js
	cp _build/default/frontend/pipeline.bc.js dist/resources/js/pipeline.js

hardware:
	$(BUILD) build frontend/hardware.bc.js
	cp _build/default/frontend/hardware.bc.js dist/resources/js/hardware.js

demo:
	$(BUILD) build frontend/demo.bc.js
	cp _build/default/frontend/demo.bc.js dist/resources/js/demo.js

css:
	$(CSS) dist/resources/css/table.scss dist/resources/css/table.css

frontend: home pipeline hardware demo input css

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

.PHONY: build doc test all frontend backend pipeline hardware clean
