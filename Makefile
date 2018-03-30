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
	$(BUILD) build frontend/topology.bc.js
	cp _build/default/frontend/topology.bc.js dist/resources/js/topology.js

user:
	$(BUILD) build frontend/user.bc.js
	cp _build/default/frontend/user.bc.js dist/resources/js/user.js

demo:
	$(BUILD) build frontend/demo.bc.js
	cp _build/default/frontend/demo.bc.js dist/resources/js/demo.js

css:
	$(CSS) dist/resources/css/components/components.scss dist/resources/css/components.css
	$(CSS) dist/resources/css/main.scss dist/resources/css/main.css
	$(CSS) dist/resources/css/pages/topology/topology.scss dist/resources/css/topology.css
	$(CSS) dist/resources/css/pages/pipeline/pipeline.scss dist/resources/css/pipeline.css

frontend: home pipeline hardware user demo input

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
