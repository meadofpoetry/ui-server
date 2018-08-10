BUILD   = dune
CSS     = scss --style compressed
CSS_DIR = dist/resources/css

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

network:
	$(BUILD) build frontend/network.bc.js
	cp _build/default/frontend/network.bc.js dist/resources/js/network.js

demo:
	$(BUILD) build frontend/demo.bc.js
	cp _build/default/frontend/demo.bc.js dist/resources/js/demo.js

css-components:
	$(CSS) $(CSS_DIR)/components/components.scss $(CSS_DIR)/components.min.css

css-pages:
	$(CSS) $(CSS_DIR)/main.scss $(CSS_DIR)/main.min.css
	$(CSS) $(CSS_DIR)/pages/topology/topology.scss $(CSS_DIR)/topology.min.css
	$(CSS) $(CSS_DIR)/pages/pipeline/pipeline.scss $(CSS_DIR)/pipeline.min.css
	$(CSS) $(CSS_DIR)/pages/user/user.scss $(CSS_DIR)/user.min.css

css: css-components css-pages

frontend: home pipeline hardware user network demo input

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
