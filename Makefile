BUILD   = dune build
CLEAN   = dune clean
CSS     = scss --style compressed
CSS_DIR = dist/resources/css

home:
	$(BUILD) frontend/home.bc.js
	cp _build/default/frontend/home.bc.js dist/resources/js/home.js

input:
	$(BUILD) frontend/input.bc.js
	cp _build/default/frontend/input.bc.js dist/resources/js/input.js

pipeline:
	$(BUILD) frontend/pipeline_video.bc.js
	$(BUILD) frontend/pipeline_editor.bc.js
	cp _build/default/frontend/pipeline_video.bc.js dist/resources/js/pipeline_video.js
	cp _build/default/frontend/pipeline_editor.bc.js dist/resources/js/pipeline_editor.js

hardware:
	$(BUILD) frontend/topology.bc.js
	$(BUILD) frontend/board_qos_stream.bc.js
	cp _build/default/frontend/topology.bc.js dist/resources/js/topology.js
	cp _build/default/frontend/board_qos_stream.bc.js dist/resources/js/board_qos_stream.js

user:
	$(BUILD) frontend/user.bc.js
	cp _build/default/frontend/user.bc.js dist/resources/js/user.js

network:
	$(BUILD) frontend/network.bc.js
	cp _build/default/frontend/network.bc.js dist/resources/js/network.js

demo:
	$(BUILD) frontend/demo.bc.js
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
	$(BUILD) backend/backend.exe
	cp _build/default/backend/backend.exe dist/backend


build: backend frontend
	@echo "Done"

doc:
	echo "not implemented"

test:
	echo "not implemented"

all: build

clean:
	$(CLEAN) clean

.PHONY: build doc test all frontend backend pipeline hardware clean
