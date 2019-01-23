PROFILE ?= release
BUILD   = dune build --profile $(PROFILE)
CLEAN   = dune clean
CSS     = scss --style compressed
CSS_DIR = dist/resources/css

all: build

clean:
	$(CLEAN)

testpage:
	$(BUILD) page/test.bc.js
	cp _build/default/page/test.bc.js dist/resources/js/test.js

home:
	$(BUILD) frontend/home.bc.js
	cp _build/default/frontend/home.bc.js dist/resources/js/home.js

input:
	$(BUILD) frontend/page_input.bc.js
	cp _build/default/frontend/page_input.bc.js dist/resources/js/input.js

stream:
	$(BUILD) frontend/page_stream.bc.js
	cp _build/default/frontend/page_stream.bc.js dist/resources/js/stream.js

pipeline:
	$(BUILD) frontend/page_mosaic.bc.js
	cp _build/default/frontend/page_mosaic.bc.js dist/resources/js/page_mosaic.js

hardware:
	$(BUILD) frontend/page_topology.bc.js
	cp _build/default/frontend/page_topology.bc.js dist/resources/js/page_topology.js

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
	$(CSS) $(CSS_DIR)/pages/demo/demo.scss $(CSS_DIR)/demo.min.css
	$(CSS) $(CSS_DIR)/pages/topology/topology.scss $(CSS_DIR)/topology.min.css
	$(CSS) $(CSS_DIR)/pages/pipeline/pipeline.scss $(CSS_DIR)/pipeline.min.css
	$(CSS) $(CSS_DIR)/pages/user/user.scss $(CSS_DIR)/user.min.css

css: css-components css-pages

backend:
	$(BUILD) backend/backend.exe
	cp _build/default/backend/backend.exe dist/backend

build: backend home pipeline hardware user network demo input stream
	@echo "Done"

dev: PROFILE = dev
dev: backend home pipeline hardware user network demo input stream
	@echo "Done"

doc:
	odig odoc
	odig doc

test:
	dune runtest

.PHONY: build doc test all clean backend home pipeline hardware user network demo input stream
