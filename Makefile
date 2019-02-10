PROFILE    ?= dev
BUILD      = dune build --profile $(PROFILE)
CLEAN      = dune clean
CSS        = scss --style compressed
CSS_DIR    = dist/resources/css
JS_TARGETS = home input stream mosaic_video mosaic_editor \
					   topology demo settings_user settings_network

all: build

$(JS_TARGETS):
	$(BUILD) frontend/$@/js/index.bc.js
	cp _build/default/frontend/$@/js/index.bc.js dist/resources/js/$@.js

clean:
	$(CLEAN)

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

frontend: $(JS_TARGETS)

build: backend frontend
	@echo "Done"

dev: PROFILE = dev
dev: backend home frontend
	@echo "Done"

doc:
	odig odoc
	odig doc

test:
	dune runtest

.PHONY: build doc test all clean backend $(JS_TARGETS)
