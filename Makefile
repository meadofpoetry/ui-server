BUILD = jbuilder build

build: backend jsoo

ui_common:
	$(BUILD)

backend: ui_common
	$(BUILD) src/main.exe

jsoo:
	$(BUILD) jsoo/script.bc.js

doc: build

test: build

all: build

clean:

.PHONY: build doc test all clean
