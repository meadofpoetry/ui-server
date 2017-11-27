BUILD = jbuilder

home:
	$(BUILD) build frontend/home.bc.js
	cp _build/default/frontend/home.bc.js dist/resources/js/home.js

pipeline:
	$(BUILD) build frontend/pipeline.bc.js
	cp _build/default/frontend/pipeline.bc.js dist/resources/js/pipeline.js

hardware:
	$(BUILD) build frontend/hardware.bc.js
	cp _build/default/frontend/hardware.bc.js dist/resources/js/hardware.js

dvb:
	$(BUILD) build frontend/dvb_niit.bc.js
	cp _build/default/frontend/dvb_niit.bc.js dist/resources/js/dvb_niit.js

ip2ts:
	$(BUILD) build frontend/ip2ts.bc.js
	cp _build/default/frontend/ip2ts.bc.js dist/resources/js/ip2ts.js

demo:
	$(BUILD) build frontend/demo.bc.js
	cp _build/default/frontend/demo.bc.js dist/resources/js/demo.js

frontend: home pipeline hardware dvb demo ip2ts

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
