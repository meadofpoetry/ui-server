PROFILE   ?= release
BUILD     = dune build --profile $(PROFILE)
BUILD_DIR = _build/default
CLEAN     = dune clean

all: build

clean:
	$(CLEAN)

backend:
	$(BUILD) backend/backend.exe
	cp $(BUILD_DIR)/backend/backend.exe dist/backend

build: backend
	@echo "Done"

doc:
	odig odoc
	odig doc

test:
	dune runtest

.PHONY: build doc test all clean backend
