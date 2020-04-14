all: build

.PHONY: build
build:
	stack build

.PHONY: run
run:
	stack run
