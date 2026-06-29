all: build

.PHONY: build
build:
	cabal v2-build all

.PHONY: test
test:
	cabal v2-test all --test-show-details=direct

.PHONY: run
run:
	cabal v2-run joy-exe
