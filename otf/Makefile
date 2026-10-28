# Makefile for runes/otf and zpb-ttf
# OpenType & TrueType font parsing and metric extraction library for Common Lisp

SBCL ?= sbcl
# The .asd files are at the root of the Runes repository, one level up.
ROOT := $(abspath $(CURDIR)/..)
SBCL_FLAGS ?= --noinform --eval '(ignore-errors (load "~/quicklisp/setup.lisp"))' --eval '(push (pathname "$(ROOT)/") asdf:*central-registry*)' --eval '(push (pathname "$(ROOT)/decompress/") asdf:*central-registry*)'

QL_LOCAL_PROJECTS ?= $(HOME)/quicklisp/local-projects
LINK_NAME ?= runes

.PHONY: all codegen generate compile build install test tests test-decompress bench benchmark benchmarks clean help

all: compile

help:
	@echo "Available targets:"
	@echo "  make codegen       - Run code generator on schemas/ to produce src/tables/"
	@echo "  make compile       - Compile :runes/otf and :zpb-ttf systems"
	@echo "  make install       - Link the Runes repository into ~/quicklisp/local-projects/"
	@echo "  make test          - Run all test suites (contour + comprehensive table readers)"
	@echo "  make test-decompress - Run the decompress tests (needs libbrotli and libz)"
	@echo "  make bench         - Run performance benchmarks"
	@echo "  make clean         - Clean ASDF FASL cache and temporary files"

## Run code generator on schemas/ to generate src/tables/
codegen generate:
	@echo "==> Running OpenType table code generator..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(asdf:load-system :runes/otf/codegen :force t)' \
		--eval '(runes/otf/codegen:generate-all-tables #p"$(CURDIR)/schemas/" #p"$(CURDIR)/src/tables/")' \
		--eval '(sb-ext:exit :code 0)'

## Compile library systems (:runes/otf and :zpb-ttf)
compile build:
	@echo "==> Compiling :runes/otf and :zpb-ttf..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(asdf:compile-system :runes/otf :force t)' \
		--eval '(asdf:compile-system :zpb-ttf :force t)' \
		--eval '(sb-ext:exit :code 0)'

## Link the Runes repository into Quicklisp local-projects
install:
	@echo "==> Installing symlink into $(QL_LOCAL_PROJECTS)/$(LINK_NAME)..."
	mkdir -p $(QL_LOCAL_PROJECTS)
	ln -sfn $(ROOT) $(QL_LOCAL_PROJECTS)/$(LINK_NAME)
	@echo "==> Registering local projects with Quicklisp..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(ql:register-local-projects)' \
		--eval '(sb-ext:exit :code 0)'
	@echo "Successfully installed symlink."

## Run all tests
test tests:
	@echo "==> Running test suite..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(asdf:load-system :zpb-ttf :force t)' \
		--load "$(CURDIR)/tests/run-tests.lisp"

## Run the Brotli/zlib decoder tests against libbrotli and libz
test-decompress:
	@echo "==> Running decompress test suite..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(asdf:test-system :decompress)' \
		--eval '(sb-ext:exit :code 0)'

## Run performance benchmarks
bench benchmark benchmarks:
	@echo "==> Running performance benchmarks..."
	$(SBCL) $(SBCL_FLAGS) \
		--eval '(asdf:load-system :zpb-ttf)' \
		--load "$(CURDIR)/tests/benchmark.lisp" \
		--eval '(sb-ext:exit :code 0)'

## Clean compiled FASL files
clean:
	@echo "==> Cleaning ASDF cache..."
	rm -rf $(HOME)/.cache/common-lisp/sbcl-*/$(CURDIR)
	find . -name "*.fasl" -delete
	@echo "Clean complete."
