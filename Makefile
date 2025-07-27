EMACS ?= emacs

# Run all tests by default.
MATCH ?=

.PHONY: test clean

default: all

# Remove compiled files
clean:
	rm -f *.elc

EL_FILES := $(wildcard *.el)

# Run checkdoc on elisp files. To do this, we run checkdoc-file via -eval on every .el file in EL_FILES
checkdoc:
	for FILE in ${EL_FILES}; do $(EMACS) --batch -L . -eval "(setq sentence-end-double-space nil)" -eval "(checkdoc-file \"$$FILE\")" 2>&1 | grep -v "should be imperative" || true ; done

compile: clean
	@$(EMACS) --batch -L . \
		--eval "(setq sentence-end-double-space nil)" \
		-f batch-byte-compile *.el 2>&1 | grep -v "websocket" || true

all: checkdoc compile
