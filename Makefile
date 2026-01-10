.PHONY: all test test-all clean compile lint

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch -L .

# Core modules
EL_FILES = sage.el \
           sage-context.el \
           sage-emacs.el \
           sage-memory.el \
           sage-project.el \
           sage-queue.el \
           sage-ratelimit.el \
           sage-session.el \
           sage-tools.el

ELC_FILES = $(EL_FILES:.el=.elc)

# Test files
TEST_FILES = test/sage-test.el \
             test/sage-context-test.el \
             test/sage-emacs-test.el \
             test/sage-memory-test.el \
             test/sage-project-test.el \
             test/sage-queue-test.el \
             test/sage-ratelimit-test.el

all: compile

# Compile all elisp files
compile:
	@for f in $(EL_FILES); do \
		echo "Compiling $$f..."; \
		$(EMACS_BATCH) -f batch-byte-compile "$$f" 2>&1 | grep -v "^$$" || true; \
	done

# Run core tests (fast, stable)
test: compile
	$(EMACS_BATCH) -l ert -l test/sage-test.el -f ert-run-tests-batch-and-exit
	$(EMACS_BATCH) -l ert -l sage-project.el -l test/sage-project-test.el -f ert-run-tests-batch-and-exit

# Run all tests
test-all: compile
	@for f in $(TEST_FILES); do \
		echo "=== Running $$f ==="; \
		$(EMACS_BATCH) -l ert -l "$$f" -f ert-run-tests-batch-and-exit || exit 1; \
	done

test-ratelimit:
	$(EMACS_BATCH) -l ert -l sage-ratelimit.el -l test/sage-ratelimit-test.el -f ert-run-tests-batch-and-exit

test-context:
	$(EMACS_BATCH) -l ert -l sage-context.el -l test/sage-context-test.el -f ert-run-tests-batch-and-exit

test-project:
	$(EMACS_BATCH) -l ert -l sage-project.el -l test/sage-project-test.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS_BATCH) --eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(require 'checkdoc)" \
		-f checkdoc-file $(EL_FILES)

clean:
	rm -f $(ELC_FILES)
	rm -f test/*.elc

.DEFAULT_GOAL := all
