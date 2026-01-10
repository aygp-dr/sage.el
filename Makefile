.PHONY: all test test-all clean compile lint

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch -L .

# Core modules
EL_FILES = gemini-repl.el \
           gemini-repl-context.el \
           gemini-repl-emacs.el \
           gemini-repl-memory.el \
           gemini-repl-project.el \
           gemini-repl-queue.el \
           gemini-repl-ratelimit.el \
           gemini-repl-session.el \
           gemini-repl-tools.el

ELC_FILES = $(EL_FILES:.el=.elc)

# Test files
TEST_FILES = test/gemini-repl-test.el \
             test/gemini-repl-context-test.el \
             test/gemini-repl-emacs-test.el \
             test/gemini-repl-memory-test.el \
             test/gemini-repl-project-test.el \
             test/gemini-repl-queue-test.el \
             test/gemini-repl-ratelimit-test.el

all: compile

# Compile all elisp files
compile:
	@for f in $(EL_FILES); do \
		echo "Compiling $$f..."; \
		$(EMACS_BATCH) -f batch-byte-compile "$$f" 2>&1 | grep -v "^$$" || true; \
	done

# Run core tests (fast, stable)
test: compile
	$(EMACS_BATCH) -l ert -l test/gemini-repl-test.el -f ert-run-tests-batch-and-exit
	$(EMACS_BATCH) -l ert -l gemini-repl-project.el -l test/gemini-repl-project-test.el -f ert-run-tests-batch-and-exit

# Run all tests
test-all: compile
	@for f in $(TEST_FILES); do \
		echo "=== Running $$f ==="; \
		$(EMACS_BATCH) -l ert -l "$$f" -f ert-run-tests-batch-and-exit || exit 1; \
	done

test-ratelimit:
	$(EMACS_BATCH) -l ert -l gemini-repl-ratelimit.el -l test/gemini-repl-ratelimit-test.el -f ert-run-tests-batch-and-exit

test-context:
	$(EMACS_BATCH) -l ert -l gemini-repl-context.el -l test/gemini-repl-context-test.el -f ert-run-tests-batch-and-exit

test-project:
	$(EMACS_BATCH) -l ert -l gemini-repl-project.el -l test/gemini-repl-project-test.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS_BATCH) --eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(require 'checkdoc)" \
		-f checkdoc-file $(EL_FILES)

clean:
	rm -f $(ELC_FILES)
	rm -f test/*.elc

.DEFAULT_GOAL := all
