.PHONY: all test clean compile

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch -L .

EL_FILES = gemini-repl.el gemini-repl-ratelimit.el gemini-repl-memory.el gemini-repl-session.el
ELC_FILES = $(EL_FILES:.el=.elc)
TEST_FILES = test/gemini-repl-test.el test/gemini-repl-ratelimit-test.el

all: compile

compile: $(ELC_FILES)

%.elc: %.el
	$(EMACS_BATCH) -f batch-byte-compile $<

test: compile
	$(EMACS_BATCH) -l ert -l test/gemini-repl-test.el -f ert-run-tests-batch-and-exit
	$(EMACS_BATCH) -l ert -l gemini-repl-ratelimit.el -l test/gemini-repl-ratelimit-test.el -f ert-run-tests-batch-and-exit

test-ratelimit:
	$(EMACS_BATCH) -l ert -l gemini-repl-ratelimit.el -l test/gemini-repl-ratelimit-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(ELC_FILES)
	rm -f test/*.elc

.DEFAULT_GOAL := all
