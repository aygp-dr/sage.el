.PHONY: all test test-all test-tools clean compile lint checkdoc package-lint check-headers \
        elisp-version elisp-load-test elisp-check-syntax elisp-http-inbox \
        check ci gh-status gh-failures gh-watch gh-logs

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

test-tools:
	$(EMACS_BATCH) -l scripts/test-tools.el

lint: checkdoc check-headers

# Check documentation strings
checkdoc:
	@echo "Running checkdoc..."
	@for f in $(EL_FILES); do \
		echo "  Checking $$f..."; \
		$(EMACS_BATCH) --eval "(require 'checkdoc)" \
			--eval "(setq checkdoc-spellcheck-documentation-flag nil)" \
			--eval "(with-current-buffer (find-file-noselect \"$$f\") \
			         (checkdoc-current-buffer t))" 2>&1 || true; \
	done

# Check package headers and conventions (requires package-lint)
package-lint:
	@echo "Running package-lint..."
	@$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) \
		         (package-refresh-contents) \
		         (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(EL_FILES)

# Check for lexical-binding and standard headers
check-headers:
	@echo "Checking file headers..."
	@failed=0; \
	for f in $(EL_FILES); do \
		if ! head -1 "$$f" | grep -q "lexical-binding: t"; then \
			echo "ERROR: $$f missing lexical-binding: t"; \
			failed=1; \
		fi; \
		if ! grep -q "^;; Author:" "$$f"; then \
			echo "WARNING: $$f missing Author header"; \
		fi; \
		if ! grep -q "^;; URL:" "$$f"; then \
			echo "WARNING: $$f missing URL header"; \
		fi; \
	done; \
	if [ $$failed -eq 1 ]; then exit 1; fi
	@echo "All headers OK"

clean:
	rm -f $(ELC_FILES)
	rm -f test/*.elc

# === Quick validation targets ===

# Show Emacs version
elisp-version:
	@$(EMACS_BATCH) --eval '(message "Emacs %s" emacs-version)'

# Test that all modules load without errors
elisp-load-test:
	@echo "Testing module loading..."
	@$(EMACS_BATCH) \
		-l sage.el \
		-l sage-context.el \
		-l sage-emacs.el \
		-l sage-memory.el \
		-l sage-project.el \
		-l sage-queue.el \
		-l sage-ratelimit.el \
		-l sage-session.el \
		-l sage-tools.el \
		--eval '(message "All modules loaded successfully")'

# Byte-compile with strict warning checks (fails on warnings)
elisp-check-syntax:
	@echo "Checking syntax with strict byte-compilation..."
	@$(EMACS_BATCH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES) 2>&1 || \
		(echo "Syntax check failed - see warnings above"; exit 1)

# Test HTTP connectivity (requires running server)
elisp-http-inbox:
	@echo "Testing HTTP inbox fetch (Ollama)..."
	@$(EMACS_BATCH) -l scripts/check-ollama.el

# Combined check target for CI
check: elisp-version elisp-load-test check-headers compile test

# Full CI pipeline
ci: check test-all lint

# === GitHub Actions targets ===

# Show recent workflow runs
gh-status:
	@echo "=== Recent GitHub Actions Runs ==="
	@gh run list --limit 10

# Show failed runs with details
gh-failures:
	@echo "=== Failed GitHub Actions Runs ==="
	@gh run list --status failure --limit 5
	@echo ""
	@echo "To view details: gh run view <run-id>"
	@echo "To view logs: gh run view <run-id> --log-failed"

# Watch current/latest run
gh-watch:
	@echo "Watching latest run..."
	@gh run watch

# View logs from latest failed run
gh-logs:
	@latest=$$(gh run list --status failure --limit 1 --json databaseId -q '.[0].databaseId'); \
	if [ -n "$$latest" ]; then \
		echo "=== Logs from run $$latest ==="; \
		gh run view $$latest --log-failed; \
	else \
		echo "No failed runs found"; \
	fi

.DEFAULT_GOAL := all
