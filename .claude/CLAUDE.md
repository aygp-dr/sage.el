# sage.el - Claude Code Instructions

See also: [AGENTS.md](../AGENTS.md) for agent coordination patterns.

## Project Overview

Elisp implementation of Sage (version 010), providing an AI coding assistant for Emacs with multi-provider support and tool calling.

This is the Emacs/Elisp counterpart to gemini-repl-009 (Rust).

## Code Style

- Use `lexical-binding: t` in all Elisp files
- Follow Emacs Lisp conventions (no CamelCase, use-hyphens)
- Prefix all public functions with `sage-`
- Prefix all private functions with `sage--`
- Use `cl-lib` for Common Lisp-style utilities
- Document all public functions with docstrings

## Tool Development Rules

**CRITICAL: Use Emacs primitives, NOT shell commands**

```elisp
;; WRONG - shell commands fail unpredictably
(shell-command-to-string "rg pattern *.el")
(shell-command-to-string "grep -r pattern .")

;; CORRECT - native primitives
(directory-files-recursively dir "\\.el$")
(string-match-p pattern content)
```

**Hard dependencies over fallbacks:**
```elisp
;; CORRECT - fail fast with clear message
(unless (require 'magit nil t)
  (error "Git tools require magit"))

;; WRONG - silent fallback with different behavior
(if (require 'magit nil t) (magit-...) (shell-command ...))
```

**Required dependencies:**
- `magit` - All git operations
- `org-mode` - Todo management (built into Emacs)

## Testing

```bash
make test              # Core tests (fast)
make test-all          # All tests
make test-integration  # Requires Ollama
make demo              # Visual demo
make demo-batch        # Terminal demo
```

**Mocking pattern for unit tests:**
```elisp
(cl-letf (((symbol-function 'url-retrieve-synchronously)
           #'mock-url-retrieve))
  (should (equal expected (function-under-test))))
```

## Commit Guidelines

- Use conventional commits
- Use `--trailer` for co-author attribution
- Do NOT use 'generated with' in commit messages

## Key Files

| File | Purpose |
|------|---------|
| `sage.el` | Main package, REPL, providers |
| `sage-tools.el` | 28 built-in tools |
| `sage-tool-factory.el` | Self-extending tool system |
| `examples/tool-demos.el` | Automated demos |
| `docs/DEMO.org` | Blog-ready documentation |

## Architecture

```
sage.el
├── Customization (defcustom)
├── Provider API (sage--format-*, --parse-*)
├── HTTP Client (sage--request)
├── Tools (sage-register-tool, built-ins)
└── REPL Interface (sage-mode)

sage-tools.el (28 tools)
├── File Tools (read, write, edit, list, glob)
├── Search Tools (code_search, search_preview)
├── Git Tools [requires magit]
├── Org Tools [requires org-mode]
├── Web Tools (fetch, search)
└── Emacs Tools (describe, eval, buffers)

sage-tool-factory.el
├── create_tool - Define new tools dynamically
├── list_custom_tools
├── delete_tool
└── reload_tools
└── Built-in: hackernews, weather, uuid, timestamp
```

## Feature Priorities

1. Core REPL functionality
2. Multi-provider support (Gemini, Ollama, OpenAI)
3. Tool calling system (28 tools + factory)
4. Session persistence
5. Org-mode integration

## Code Review Checklist

For significant changes, request multi-perspective review:

```bash
bd create "Review: Domain expert - elisp patterns" -p 1
bd create "Review: L7 - tool calling, API contracts" -p 1
bd create "Review: CTO - architecture, security" -p 1
```

## Demo Commands

```bash
make demo              # Interactive GUI demo
make demo-batch        # Terminal output
./bin/run-demo --remote pi.lan  # Remote testing
```
