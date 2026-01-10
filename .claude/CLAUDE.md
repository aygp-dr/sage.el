# sage.el - Claude Code Instructions

## Project Overview

Elisp implementation of Sage (version 010), providing an AI coding assistant for Emacs with multi-provider support and tool calling.

This is the Emacs/Elisp counterpart to sage-009 (Rust).

## Code Style

- Use `lexical-binding: t` in all Elisp files
- Follow Emacs Lisp conventions (no CamelCase, use-hyphens)
- Prefix all public functions with `sage-`
- Prefix all private functions with `sage--`
- Use `cl-lib` for Common Lisp-style utilities
- Document all public functions with docstrings

## Testing

Run tests with:
```bash
cask exec ert-runner
```

## Commit Guidelines

- Use conventional commits
- Use `--trailer` for co-author attribution
- Do NOT use 'generated with' in commit messages

## Key Files

- `sage.el` - Main package file
- `README.org` - Documentation
- `Cask` - Package dependencies

## Architecture

```
sage.el
├── Customization (defcustom)
├── Provider API (sage--format-*, --parse-*)
├── HTTP Client (sage--request)
├── Tools (sage-register-tool, built-ins)
└── REPL Interface (sage-mode)
```

## Feature Priorities

1. Core REPL functionality
2. Multi-provider support
3. Tool calling system
4. Session persistence
5. Org-mode integration
