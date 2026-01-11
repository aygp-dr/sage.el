# Sage Integrations Roadmap

Potential integrations with popular Emacs packages that would enhance sage's capabilities.

## High Priority

### 1. sage-lsp.el - LSP Mode Integration

```elisp
;; Provide LSP diagnostics to sage for context-aware assistance
(defun sage-lsp-get-diagnostics ()
  "Get current buffer's LSP diagnostics for sage context."
  (when (bound-and-true-p lsp-mode)
    (mapcar (lambda (diag)
              `((file . ,(buffer-file-name))
                (line . ,(lsp:diagnostic-range-start-line diag))
                (severity . ,(lsp:diagnostic-severity diag))
                (message . ,(lsp:diagnostic-message diag))))
            (lsp--get-buffer-diagnostics))))

;; Tool: fix_lsp_diagnostic
;; "Fix the LSP diagnostic at point using AI assistance"
```

**Features:**
- `fix_lsp_error` - AI-assisted error fixing
- `explain_lsp_diagnostic` - Explain what the error means
- `suggest_lsp_actions` - Suggest code actions

### 2. sage-magit.el - Enhanced Git Integration

```elisp
;; Tools beyond basic git:
;; - generate_commit_message: AI-generated commit messages from diff
;; - explain_merge_conflict: Explain conflict and suggest resolution
;; - review_staged_changes: Code review before commit
;; - suggest_branch_name: Generate branch name from description
```

**Features:**
- AI-generated commit messages from staged changes
- Merge conflict explanation and resolution
- Pre-commit code review
- PR description generation

### 3. sage-org.el - Extended Org Mode

```elisp
;; Tools for org-mode power users:
;; - summarize_org_file: Generate summary of org document
;; - expand_outline: Flesh out outline into full content
;; - org_to_presentation: Convert org to reveal.js/beamer
;; - schedule_todos: AI-assisted scheduling
```

**Features:**
- Document summarization
- Outline expansion
- Meeting notes extraction
- TODO prioritization

### 4. sage-flycheck.el - Diagnostics Integration

```elisp
;; Integrate with flycheck/flymake for error context
(defun sage-flycheck-errors-at-point ()
  "Get flycheck errors at point for AI context."
  (when (bound-and-true-p flycheck-mode)
    (flycheck-overlay-errors-at (point))))
```

## Medium Priority

### 5. sage-project.el - Project Management (Existing, Enhance)

```elisp
;; Deeper project.el/projectile integration:
;; - project_overview: Generate project documentation
;; - find_related_files: AI-powered related file discovery
;; - refactor_across_project: Multi-file refactoring
```

### 6. sage-dired.el - File Management

```elisp
;; AI-assisted file operations:
;; - rename_files_batch: Smart batch renaming
;; - organize_directory: Suggest file organization
;; - find_duplicates: Find similar/duplicate files
```

### 7. sage-company.el / sage-corfu.el - Completions

```elisp
;; AI-powered completions:
(defun sage-company-backend (command &optional arg &rest ignored)
  "Company backend using sage for completions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'sage-company-backend))
    (prefix (sage--completion-prefix))
    (candidates (sage--get-completions arg))
    (annotation (sage--completion-annotation arg))))
```

### 8. sage-treemacs.el - File Tree

```elisp
;; AI-enhanced file tree:
;; - Suggest files to open based on current work
;; - Highlight recently AI-modified files
;; - Project structure recommendations
```

## Lower Priority (Niche)

### 9. sage-latex.el - LaTeX/AUCTeX

```elisp
;; LaTeX assistance:
;; - fix_latex_error: Fix compilation errors
;; - suggest_package: Recommend packages for task
;; - explain_latex_macro: Explain what a macro does
```

### 10. sage-eshell.el - Shell Integration

```elisp
;; Shell command assistance:
;; - suggest_command: AI command suggestions
;; - explain_command: Explain what a command does
;; - fix_command_error: Fix shell errors
```

### 11. sage-eglot.el - Lightweight LSP

```elisp
;; Simpler LSP integration for eglot users
;; Similar to sage-lsp but for eglot
```

### 12. sage-mu4e.el / sage-notmuch.el - Email

```elisp
;; Email assistance:
;; - draft_reply: Generate reply draft
;; - summarize_thread: Summarize email thread
;; - extract_action_items: Find TODOs in emails
```

## Implementation Pattern

All integrations should follow this pattern:

```elisp
;;; sage-PACKAGE.el --- Sage integration with PACKAGE -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (sage "0.1") (PACKAGE "X.Y"))

(require 'sage-tools)
(require 'PACKAGE)

;; Check for hard dependency
(unless (featurep 'PACKAGE)
  (error "sage-PACKAGE requires PACKAGE to be installed"))

;; Register tools
(sage-tools--register
 "tool_name"
 "Tool description"
 '((type . "object") ...)
 #'sage-PACKAGE--tool-fn)

(provide 'sage-PACKAGE)
```

## Priority Matrix

| Integration | Value | Effort | Users |
|-------------|-------|--------|-------|
| sage-lsp | High | Medium | Many |
| sage-magit | High | Low | Many |
| sage-org | High | Medium | Many |
| sage-flycheck | Medium | Low | Many |
| sage-company | Medium | High | Many |
| sage-dired | Medium | Low | All |
| sage-latex | Low | Medium | Few |
| sage-mu4e | Low | Medium | Few |

## Quick Wins (Can implement now)

1. **sage-magit**: AI commit messages from `magit-diff-staged`
2. **sage-org**: Summarize org subtree
3. **sage-flycheck**: Explain error at point
4. **sage-dired**: Batch rename suggestions
