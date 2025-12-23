# Emacs-Native Integration for Gemini REPL

This document describes the Emacs-native features added to gemini-repl-010.

## Files Created

### Core Integration: `gemini-repl-emacs.el`
**Lines:** 504
**Size:** 19KB

Provides deep integration with Emacs modes and workflows:

#### Org Mode Integration
- `gemini-repl-org-send-subtree` - Send org subtree as context
- `gemini-repl-org-send-src-block` - Send org source block
- `gemini-repl-org-insert-response` - Insert response as org AI block
- `gemini-repl-org-process-ai-blocks` - Process all `#+begin_ai` blocks

#### Buffer Integration
- `gemini-repl-send-buffer` - Send entire buffer
- `gemini-repl-send-region-improved` - Enhanced region sending with prompts
- `gemini-repl-send-defun` - Send current function definition
- `gemini-repl-insert-response` - Insert last AI response
- `gemini-repl-ask-about-buffer` - Interactive buffer queries
- `gemini-repl-ask-about-project` - Project-level queries

#### Dired Integration
- `gemini-repl-dired-summarize` - Summarize marked files
- `gemini-repl-dired-describe-file` - Describe file at point

#### Prog Mode Integration
- `gemini-repl-explain-error` - Explain error with context
- `gemini-repl-suggest-fix` - Suggest fixes for code issues
- `gemini-repl-explain-at-point` - Explain symbol/code at point
- `gemini-repl-explain-compilation-error` - Explain compilation errors

#### Minor Mode
- `gemini-repl-mode` - Minor mode with comprehensive keybindings
- `global-gemini-repl-mode` - Global minor mode variant

### Tests: `test/gemini-repl-emacs-test.el`
**Lines:** 90
**Size:** 3.1KB

Comprehensive test suite covering:
- Language detection for syntax highlighting
- Org mode block insertion
- Comment insertion
- Mode detection
- Minor mode activation
- Keybinding verification
- Customization defaults
- Response capture

### Examples and Documentation

#### `examples/init-example.el`
**Lines:** 195
**Size:** 5.7KB

Complete configuration example showing:
- Basic setup with API keys
- Provider selection
- Permission settings
- Emacs integration customization
- Custom tool registration
- Advanced configuration patterns
- Keybinding customization
- Integration with other packages

#### `examples/usage-guide.org`
**Lines:** 428
**Size:** 10.7KB

Comprehensive usage guide covering:
- Quick start
- Common workflows (code review, debugging, documentation)
- Org mode integration patterns
- Dired file management
- Writing and refactoring
- Advanced patterns and automation
- Integration with Magit, LSP, Projectile
- Troubleshooting
- Example sessions

### Documentation Updates: `README.org`

Added comprehensive section "Emacs-Native Integrations" including:
- Feature tables for all integrations
- Org mode AI blocks usage
- Buffer operation reference
- Dired commands
- Prog mode commands
- Compilation mode integration
- Minor mode setup
- Customization options
- Complete keybinding reference

## Keybindings

All keybindings use the `C-c C-g` prefix:

### Global Commands
| Key         | Command                          |
|-------------|----------------------------------|
| `C-c C-g r` | Open REPL                        |
| `C-c C-g g` | Send region                      |
| `C-c C-g b` | Send buffer                      |
| `C-c C-g d` | Send defun                       |
| `C-c C-g e` | Explain at point                 |
| `C-c C-g i` | Insert response                  |
| `C-c C-g a` | Ask about buffer                 |
| `C-c C-g p` | Ask about project                |

### Prog Mode Commands
| Key         | Command                          |
|-------------|----------------------------------|
| `C-c C-g x` | Explain error                    |
| `C-c C-g f` | Suggest fix                      |

### Org Mode Commands
| Key           | Command                        |
|---------------|--------------------------------|
| `C-c C-g o s` | Send org subtree               |
| `C-c C-g o c` | Send org source block          |
| `C-c C-g o i` | Insert org AI block            |
| `C-c C-g o p` | Process org AI blocks          |

### Dired Commands
| Key         | Command                          |
|-------------|----------------------------------|
| `C-c C-g s` | Summarize files                  |
| `C-c C-g d` | Describe file                    |

## Customization Options

```elisp
;; Org mode AI block type
(setq gemini-repl-emacs-org-ai-block-type "ai")

;; Insert responses as comments in code buffers
(setq gemini-repl-emacs-insert-as-comment nil)

;; Lines of context for error explanations
(setq gemini-repl-emacs-context-lines 5)

;; Auto-format responses based on mode
(setq gemini-repl-emacs-auto-format-response t)
```

## Features Summary

### Org Mode Integration ✓
- [x] Send org subtree as context
- [x] Send source blocks to AI
- [x] Insert responses as org blocks
- [x] Support for `#+begin_ai` blocks
- [x] Batch processing of AI blocks

### Buffer Integration ✓
- [x] Send entire buffer
- [x] Enhanced region sending with prompts
- [x] Send function definitions
- [x] Insert AI responses at point
- [x] Context-aware insertion (comments in code)
- [x] Interactive buffer queries

### Dired Integration ✓
- [x] Summarize marked files
- [x] Describe file at point
- [x] Read file contents for text files
- [x] Calculate size statistics

### Prog Mode Integration ✓
- [x] Explain errors with context
- [x] Suggest fixes
- [x] Explain symbols at point
- [x] Context extraction
- [x] Mode-specific syntax highlighting

### Compilation Mode Integration ✓
- [x] Explain compilation errors

### Minor Mode ✓
- [x] Global and local minor mode
- [x] Comprehensive keybindings
- [x] Mode-specific bindings
- [x] Hook integration

## Usage Example

```elisp
;; Setup
(require 'gemini-repl-emacs)
(global-gemini-repl-mode 1)

;; In any buffer:
;; C-c C-g g - Send selected region
;; C-c C-g b - Send entire buffer
;; C-c C-g e - Explain at point

;; In Org mode:
;; C-c C-g o s - Send subtree
;; C-c C-g o c - Send source block

;; In Dired:
;; Mark files with 'm', then C-c C-g s to summarize

;; In prog-mode:
;; C-c C-g x - Explain error at point
;; C-c C-g f - Suggest fix
```

## Implementation Details

### Language Detection
Automatic language detection for syntax highlighting in code snippets:
- Emacs Lisp → `elisp`
- Python → `python`
- Rust → `rust`
- JavaScript/TypeScript → `javascript`/`typescript`
- And more...

### Response Capture
Automatically captures AI responses for later insertion with `C-c C-g i`.

### Context Management
Extracts configurable lines of context (default: 5) for error explanations.

### Safety
Inherits all safety features from base `gemini-repl`:
- Path validation
- Workspace restrictions
- Permission system

## Testing

Run tests with:
```bash
emacs -batch -l ert -l test/gemini-repl-emacs-test.el -f ert-run-tests-batch-and-exit
```

## Integration Points

### Hooks
- `dired-mode-hook` - Setup Dired bindings
- `compilation-mode-hook` - Setup compilation bindings

### Advice
- `gemini-repl--handle-response` - Capture responses for insertion

### Dependencies
- `gemini-repl` (required)
- `org` (optional, for org integration)
- `dired` (optional, for file management)

## Future Enhancements

Potential additions:
- [ ] Integration with `company-mode` for AI-powered completion
- [ ] `magit` integration for commit review
- [ ] `flycheck`/`flymake` integration
- [ ] Project templates generation
- [ ] Code snippet extraction and refinement
- [ ] Multi-file refactoring support

## See Also

- `gemini-repl.el` - Core REPL functionality
- `examples/init-example.el` - Configuration examples
- `examples/usage-guide.org` - Detailed usage patterns
- `README.org` - Project documentation
