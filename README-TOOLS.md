# gemini-repl-tools.el - Comprehensive Tool Suite

This file provides an expanded set of tools for `gemini-repl.el`, including file operations, git integration, search capabilities, and Emacs-specific tools.

## Tool Categories

### FILE TOOLS
- **read_file** - Read file contents
- **write_file** - Write content to file
- **list_files** - List directory contents with glob patterns
- **edit_file** - Edit files using text replacement (diff-mode style)

### GIT TOOLS
All git tools use magit if available, falling back to shell commands:
- **git_status** - Show git status
- **git_diff** - Show git diff (staged or unstaged)
- **git_log** - Show commit history
- **git_branch** - List branches
- **git_blame** - Show file blame information

### SEARCH TOOLS
- **code_search** - Search code with ripgrep/grep
- **glob_files** - Find files matching glob patterns
- **search_preview** - Search with line numbers and context

### EMACS-SPECIFIC TOOLS
- **eval_elisp** - Evaluate Elisp code safely
- **describe_function** - Get function documentation
- **describe_variable** - Get variable documentation
- **find_definition** - Use xref to find symbol definitions
- **list_buffers** - List open buffers with modes and files
- **switch_buffer** - Switch to a buffer by name
- **insert_at_point** - Insert text at point in buffer

### SELF-AWARENESS TOOLS
- **project_map** - Get project structure (project.el/projectile)
- **get_capabilities** - List all available tools and system info

## Integration

### Automatic Loading
If `gemini-repl-tools.el` is in your load-path, it will be automatically loaded by `gemini-repl.el`:

```elisp
(require 'gemini-repl)
;; Tools are automatically registered on first use
```

### Manual Loading
```elisp
(require 'gemini-repl-tools)
(gemini-repl--init-default-tools)
```

### Checking Available Tools
From within the REPL:
```
/tools
```

Or programmatically:
```elisp
(length gemini-repl-tools)  ; Number of registered tools
```

## Tool Schema

Each tool follows this structure:
```elisp
((name . "tool_name")
 (description . "What the tool does")
 (parameters . ((type . "object")
                (properties . ...)
                (required . [...])))
 (execute . function-to-call))
```

## Safety

Tools are categorized as:
- **Safe** (read-only): Automatically allowed unless `gemini-repl-confirm-safe-tools` is t
- **Unsafe** (write operations): Require user confirmation unless `gemini-repl-yolo-mode` is t

Safe tools include:
- All git read operations
- All search operations
- File reading
- Emacs introspection (describe, find-definition, etc.)

Unsafe tools include:
- write_file, edit_file
- eval_elisp
- insert_at_point, switch_buffer

## Dependencies

Optional but recommended:
- **magit** - Enhanced git operations
- **xref** - Symbol definitions (built-in Emacs 28+)
- **project** - Project management (built-in Emacs 28+)
- **projectile** - Alternative project management

All dependencies are optional; tools gracefully degrade to shell commands or report unavailability.

## Extending

To add your own tools:

```elisp
(gemini-repl-register-tool
 "my_tool"
 "Description of what it does"
 '((type . "object")
   (properties . ((param1 . ((type . "string")
                             (description . "First parameter")))))
   (required . ["param1"]))
 (lambda (args)
   (let ((param1 (alist-get 'param1 args)))
     ;; Your tool implementation
     (format "Result: %s" param1))))
```

## File Structure

```
gemini-repl-010/
├── gemini-repl.el          # Main REPL implementation
├── gemini-repl-tools.el    # Comprehensive tool suite (this file)
├── gemini-repl-memory.el   # Memory/persistence (optional)
└── README-TOOLS.md         # This documentation
```

## Example Usage

```
> List all Emacs Lisp files in the project
[AI calls: glob_files with pattern="**/*.el"]

> Show me the git status
[AI calls: git_status]

> Search for "defun gemini-repl" in the codebase
[AI calls: code_search with pattern="defun gemini-repl"]

> What does the function gemini-repl--execute-tool do?
[AI calls: describe_function with function="gemini-repl--execute-tool"]

> Find the definition of gemini-repl-register-tool
[AI calls: find_definition with symbol="gemini-repl-register-tool"]
```

## Total Tools

The comprehensive tool suite provides **21 tools** covering:
- 4 file operations
- 5 git operations
- 3 search operations
- 7 Emacs-specific operations
- 2 self-awareness operations
