# Gemini REPL Slash Commands Reference

## Quick Reference

Type any command at the REPL prompt to execute it locally without sending to the AI.

### SESSION MANAGEMENT

| Command | Arguments | Description |
|---------|-----------|-------------|
| `/save` | `[name]` | Save current conversation (auto-generates name if not provided) |
| `/load` | `<name>` | Load a previously saved session |
| `/sessions` | - | List all saved sessions |
| `/delete` | `<name>` | Delete a saved session |
| `/export` | `[format] [file]` | Export conversation (json/markdown) |
| `/reset` | - | Clear conversation and reset all statistics |

### MEMORY SYSTEM

| Command | Arguments | Description |
|---------|-----------|-------------|
| `/memory` | - | List all remembered facts |
| `/remember` | `<key> <value>` | Store a fact in memory |
| `/forget` | `<key>` | Remove a fact from memory |

### INFORMATION

| Command | Arguments | Description |
|---------|-----------|-------------|
| `/help` | - | Show all available commands |
| `/tools` | - | List registered AI tools |
| `/model` | - | Show current model and provider info |
| `/stats` | - | Display session statistics |
| `/tokens` | - | Show token usage details |

### EMACS INTEGRATION

| Command | Arguments | Description |
|---------|-----------|-------------|
| `/region` | - | Send current region to AI |
| `/buffer` | - | Send current buffer to AI |
| `/org` | - | Send current org-mode subtree to AI |
| `/yank` | - | Insert last AI response into current buffer |

## Examples

```elisp
;; Save current conversation
> /save important-discussion

;; Remember project context
> /remember project gemini-repl-010
> /remember language elisp

;; Check statistics
> /stats
Session Statistics:
Messages: 12
Requests: 6
Approx. tokens: 4500
Session: important-discussion

;; List available tools
> /tools
Registered tools:
- read_file: Read contents of a file
- write_file: Write content to a file
- git_status: Get git status
- code_search: Search code using ripgrep
...

;; Export conversation
> /export markdown conversation.md
Exported to conversation.md (Markdown)

;; Send current region to AI
> /region
Region sent to AI

;; Yank last response to other buffer
> /yank
Last response yanked
```

## Notes

- Commands starting with `/` are processed locally and never sent to the AI
- Unknown commands will display an error and suggest using `/help`
- Session names are case-sensitive
- Token counts are approximate (based on character count / 4)
- Export formats: `json` (default) or `markdown`
- Emacs commands work with the most recently used buffer

## Session Workflow

1. Start REPL: `M-x gemini-repl`
2. Have conversation with AI
3. Save session: `/save project-discussion`
4. Later, load session: `/load project-discussion`
5. Export for sharing: `/export markdown discussion.md`

## Memory Workflow

1. Remember important facts: `/remember api-key sk-xxxxx`
2. List facts: `/memory`
3. Forget when done: `/forget api-key`

## Emacs Integration Workflow

1. Select code in source buffer
2. Switch to REPL: `C-x b *gemini-repl*`
3. Send region: `/region`
4. Get AI response
5. Insert response back: `/yank`
