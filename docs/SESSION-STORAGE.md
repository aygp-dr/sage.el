# Session Storage Specification

This document specifies how sage stores session history, aligned with Claude Code's conventions while supporting Emacs-native workflows.

## Design Philosophy

Unlike Claude Code which requires `--continue` to resume sessions, sage defaults to **continuous conversation** within a project context. This is because:

1. **Emacs sessions are long-lived** - users typically keep Emacs running
2. **Compaction is built-in** - sage-context handles context window limits
3. **Reflection is native** - sage-reflect tracks session patterns

## Storage Location

```
~/.emacs.d/sage/              # Default (emacs-native preset)
├── projects/                 # Per-project conversations
│   └── <encoded-path>/
│       ├── conversation.jsonl
│       ├── metadata.json
│       └── history/
├── memory/                   # Persistent facts
└── config/                   # User configuration
```

### Storage Presets

Configure via `sage-project-storage-preset`:

| Preset | Directory | Use Case |
|--------|-----------|----------|
| `emacs-native` | `~/.emacs.d/sage/projects/` | Default, Emacs-centric |
| `standalone` | `~/.sage/projects/` | Separate from Emacs config |
| `claude-shared` | `~/.claude/projects/` | Share history with Claude Code |
| `custom` | User-defined | Full control |

```elisp
;; In your init.el
(setq sage-project-storage-preset 'claude-shared)  ; Share with Claude
```

## Path Encoding

To align with Claude Code, encode paths as follows:

| Original | With encode-dots=t | With encode-dots=nil |
|----------|-------------------|---------------------|
| `/home/user/github.com/repo` | `-home-user-github-com-repo` | `-home-user-github.com-repo` |

Configure via `sage-project-encode-dots` (default: t for Claude compatibility).

## JSONL Message Schema

### Current Schema (Simple)

```json
{
  "role": "user|assistant|system",
  "content": "message text",
  "timestamp": "2026-01-11T23:00:00Z"
}
```

### Extended Schema (Proposed)

```json
{
  "uuid": "msg-uuid",
  "parentUuid": "parent-uuid|null",
  "sessionId": "session-uuid",
  "role": "user|assistant|system",
  "content": "message text",
  "timestamp": "2026-01-11T23:00:00.000Z",
  "metadata": {
    "model": "gemini-2.0-flash-exp",
    "gitBranch": "main"
  }
}
```

## Default Behavior: Auto-Continue

Unlike Claude Code's `--continue` flag, sage automatically continues the last session:

```elisp
;; Just start - automatically continues
M-x sage-start

;; Force new session
C-u M-x sage-start
```

## Configuration Summary

```elisp
;; Storage location (choose one)
(setq sage-project-storage-preset 'emacs-native)   ; Default
(setq sage-project-storage-preset 'standalone)     ; ~/.sage/
(setq sage-project-storage-preset 'claude-shared)  ; ~/.claude/

;; Path encoding
(setq sage-project-encode-dots t)    ; Claude-compatible (default)
(setq sage-project-encode-dots nil)  ; Preserve dots

;; Session behavior
(setq sage-project-auto-load t)      ; Auto-load on start
(setq sage-project-auto-save t)      ; Save after each message
(setq sage-project-max-messages 1000) ; Auto-archive threshold
```

## Security Notes

1. **No PII in session IDs** - Use timestamps + random hash
2. **Local storage only** - Never sync session files to cloud
3. **Sanitize content** - Filter API keys before logging
