# JSONL Schema Specification

This document specifies the JSONL schema for sage project conversations and compares it with Claude Code's schema.

## Directory Encoding

Both sage and Claude Code use path encoding for project directories:

| Feature | Sage | Claude Code |
|---------|------|-------------|
| Base Directory | `~/.emacs.d/sage/projects/` | `~/.claude/projects/` |
| Path Separator | `/` → `-` | `/` → `-` |
| Dot Encoding | `.` preserved | `.` → `-` |
| Trailing Character | `-` added | None |

### Examples

| Original Path | Sage Encoding | Claude Encoding |
|---------------|---------------|-----------------|
| `/home/user/project` | `-home-user-project-` | `-home-user-project` |
| `/home/user/github.com/repo` | `-home-user-github.com-repo-` | `-home-user-github-com-repo` |

**Recommendation**: Sage should align with Claude's encoding by also encoding `.` as `-` and removing the trailing `-`.

## JSONL Message Schema

### Sage Schema (Current)

Each line in `conversation.jsonl` is a JSON object:

```json
{
  "role": "user|assistant|system",
  "content": "message content",
  "timestamp": "2026-01-11T22:00:00Z"
}
```

### Claude Code Schema (Reference)

Claude Code uses a comprehensive schema with these fields:

| Field | Type | Description |
|-------|------|-------------|
| `type` | string | "user", "assistant", "file-history-snapshot" |
| `uuid` | string | Unique identifier for this message |
| `parentUuid` | string | UUID of parent message (for threading) |
| `sessionId` | string | Session identifier |
| `cwd` | string | Current working directory |
| `gitBranch` | string | Git branch name |
| `version` | string | Claude Code version (e.g., "2.1.3") |
| `timestamp` | string | ISO 8601 timestamp |
| `userType` | string | "external" for user messages |
| `isSidechain` | boolean | Whether message is in sidechain |
| `message` | object | Contains `role` and `content` |
| `requestId` | string | API request ID (assistant messages) |
| `usage` | object | Token usage stats |
| `thinkingMetadata` | object | Extended thinking settings |
| `todos` | array | Todo list state |

Example user message:
```json
{
  "type": "user",
  "uuid": "bf8f295c-98bb-43bf-a972-9959174591e4",
  "parentUuid": null,
  "sessionId": "96d90d15-4805-4899-889c-7a16bfc522ff",
  "cwd": "/home/user/project",
  "gitBranch": "main",
  "version": "2.1.3",
  "userType": "external",
  "isSidechain": false,
  "timestamp": "2026-01-11T22:00:00.000Z",
  "message": {
    "role": "user",
    "content": "Hello, help me with this code"
  },
  "thinkingMetadata": {"level": "high", "disabled": false},
  "todos": []
}
```

Example assistant message:
```json
{
  "type": "assistant",
  "uuid": "a7d30fa8-cdbd-4391-b13e-2ced4243687a",
  "parentUuid": "bf8f295c-98bb-43bf-a972-9959174591e4",
  "sessionId": "96d90d15-4805-4899-889c-7a16bfc522ff",
  "cwd": "/home/user/project",
  "gitBranch": "main",
  "version": "2.1.3",
  "requestId": "req_011CWzMd5vc7cwkNhz5GovQM",
  "isSidechain": false,
  "timestamp": "2026-01-11T22:00:01.000Z",
  "message": {
    "model": "claude-opus-4-5-20251101",
    "role": "assistant",
    "content": [{"type": "text", "text": "I'll help you..."}],
    "usage": {
      "input_tokens": 1234,
      "output_tokens": 567,
      "cache_read_input_tokens": 15000
    }
  }
}
```

## Sage Extended Schema (Proposed)

To support better tooling and analysis while maintaining simplicity:

```json
{
  "role": "user|assistant|system",
  "content": "message content",
  "timestamp": "2026-01-11T22:00:00Z",
  "uuid": "optional-message-uuid",
  "session_id": "optional-session-uuid"
}
```

### Optional Fields for Tool Calls

```json
{
  "role": "assistant",
  "content": "Let me read that file",
  "timestamp": "2026-01-11T22:00:00Z",
  "tool_calls": [
    {
      "name": "read_file",
      "args": {"path": "example.el"},
      "result": "file contents...",
      "duration_ms": 45,
      "success": true
    }
  ]
}
```

## Security: Token and PII Prevention

### Fields That Should NEVER Be Logged

1. **API Keys/Secrets**
   - Never log environment variables containing keys
   - Never log `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY`
   - Never log authentication tokens

2. **Sensitive File Contents**
   - Filter `.env` file contents
   - Filter `credentials.json`, `secrets.yaml`
   - Filter SSH private keys (`~/.ssh/id_*`)

3. **Personal Identifiable Information (PII)**
   - Full credit card numbers
   - Social security numbers
   - Passwords in plain text

### Implementation Guidelines

```elisp
;; Sanitize sensitive patterns before logging
(defun sage-project--sanitize-content (content)
  "Remove sensitive data from CONTENT before logging."
  (let ((sanitized content))
    ;; Mask API keys
    (setq sanitized (replace-regexp-in-string
                     "\\(ANTHROPIC_API_KEY\\|OPENAI_API_KEY\\|GEMINI_API_KEY\\)=['\"]?[^'\"\n]+['\"]?"
                     "\\1=[REDACTED]"
                     sanitized))
    ;; Mask bearer tokens
    (setq sanitized (replace-regexp-in-string
                     "Bearer [A-Za-z0-9_-]+"
                     "Bearer [REDACTED]"
                     sanitized))
    sanitized))
```

### Token Count Privacy

Token counts (like Claude's `usage` field) are **safe to log** because:
- They are aggregate numbers, not content
- They help with debugging and cost tracking
- They don't reveal conversation content

Sage can optionally add token tracking:

```json
{
  "role": "assistant",
  "content": "response...",
  "timestamp": "...",
  "tokens": {
    "estimated": 150
  }
}
```

## Metadata Schema

Each project also has a `metadata.json`:

```json
{
  "project_dir": "/home/user/project",
  "created_at": "2026-01-11T22:00:00Z",
  "updated_at": "2026-01-11T22:30:00Z",
  "message_count": 42,
  "total_size": 15234
}
```

## Migration Path

To align sage with Claude Code conventions:

1. **Phase 1**: Update `sage-project-encode-path` to encode `.` as `-`
2. **Phase 2**: Add optional `uuid` and `session_id` fields
3. **Phase 3**: Add optional token tracking
4. **Phase 4**: Implement content sanitization

## File Structure

```
~/.emacs.d/sage/projects/
└── -home-user-github-com-repo/        # Note: aligned encoding
    ├── conversation.jsonl              # Current conversation
    ├── metadata.json                   # Project metadata
    └── history/                        # Archived conversations
        ├── 20260110-120000.jsonl
        └── 20260111-090000.jsonl
```

## Validation

A message is valid if:
- `role` is one of: `"user"`, `"assistant"`, `"system"`
- `content` is a non-empty string
- `timestamp` is ISO 8601 format (optional but recommended)

## Compatibility Notes

- Sage JSONL files can be processed with standard `jq` commands
- Each line is independent, supporting streaming reads
- Files are append-only for efficient writes
- Backward compatible: old files without optional fields still work
