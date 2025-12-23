# Gemini REPL Session Persistence

## Overview

The `gemini-repl-session.el` module provides comprehensive session persistence functionality for the Gemini REPL, allowing you to save, load, and manage conversation sessions.

## Features

### 1. Session Data Structure

Each session includes:
- **Name**: Unique session identifier
- **Model**: The AI model used
- **Provider**: AI provider (gemini, ollama, openai)
- **Messages**: Full conversation history with timestamps
- **Statistics**: Message count, token estimates
- **Metadata**: Creation and update timestamps, workspace directory

### 2. Session Management

#### Create a New Session

```elisp
(gemini-repl-session-new "my-session" "gemini-2.0-flash-exp" 'gemini "/path/to/workspace")
```

Or interactively:
```
M-x gemini-repl-session-new RET my-session RET
```

#### Add Messages to Session

```elisp
(gemini-repl-session-add-message "user" "What is the weather?")
(gemini-repl-session-add-message "assistant" "I don't have access to weather data.")
```

#### Save Session

```elisp
(gemini-repl-session-save)  ; Saves current session
```

Or interactively:
```
M-x gemini-repl-session-save RET
```

Sessions are saved to `~/.emacs.d/gemini-repl/sessions/<name>.json`

#### Load Session

```elisp
(gemini-repl-session-load "my-session")
```

Or interactively with completion:
```
M-x gemini-repl-session-load RET my-session RET
```

#### List Sessions

```elisp
(gemini-repl-session-list)  ; Returns list of session names
```

Or interactively:
```
M-x gemini-repl-session-list RET
```

#### Delete Session

```elisp
(gemini-repl-session-delete "my-session")
```

Or interactively:
```
M-x gemini-repl-session-delete RET my-session RET
```

#### Rename Session

```elisp
(gemini-repl-session-rename "old-name" "new-name")
```

Or interactively:
```
M-x gemini-repl-session-rename RET old-name RET new-name RET
```

### 3. Export Functionality

#### Export to JSON

```elisp
(gemini-repl-session-export-json "/path/to/export.json")
```

Or interactively:
```
M-x gemini-repl-session-export-json RET /path/to/export.json RET
```

#### Export to Markdown

```elisp
(gemini-repl-session-export-markdown "/path/to/export.md")
```

Or interactively:
```
M-x gemini-repl-session-export-markdown RET /path/to/export.md RET
```

The Markdown export includes:
- Session metadata (model, provider, timestamps, statistics)
- Full conversation with timestamps for each message
- Formatted for easy reading and sharing

### 4. Session Statistics

```elisp
(gemini-repl-session-stats)  ; Display statistics for current session
```

Or interactively:
```
M-x gemini-repl-session-stats RET
```

Statistics include:
- Session name and model
- Total messages (broken down by user/assistant)
- Estimated token count
- Session duration

### 5. Auto-save Functionality

Sessions can be automatically saved at regular intervals:

```elisp
;; Enable auto-save (default: enabled)
(setq gemini-repl-session-auto-save t)

;; Set auto-save interval (default: 300 seconds / 5 minutes)
(setq gemini-repl-session-auto-save-interval 300)

;; Stop auto-save
(gemini-repl-session-stop-auto-save)
```

Auto-save starts automatically when you create or load a session.

## Configuration

### Session Directory

Customize where sessions are stored:

```elisp
(setq gemini-repl-session-directory
      (expand-file-name "my-gemini-sessions" user-emacs-directory))
```

### Auto-save Settings

```elisp
;; Disable auto-save
(setq gemini-repl-session-auto-save nil)

;; Auto-save every 10 minutes
(setq gemini-repl-session-auto-save-interval 600)
```

## Session File Format

Sessions are saved as JSON files with the following structure:

```json
{
  "name": "my-session",
  "model": "gemini-2.0-flash-exp",
  "provider": "gemini",
  "messages": [
    {
      "role": "user",
      "content": "Hello!",
      "timestamp": "2024-12-23T17:00:00-0800"
    },
    {
      "role": "assistant",
      "content": "Hi! How can I help?",
      "timestamp": "2024-12-23T17:00:05-0800"
    }
  ],
  "created_at": "2024-12-23T17:00:00-0800",
  "updated_at": "2024-12-23T17:00:05-0800",
  "message_count": 2,
  "total_tokens": 24,
  "workspace": "/home/user/projects/myproject"
}
```

## Integration with Gemini REPL

The session module integrates seamlessly with the main Gemini REPL:

```elisp
;; In your init.el or .emacs
(require 'gemini-repl)  ; This automatically loads gemini-repl-session

;; Start REPL
(gemini-repl)

;; Create a named session for this conversation
(gemini-repl-session-new "project-chat")

;; Your conversations are now being tracked in the session
;; Messages are automatically added as you chat

;; Save when done
(gemini-repl-session-save)

;; Resume later
(gemini-repl)
(gemini-repl-session-load "project-chat")
```

## Helper Functions

### Get Current Session

```elisp
(gemini-repl-session-current)  ; Returns current session or nil
```

### Get Session Messages

```elisp
(gemini-repl-session-get-messages)  ; Returns messages from current session
```

### Set Session Messages

```elisp
(gemini-repl-session-set-messages messages)  ; Updates current session messages
```

## Example Workflow

```elisp
;; 1. Start a new project conversation
(gemini-repl)
(gemini-repl-session-new "rust-learning")

;; 2. Chat with the AI (messages automatically tracked)
;; ... conversation happens ...

;; 3. Save the session
(gemini-repl-session-save)

;; 4. Later, resume the conversation
(gemini-repl)
(gemini-repl-session-load "rust-learning")

;; 5. Export for sharing or documentation
(gemini-repl-session-export-markdown "~/Documents/rust-learning-session.md")

;; 6. Check statistics
(gemini-repl-session-stats)
;; => Session: rust-learning | Model: gemini-2.0-flash-exp | Messages: 24 | Tokens: ~1200
```

## Advanced Usage

### Custom Session Processing

```elisp
;; Load a session and process its messages
(let* ((session (gemini-repl-session-load "my-session"))
       (messages (gemini-repl-session-messages session)))
  (dolist (msg messages)
    (when (string= (alist-get 'role msg) "user")
      (message "User said: %s" (alist-get 'content msg)))))
```

### Bulk Export

```elisp
;; Export all sessions to markdown
(dolist (session-name (gemini-repl-session-list-names))
  (let ((session (gemini-repl-session-load session-name)))
    (gemini-repl-session-export-markdown
     (format "~/exports/%s.md" session-name))))
```

### Session Cleanup

```elisp
;; Delete old sessions (example: sessions not updated in 30 days)
(dolist (session-name (gemini-repl-session-list-names))
  (let* ((session (gemini-repl-session-load session-name))
         (updated (gemini-repl-session-updated-at session))
         (updated-time (parse-iso8601-time-string updated))
         (age-days (/ (float-time (time-since updated-time)) 86400)))
    (when (> age-days 30)
      (message "Deleting old session: %s (%.0f days old)" session-name age-days)
      (gemini-repl-session-delete session-name))))
```

## Troubleshooting

### Sessions not saving

Check that the session directory exists and is writable:

```elisp
(file-writable-p gemini-repl-session-directory)
```

### Auto-save not working

Verify auto-save is enabled and check the timer:

```elisp
gemini-repl-session-auto-save              ; Should be t
gemini-repl-session--auto-save-timer       ; Should be a timer object
```

### Loading old session fails

Ensure the session file is valid JSON:

```elisp
(let ((file (expand-file-name "session-name.json" gemini-repl-session-directory)))
  (with-temp-buffer
    (insert-file-contents file)
    (json-read)))
```

## See Also

- `gemini-repl.el` - Main REPL module
- `gemini-repl-memory.el` - Fact memory system
- `gemini-repl-tools.el` - Tool/function calling system
