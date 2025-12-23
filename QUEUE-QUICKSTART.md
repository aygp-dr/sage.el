# Gemini REPL Queue System - Quick Start Guide

The `gemini-repl-queue` provides file-based inter-agent communication for Emacs.

## Installation

```elisp
;; Add to your init.el
(add-to-list 'load-path "/home/jwalsh/ghq/github.com/aygp-dr/gemini-repl-010")
(require 'gemini-repl-queue)
```

## Directory Structure

The queue system uses three directories under `~/.emacs.d/gemini-repl/queues/`:

```
~/.emacs.d/gemini-repl/queues/
├── input/    # Incoming requests (JSON files)
├── output/   # Responses from processing
└── archive/  # Processed and completed requests
```

These directories are created automatically on first use.

## Basic Usage

### 1. Submit a Request

```elisp
;; Interactive
M-x gemini-repl-queue-submit
Type: ping
Content: Hello

;; Programmatic
(gemini-repl-queue-submit 'ping "Hello from Emacs")
;; => "20241223-170945-a1b2c3d4" (request ID)
```

### 2. Enable Watch Mode

Watch mode automatically processes incoming requests:

```elisp
;; Enable
M-x gemini-repl-queue-watch-mode
;; or
(gemini-repl-queue-watch-mode 1)

;; Disable
(gemini-repl-queue-watch-mode -1)
```

### 3. Check Status

```elisp
M-x gemini-repl-queue-status
```

Shows:
- Current queue counts (pending, ready, archived)
- Session statistics
- Watch mode status

### 4. Poll Manually

Without watch mode, poll manually:

```elisp
(let ((request (gemini-repl-queue-poll)))
  (when request
    (message "Found: %s" (alist-get 'content request))))
```

### 5. Custom Handlers

Register handlers for specific request types:

```elisp
(gemini-repl-queue-register-handler
 'my-task
 (lambda (request)
   (let ((content (alist-get 'content request)))
     ;; Process the request
     (cons 'success (format "Processed: %s" content)))))

;; Submit request
(gemini-repl-queue-submit 'my-task "Do something")
```

## Request/Response Format

### Request (JSON)

```json
{
  "id": "20241223-170945-a1b2c3d4",
  "type": "prompt",
  "content": "What is 2+2?",
  "context": {
    "priority": "high",
    "tags": ["math", "simple"]
  },
  "created_at": "2024-12-23T17:09:45+0000"
}
```

### Response (JSON)

```json
{
  "request_id": "20241223-170945-a1b2c3d4",
  "status": "success",
  "content": "4",
  "metadata": {},
  "processed_at": "2024-12-23T17:09:47+0000"
}
```

## Built-in Request Types

### 1. Ping

Simple echo for testing:

```elisp
(gemini-repl-queue-submit 'ping "test")
;; Response: "pong: 2024-12-23T17:09:45+0000"
```

### 2. Prompt

Execute AI prompt using `gemini-repl`:

```elisp
(gemini-repl-queue-submit 'prompt "Explain recursion")
;; Response: AI-generated explanation
```

### 3. Command

Generic command type (requires custom handler):

```elisp
(gemini-repl-queue-submit 'command "restart-service")
```

## Common Workflows

### Multi-Agent Communication

```elisp
;; Agent A submits work
(gemini-repl-queue-submit
 'analyze-code
 (buffer-string)
 '((file . "mycode.el")))

;; Agent B processes (with watch mode enabled)
(gemini-repl-queue-register-handler
 'analyze-code
 (lambda (request)
   (let* ((code (alist-get 'content request))
          (file (alist-get 'file (alist-get 'context request))))
     (cons 'success (format "Analyzed %s" file)))))
```

### Async Task Processing

```elisp
;; Submit long-running task
(gemini-repl-queue-submit 'heavy-task "large dataset")

;; Handler processes asynchronously (won't block Emacs)
(gemini-repl-queue-register-handler
 'heavy-task
 (lambda (request)
   (sleep-for 5) ; Simulate long processing
   (cons 'success "Task complete")))
```

### Pipeline Processing

```elisp
;; Stage 1
(gemini-repl-queue-register-handler
 'step1
 (lambda (request)
   ;; Process and queue next step
   (gemini-repl-queue-submit 'step2 "processed-data")
   (cons 'success "Step 1 done")))

;; Stage 2
(gemini-repl-queue-register-handler
 'step2
 (lambda (request)
   (cons 'success "Pipeline complete")))

;; Start pipeline
(gemini-repl-queue-submit 'step1 "raw-data")
```

## Examples

### Run Demo

```elisp
;; Load demos
(load-file "examples/queue-demo.el")

;; Interactive menu
M-x queue-demo-menu

;; Or run specific demos
(queue-demo-basic)
(queue-demo-custom-handler)
(queue-demo-watch-mode)
```

### Multi-Agent Workflow

```elisp
;; Load multi-agent example
(load-file "examples/multi-agent-workflow.el")

;; Setup agents
M-x multi-agent-workflow-setup

;; Review current file
M-x multi-agent-workflow-review-current-buffer

;; Run demo
M-x multi-agent-workflow-demo
```

## Configuration

### Customize Directory

```elisp
(setq gemini-repl-queue-directory
      (expand-file-name "~/my-queues"))
```

### Polling Interval

When file-notify is unavailable:

```elisp
(setq gemini-repl-queue-poll-interval 2.0) ; seconds
```

### Auto-Processing

```elisp
;; Enable/disable automatic processing in watch mode
(setq gemini-repl-queue-auto-process t)
```

### Archive Retention

```elisp
;; Days to keep archived requests
(setq gemini-repl-queue-retention-days 7)

;; Manually clean old archives
(gemini-repl-queue-cleanup-archives)
```

## Monitoring

### View Status

```elisp
M-x gemini-repl-queue-status
```

### Check Directories

```elisp
;; List pending requests
(directory-files gemini-repl-queue--input-dir nil "\\.json$")

;; List responses
(directory-files gemini-repl-queue--output-dir nil "\\.json$")

;; List archives
(directory-files gemini-repl-queue--archive-dir nil "\\.json$")
```

### Statistics

Session statistics tracked automatically:
- Submitted requests
- Processed requests
- Errors
- Archived requests

## Troubleshooting

### File-notify Not Available

The system automatically falls back to polling. To check:

```elisp
(featurep 'filenotify) ; Should return t if available
```

### Requests Not Processing

1. Ensure watch mode is enabled: `M-x gemini-repl-queue-status`
2. Check handlers are registered
3. Look for errors in `*Messages*` buffer
4. Try manual poll: `(gemini-repl-queue-poll)`

### Permission Issues

```elisp
;; Check directory permissions
(file-writable-p gemini-repl-queue-directory)
```

## Integration with External Tools

### From Shell

```bash
# Submit request
cat > ~/.emacs.d/gemini-repl/queues/input/my-request.json << EOF
{
  "id": "shell-$(date +%s)",
  "type": "prompt",
  "content": "What is the time?",
  "created_at": "$(date -Iseconds)"
}
EOF

# Read response (wait for Emacs to process)
sleep 2
cat ~/.emacs.d/gemini-repl/queues/output/shell-*.json
```

### From Python

```python
import json
from pathlib import Path
from datetime import datetime

queue_dir = Path.home() / ".emacs.d/gemini-repl/queues"

# Submit request
request = {
    "id": f"python-{datetime.now().timestamp()}",
    "type": "prompt",
    "content": "Explain Python decorators",
    "created_at": datetime.now().isoformat()
}

input_file = queue_dir / "input" / f"{request['id']}.json"
input_file.write_text(json.dumps(request))

# Wait and read response
import time
time.sleep(2)

output_file = queue_dir / "output" / f"{request['id']}.json"
if output_file.exists():
    response = json.loads(output_file.read_text())
    print(response['content'])
```

## Testing

Run the test suite:

```elisp
;; Load tests
(load-file "test/gemini-repl-queue-test.el")

;; Run all tests
M-x ert RET t RET

;; Run specific test
M-x ert RET gemini-repl-queue-test-submit-request RET
```

## Full Documentation

See [QUEUE.org](QUEUE.org) for complete documentation including:
- Advanced features
- Performance benchmarks
- Security considerations
- API reference
- Detailed examples

## Quick Reference

| Function | Description |
|----------|-------------|
| `gemini-repl-queue-submit` | Submit a request |
| `gemini-repl-queue-poll` | Poll for pending request |
| `gemini-repl-queue-respond` | Write response |
| `gemini-repl-queue-archive` | Archive completed request |
| `gemini-repl-queue-status` | Show queue status |
| `gemini-repl-queue-watch-mode` | Enable/disable auto-processing |
| `gemini-repl-queue-register-handler` | Register custom handler |
| `gemini-repl-queue-cleanup-archives` | Remove old archives |
| `gemini-repl-queue-send-to-agent` | Send to specific agent |
| `gemini-repl-queue-broadcast` | Broadcast to all agents |

## Next Steps

1. **Enable watch mode** for automatic processing
2. **Register custom handlers** for your use cases
3. **Try examples** in `examples/queue-demo.el`
4. **Read full docs** in `QUEUE.org`
5. **Explore multi-agent workflows** in `examples/multi-agent-workflow.el`
