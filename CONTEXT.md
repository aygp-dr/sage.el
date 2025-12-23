# Context Management Guide

This document provides detailed information about the context and token management features in `gemini-repl-context.el`.

## Overview

The context management module helps you:
- Track token usage to avoid exceeding provider limits
- Automatically compact conversations when they get too long
- Monitor context usage in real-time
- Choose between multiple compaction strategies

## Quick Start

```elisp
;; Load the context module
(require 'gemini-repl-context)

;; Configure thresholds
(setq gemini-repl-context-warning-threshold 0.80)     ; Warn at 80%
(setq gemini-repl-context-compaction-threshold 0.90)  ; Compact at 90%

;; Enable auto-compaction
(setq gemini-repl-context-auto-compact t)

;; Choose strategy
(setq gemini-repl-context-default-strategy 'sliding-window)
```

## Token Counting

### How It Works

Token counting uses a simple heuristic: **characters ÷ 4 ≈ tokens**

This is approximate but works well for English text and code. Actual tokenization varies by model:
- GPT models: ~4 chars/token for English
- Gemini models: Similar to GPT
- Code: Often 2-3 chars/token (more dense)

### Functions

```elisp
;; Estimate tokens for text
(gemini-repl-context-estimate-tokens "Hello world")
;; => 2

;; Count tokens in messages
(let* ((messages '(((role . "user") (content . "Hello"))
                   ((role . "assistant") (content . "Hi there!"))))
       (stats (gemini-repl-context-tokens messages)))
  (alist-get 'total stats))
;; => total token count

;; Calculate usage percentage
(gemini-repl-context-usage messages 8192)
;; => 0.0153 (1.53%)
```

## Context Monitoring

### Automatic Warnings

When context usage exceeds the warning threshold (default: 80%), you'll see:

```
Context usage at 82% (warning threshold: 80%)
```

### Automatic Compaction

When usage exceeds compaction threshold (default: 90%):

```
Context usage at 92%, compacting...
```

### Manual Status Check

```elisp
;; Interactive command
M-x gemini-repl-context-show-status

;; Programmatic
(gemini-repl-context-status messages)
```

This displays:
- Model and max tokens
- Total messages and tokens
- Usage percentage
- Tokens by role (user, assistant, function)
- Threshold status
- Compaction count

## Compaction Strategies

### 1. Sliding Window (Default)

Keeps the N most recent messages, discarding older ones.

**Pros:**
- Fast and simple
- No API calls needed
- Preserves recent context exactly

**Cons:**
- Loses older context completely
- No summary of what was removed

**Configuration:**
```elisp
(setq gemini-repl-context-default-strategy 'sliding-window)
(setq gemini-repl-context-window-size 20)  ; Keep 20 messages
```

**How it works:**
1. Preserves all system messages
2. Keeps N most recent non-system messages
3. Drops everything else

### 2. Summarization

Uses the LLM to summarize old messages into a compact form.

**Pros:**
- Preserves information from old messages
- Can maintain long-term context

**Cons:**
- Requires API call (costs tokens)
- Slower than sliding window
- Summary quality depends on LLM
- Requires active gemini-repl session

**Configuration:**
```elisp
(setq gemini-repl-context-default-strategy 'summarization)
(setq gemini-repl-context-summary-ratio 0.5)  ; Target 50% reduction
```

**How it works:**
1. Identifies messages to summarize
2. Sends to LLM with summarization prompt
3. Replaces old messages with summary as system message
4. Keeps recent messages intact

### 3. Hybrid

Combines both strategies for best results.

**Pros:**
- Summarizes very old messages
- Uses sliding window for recent history
- Balances speed and context preservation

**Cons:**
- More complex
- Still requires API call for summarization

**Configuration:**
```elisp
(setq gemini-repl-context-default-strategy 'hybrid)
```

**How it works:**
1. Takes oldest 25% of messages
2. Summarizes them with LLM
3. Applies sliding window to remaining 75%
4. Combines summary + recent messages

## Provider Token Limits

Pre-configured limits for common models:

| Provider | Model | Max Tokens |
|----------|-------|------------|
| Google Gemini | gemini-1.5-pro | 1,000,000 |
| Google Gemini | gemini-1.5-flash | 128,000 |
| Google Gemini | gemini-2.0-flash-exp | 128,000 |
| OpenAI | gpt-4o | 128,000 |
| OpenAI | gpt-4o-mini | 128,000 |
| OpenAI | gpt-4-turbo | 128,000 |
| Ollama | llama3.2 | 8,192 |
| Ollama | llama3.1 | 128,000 |
| Ollama | mistral | 32,768 |

Unknown models default to 8,192 tokens.

## Session Tracking

The module tracks token usage across your session:

```elisp
;; Track a message
(gemini-repl-context-track-message message)

;; Check session stats
gemini-repl-context-total-tokens        ; Total tokens used
gemini-repl-context-message-count       ; Number of messages
gemini-repl-context-tokens-by-role      ; Hash table by role
gemini-repl-context-compaction-count    ; Times compacted

;; Reset stats
(gemini-repl-context-reset-stats)
```

## Integration with gemini-repl

The context module integrates automatically:

```elisp
;; Auto-check before sending
(gemini-repl-context-maybe-compact gemini-repl-conversation)

;; Check and warn
(gemini-repl-context-check-and-warn gemini-repl-conversation)
```

## Interactive Commands

- `M-x gemini-repl-context-show-status` - Display context status
- `M-x gemini-repl-context-compact-now` - Manually compact conversation
- `M-x gemini-repl-context-reset-stats` - Reset session statistics

## Advanced Usage

### Custom Token Estimation

If you want more accurate token counting:

```elisp
;; Adjust chars-per-token based on your content
(setq gemini-repl-context-chars-per-token 3)  ; For code-heavy conversations
```

### Conditional Compaction

```elisp
;; Only compact if usage > 95%
(when (> (gemini-repl-context-usage messages) 0.95)
  (setq messages (gemini-repl-context-compact messages)))
```

### Custom Compaction Logic

```elisp
(defun my-custom-compact (messages)
  "Custom compaction: keep only user and assistant messages."
  (seq-filter (lambda (msg)
                (member (alist-get 'role msg) '("user" "assistant")))
              messages))
```

## Best Practices

1. **Monitor usage** - Check status occasionally with `gemini-repl-context-show-status`
2. **Enable auto-compact** - Set `gemini-repl-context-auto-compact` to `t`
3. **Choose right strategy**:
   - Short sessions: sliding-window
   - Long sessions with history: summarization or hybrid
4. **Adjust thresholds** - Lower for smaller models (e.g., llama3.2)
5. **Reset between projects** - Call `gemini-repl-context-reset-stats` when switching

## Troubleshooting

### "Context usage at 100%"

Your conversation has exceeded the model's limit.

**Solutions:**
1. Clear conversation: `M-x gemini-repl-clear`
2. Manually compact: `M-x gemini-repl-context-compact-now`
3. Enable auto-compact: `(setq gemini-repl-context-auto-compact t)`

### Compaction not working

**Check:**
1. Is `gemini-repl-context-auto-compact` enabled?
2. Is threshold set correctly?
3. For summarization: Is gemini-repl session active?

### Inaccurate token counts

Token estimation is approximate. For precise counts:
1. Check provider's actual usage in their dashboard
2. Adjust `gemini-repl-context-chars-per-token` based on observations

## Examples

See `examples/context-example.el` for complete working examples.

## API Reference

### Configuration Variables

- `gemini-repl-context-warning-threshold` - Warning threshold (0.0-1.0)
- `gemini-repl-context-compaction-threshold` - Compaction threshold (0.0-1.0)
- `gemini-repl-context-auto-compact` - Enable auto-compaction
- `gemini-repl-context-default-strategy` - Default compaction strategy
- `gemini-repl-context-window-size` - Sliding window size
- `gemini-repl-context-chars-per-token` - Characters per token estimate

### Functions

#### Token Counting
- `(gemini-repl-context-estimate-tokens TEXT)` - Estimate tokens for text
- `(gemini-repl-context-tokens MESSAGES)` - Count tokens in messages
- `(gemini-repl-context-get-max-tokens &optional MODEL)` - Get max tokens for model

#### Usage Calculation
- `(gemini-repl-context-usage MESSAGES &optional MAX-TOKENS)` - Calculate usage %
- `(gemini-repl-context-needs-compaction-p MESSAGES)` - Check if compaction needed
- `(gemini-repl-context-needs-warning-p MESSAGES)` - Check if warning needed

#### Compaction
- `(gemini-repl-context-compact MESSAGES &optional STRATEGY)` - Compact messages
- `(gemini-repl-context-compact-sliding-window MESSAGES)` - Sliding window
- `(gemini-repl-context-compact-summarization MESSAGES)` - Summarization
- `(gemini-repl-context-compact-hybrid MESSAGES)` - Hybrid

#### Monitoring
- `(gemini-repl-context-status &optional MESSAGES)` - Display status
- `(gemini-repl-context-check-and-warn MESSAGES)` - Check and warn if needed

#### Session Tracking
- `(gemini-repl-context-track-message MESSAGE)` - Track message tokens
- `(gemini-repl-context-reset-stats)` - Reset session stats

## See Also

- `README.org` - Main documentation
- `gemini-repl-context.el` - Source code
- `test/gemini-repl-context-test.el` - Test suite
- `examples/context-example.el` - Usage examples
