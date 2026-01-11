;;; context-example.el --- Example usage of sage-context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; This file demonstrates how to use the sage-context module
;; for token counting and context window management.

;;; Code:

(require 'sage-context)

;;; Example 1: Basic Token Counting

(defun example-token-counting ()
  "Demonstrate basic token counting."
  (let ((messages '(((role . "user")
                     (content . "Hello, can you help me with some code?"))
                    ((role . "assistant")
                     (content . "Of course! I'd be happy to help. What kind of code are you working on?"))
                    ((role . "user")
                     (content . "I need to write a function in Emacs Lisp that processes a list.")))))
    ;; Count tokens
    (let* ((stats (sage-context-tokens-detailed messages))
           (total (alist-get 'total stats))
           (by-role (alist-get 'by-role stats))
           (count (alist-get 'count stats)))
      (message "Messages: %d, Total tokens: %d" count total)
      (message "User tokens: %d" (gethash "user" by-role 0))
      (message "Assistant tokens: %d" (gethash "assistant" by-role 0)))))

;;; Example 2: Context Usage Monitoring

(defun example-usage-monitoring ()
  "Demonstrate context usage monitoring."
  (let ((messages '(((role . "user") (content . "test message"))
                    ((role . "assistant") (content . "response"))))
        (max-tokens 8192))
    ;; Calculate usage percentage
    (let ((usage (sage-context-usage messages max-tokens)))
      (message "Context usage: %.2f%%" (* usage 100))

      ;; Check if compaction needed
      (when (sage-context-needs-compaction-p messages)
        (message "⚠ Context needs compaction!")))))

;;; Example 3: Sliding Window Compaction

(defun example-sliding-window ()
  "Demonstrate sliding window compaction."
  ;; Create a long conversation
  (let ((messages '(((role . "system") (content . "You are a helpful assistant"))
                    ((role . "user") (content . "Question 1"))
                    ((role . "assistant") (content . "Answer 1"))
                    ((role . "user") (content . "Question 2"))
                    ((role . "assistant") (content . "Answer 2"))
                    ((role . "user") (content . "Question 3"))
                    ((role . "assistant") (content . "Answer 3"))
                    ((role . "user") (content . "Question 4"))
                    ((role . "assistant") (content . "Answer 4")))))

    ;; Keep only 4 messages (plus system)
    (let ((sage-context-window-size 4))
      (let ((compacted (sage-context-compact-sliding-window messages)))
        (message "Original: %d messages, Compacted: %d messages"
                 (length messages) (length compacted))

        ;; System message should still be there
        (when (member '((role . "system") (content . "You are a helpful assistant"))
                     compacted)
          (message "✓ System message preserved"))))))

;;; Example 4: Automatic Compaction Setup

(defun example-auto-compaction-setup ()
  "Configure automatic context compaction."
  ;; Configure thresholds
  (setq sage-context-warning-threshold 0.80)
  (setq sage-context-compaction-threshold 0.90)

  ;; Enable auto-compaction
  (setq sage-context-auto-compact t)

  ;; Choose strategy
  (setq sage-context-default-strategy 'sliding-window)
  (setq sage-context-window-size 20)

  (message "Auto-compaction configured: warn at 80%%, compact at 90%%"))

;;; Example 5: Context Status Display

(defun example-status-display ()
  "Show how to display context status."
  (let ((messages '(((role . "user") (content . "Hello!"))
                    ((role . "assistant") (content . "Hi there!")))))
    ;; Display status buffer
    (sage-context-status messages)))

;;; Example 6: Session Tracking

(defun example-session-tracking ()
  "Demonstrate session-level token tracking."
  ;; Reset stats
  (sage-context-reset-stats)

  ;; Track several messages
  (sage-context-track-message '((role . "user") (content . "First message")))
  (sage-context-track-message '((role . "assistant") (content . "First response")))
  (sage-context-track-message '((role . "user") (content . "Second message")))

  ;; Check accumulated stats
  (message "Session stats:")
  (message "  Total tokens: %d" sage-context-total-tokens)
  (message "  Messages: %d" sage-context-message-count)
  (message "  User tokens: %d"
           (gethash "user" sage-context-tokens-by-role 0))
  (message "  Assistant tokens: %d"
           (gethash "assistant" sage-context-tokens-by-role 0)))

;;; Example 7: Provider-Specific Limits

(defun example-provider-limits ()
  "Show provider-specific token limits."
  (message "Provider token limits:")
  (message "  Gemini 1.5 Pro: %s tokens"
           (sage-context-format-number
            (sage-context-get-max-tokens "gemini-1.5-pro")))
  (message "  GPT-4o: %s tokens"
           (sage-context-format-number
            (sage-context-get-max-tokens "gpt-4o")))
  (message "  Llama 3.2: %s tokens"
           (sage-context-format-number
            (sage-context-get-max-tokens "llama3.2"))))

;;; Example 8: Manual Compaction

(defun example-manual-compaction ()
  "Demonstrate manual context compaction."
  (let ((messages (make-list 50 '((role . "user") (content . "test")))))
    ;; Try different strategies
    (message "Original: %d messages" (length messages))

    ;; Sliding window
    (let ((compacted (sage-context-compact messages 'sliding-window)))
      (message "After sliding-window: %d messages" (length compacted)))

    ;; Note: Summarization requires active sage session
    (message "Summarization strategy requires active REPL session")))

;;; Running the Examples

;; Uncomment to run examples:
;; (example-token-counting)
;; (example-usage-monitoring)
;; (example-sliding-window)
;; (example-auto-compaction-setup)
;; (example-status-display)
;; (example-session-tracking)
;; (example-provider-limits)
;; (example-manual-compaction)

(provide 'context-example)
;;; context-example.el ends here
