;;; sage-context.el --- Context and token management for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, llm
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Token counting and context management for sage.
;;
;; Features:
;; - Token estimation (rough heuristic: chars/4)
;; - Token tracking by role (user, assistant, function)
;; - Context usage percentage calculation
;; - Automatic warnings at configurable thresholds
;; - Multiple compaction strategies (sliding window, summarization)
;; - Provider-specific token limits
;;
;; Usage:
;;   (require 'sage-context)
;;   (sage-context-tokens messages)
;;   (sage-context-usage messages max-tokens)
;;   (sage-context-compact messages 'sliding-window)
;;   (sage-context-status)

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup sage-context nil
  "Context and token management for sage."
  :group 'sage
  :prefix "sage-context-")

(defcustom sage-context-warning-threshold 0.80
  "Warn when context usage exceeds this percentage (0.0-1.0)."
  :type 'float
  :group 'sage-context)

(defcustom sage-context-compaction-threshold 0.90
  "Trigger automatic compaction at this percentage (0.0-1.0)."
  :type 'float
  :group 'sage-context)

(defcustom sage-context-auto-compact t
  "Automatically compact context when threshold is reached."
  :type 'boolean
  :group 'sage-context)

(defcustom sage-context-default-strategy 'sliding-window
  "Default compaction strategy.
Options:
  'sliding-window - Drop oldest messages
  'summarization  - Use LLM to summarize old messages
  'hybrid         - Combine both strategies"
  :type '(choice (const :tag "Sliding Window" sliding-window)
                 (const :tag "Summarization" summarization)
                 (const :tag "Hybrid" hybrid))
  :group 'sage-context)

(defcustom sage-context-window-size 20
  "Number of messages to keep in sliding window strategy."
  :type 'integer
  :group 'sage-context)

(defcustom sage-context-summary-ratio 0.5
  "Target token ratio after summarization (0.0-1.0)."
  :type 'float
  :group 'sage-context)

(defcustom sage-context-chars-per-token 4
  "Estimated characters per token for rough counting.
This is a heuristic - actual tokenization varies by model.
Common values: 3-4 for English text, 2-3 for code."
  :type 'integer
  :group 'sage-context)

(defcustom sage-context-provider-limits
  '((gemini-1.5-pro . 1000000)
    (gemini-1.5-flash . 128000)
    (gemini-2.0-flash-exp . 128000)
    (gpt-4o . 128000)
    (gpt-4o-mini . 128000)
    (gpt-4-turbo . 128000)
    (llama3.2 . 8192)
    (llama3.1 . 128000)
    (mistral . 32768)
    (qwen2.5 . 32768)
    (default . 8192))
  "Token limits by model.
Each entry is (MODEL-SYMBOL . MAX-TOKENS)."
  :type '(alist :key-type symbol :value-type integer)
  :group 'sage-context)

;;; Variables

(defvar sage-context-total-tokens 0
  "Total tokens used in current session.")

(defvar sage-context-tokens-by-role (make-hash-table :test 'equal)
  "Hash table tracking tokens by role (user, assistant, function).")

(defvar sage-context-message-count 0
  "Total number of messages in current session.")

(defvar sage-context-compaction-count 0
  "Number of times context has been compacted.")

(defvar sage-context-last-warning nil
  "Timestamp of last context warning.")

;;; Core Functions

(defun sage-context-estimate-tokens (text)
  "Estimate token count for TEXT using character-based heuristic.
This is a rough approximation - actual tokenization varies by model."
  (if (stringp text)
      (/ (length text) sage-context-chars-per-token)
    0))

(defun sage-context-tokens (messages)
  "Count total tokens in MESSAGES.
MESSAGES is a list of message alists with 'role and 'content keys.
Returns alist with:
  - total: total token count
  - by-role: hash table of tokens per role
  - count: number of messages"
  (let ((total 0)
        (by-role (make-hash-table :test 'equal))
        (count 0))
    (dolist (msg messages)
      (let* ((role (alist-get 'role msg))
             (content (alist-get 'content msg))
             (tokens (sage-context-estimate-tokens content)))
        (cl-incf total tokens)
        (cl-incf (gethash role by-role 0) tokens)
        (cl-incf count)))
    `((total . ,total)
      (by-role . ,by-role)
      (count . ,count))))

(defun sage-context-get-max-tokens (&optional model)
  "Get maximum token limit for MODEL.
If MODEL is nil, uses current provider's default model.
Returns token limit from `sage-context-provider-limits'."
  (let* ((model-name (or model
                        (and (boundp 'sage-model)
                             (symbol-name sage-model))
                        (and (boundp 'sage-provider)
                             (pcase sage-provider
                               ('gemini "gemini-2.0-flash-exp")
                               ('ollama "llama3.2")
                               ('openai "gpt-4o")
                               (_ "default")))))
         (model-sym (intern model-name)))
    (or (alist-get model-sym sage-context-provider-limits)
        (alist-get 'default sage-context-provider-limits)
        8192)))

(defun sage-context-usage (messages &optional max-tokens)
  "Calculate context usage percentage for MESSAGES.
MAX-TOKENS defaults to current provider's limit.
Returns float between 0.0 and 1.0."
  (let* ((max (or max-tokens (sage-context-get-max-tokens)))
         (stats (sage-context-tokens messages))
         (total (alist-get 'total stats)))
    (/ (float total) max)))

(defun sage-context-needs-compaction-p (messages &optional max-tokens)
  "Check if MESSAGES exceed compaction threshold.
MAX-TOKENS overrides model-based detection.
Returns non-nil if compaction is needed."
  (>= (sage-context-usage messages max-tokens)
      sage-context-compaction-threshold))

(defun sage-context-needs-warning-p (messages)
  "Check if MESSAGES exceed warning threshold.
Returns non-nil if warning should be shown."
  (and (>= (sage-context-usage messages)
           sage-context-warning-threshold)
       (< (sage-context-usage messages)
          sage-context-compaction-threshold)))

;;; Compaction Strategies

(defun sage-context-compact-sliding-window (messages)
  "Compact MESSAGES using sliding window strategy.
Keeps most recent N messages (N = `sage-context-window-size').
Always preserves system messages."
  (let ((system-msgs (seq-filter
                      (lambda (msg) (string= (alist-get 'role msg) "system"))
                      messages))
        (other-msgs (seq-filter
                     (lambda (msg) (not (string= (alist-get 'role msg) "system")))
                     messages)))
    (append system-msgs
            (seq-take (nreverse other-msgs)
                     sage-context-window-size))))

(defun sage-context-compact-summarization (messages)
  "Compact MESSAGES using LLM summarization.
Summarizes older messages, keeping recent ones intact.
Requires active sage session."
  (if (not (and (boundp 'sage-conversation)
                (fboundp 'sage--request)))
      (progn
        (message "Summarization requires active sage session, falling back to sliding window")
        (sage-context-compact-sliding-window messages))
    (let* ((split-point (max 1 (- (length messages)
                                 sage-context-window-size)))
           (to-summarize (seq-take messages split-point))
           (to-keep (seq-drop messages split-point))
           (summary nil)
           (done nil))
      ;; Request summary from LLM
      (sage--request
       `(((role . "user")
          (content . ,(sage-context--summarization-prompt to-summarize))))
       nil
       (lambda (response error)
         (setq summary
               (if error
                   (format "Error summarizing: %s" error)
                 (alist-get 'content response)))
         (setq done t)))
      ;; Wait for summary
      (while (not done)
        (sleep-for 0.1))
      ;; Combine summary + kept messages
      (append `(((role . "system")
                 (content . ,(format "Previous conversation summary:\n%s" summary))))
              to-keep))))

(defun sage-context-compact-hybrid (messages)
  "Compact MESSAGES using hybrid strategy.
Summarizes very old messages, uses sliding window for recent ones."
  (if (< (length messages) (* 2 sage-context-window-size))
      ;; Not enough messages for hybrid, use sliding window
      (sage-context-compact-sliding-window messages)
    (let* ((quarter (/ (length messages) 4))
           (very-old (seq-take messages quarter))
           (rest (seq-drop messages quarter)))
      ;; Summarize very old, then sliding window on the rest
      (if (not (and (boundp 'sage-conversation)
                    (fboundp 'sage--request)))
          (sage-context-compact-sliding-window messages)
        (let ((summary nil)
              (done nil))
          (sage--request
           `(((role . "user")
              (content . ,(sage-context--summarization-prompt very-old))))
           nil
           (lambda (response error)
             (setq summary
                   (if error
                       (format "Error: %s" error)
                     (alist-get 'content response)))
             (setq done t)))
          (while (not done)
            (sleep-for 0.1))
          (append `(((role . "system")
                     (content . ,(format "Earlier conversation summary:\n%s" summary))))
                  (sage-context-compact-sliding-window rest)))))))

(defun sage-context--summarization-prompt (messages)
  "Generate summarization prompt for MESSAGES."
  (format "Please provide a concise summary of the following conversation, \
preserving key facts, decisions, and context. Be brief but comprehensive.

Conversation to summarize:
%s

Summary:" (sage-context--format-messages-for-summary messages)))

(defun sage-context--format-messages-for-summary (messages)
  "Format MESSAGES as readable text for summarization."
  (mapconcat
   (lambda (msg)
     (format "[%s]: %s"
             (alist-get 'role msg)
             (truncate-string-to-width (alist-get 'content msg) 500 nil nil "...")))
   messages
   "\n"))

(defun sage-context-compact (messages &optional strategy)
  "Compact MESSAGES using STRATEGY.
STRATEGY defaults to `sage-context-default-strategy'.
Available strategies:
  - 'sliding-window: Keep most recent N messages
  - 'summarization: Summarize old messages with LLM
  - 'hybrid: Combine both approaches

Returns compacted message list."
  (let ((strat (or strategy sage-context-default-strategy)))
    (cl-incf sage-context-compaction-count)
    (message "Compacting context using %s strategy..." strat)
    (pcase strat
      ('sliding-window (sage-context-compact-sliding-window messages))
      ('summarization (sage-context-compact-summarization messages))
      ('hybrid (sage-context-compact-hybrid messages))
      (_ (error "Unknown compaction strategy: %s" strat)))))

;;; Monitoring and Status

(defun sage-context-check-and-warn (messages)
  "Check MESSAGES and warn/compact if needed.
Called automatically if `sage-context-auto-compact' is non-nil."
  (let ((usage (sage-context-usage messages)))
    (cond
     ;; Needs compaction
     ((>= usage sage-context-compaction-threshold)
      (when sage-context-auto-compact
        (message "Context usage at %.0f%%, compacting..." (* usage 100))
        (sage-context-compact messages)))
     ;; Just warning
     ((and (>= usage sage-context-warning-threshold)
           (or (null sage-context-last-warning)
               (> (float-time (time-since sage-context-last-warning)) 60)))
      (setq sage-context-last-warning (current-time))
      (message "Context usage at %.0f%% (warning threshold: %.0f%%)"
               (* usage 100)
               (* sage-context-warning-threshold 100))))))

(defun sage-context-status (&optional messages)
  "Display context status for MESSAGES.
If MESSAGES is nil, uses current conversation."
  (interactive)
  (let* ((msgs (or messages
                   (and (boundp 'sage-conversation)
                        sage-conversation)
                   '()))
         (stats (sage-context-tokens msgs))
         (total (alist-get 'total stats))
         (by-role (alist-get 'by-role stats))
         (count (alist-get 'count stats))
         (max-tokens (sage-context-get-max-tokens))
         (usage-pct (* 100 (/ (float total) max-tokens))))
    (with-output-to-temp-buffer "*sage-context-status*"
      (princ "=== Sage Context Status ===\n\n")
      (princ (format "Model: %s\n"
                     (or (and (boundp 'sage-model) sage-model)
                         "default")))
      (princ (format "Max Tokens: %s\n" (format-number max-tokens)))
      (princ (format "Total Messages: %d\n\n" count))
      (princ "--- Token Usage ---\n")
      (princ (format "Total Tokens: %s\n" (format-number total)))
      (princ (format "Usage: %.1f%% of maximum\n" usage-pct))
      (princ (format "Remaining: %s tokens\n\n" (format-number (- max-tokens total))))
      (princ "--- Tokens by Role ---\n")
      (maphash (lambda (role tokens)
                 (princ (format "%s: %s (%.1f%%)\n"
                               (capitalize role)
                               (format-number tokens)
                               (* 100 (/ (float tokens) total)))))
               by-role)
      (princ "\n--- Thresholds ---\n")
      (princ (format "Warning: %.0f%% %s\n"
                     (* 100 sage-context-warning-threshold)
                     (if (>= (/ (float total) max-tokens)
                            sage-context-warning-threshold)
                         "⚠ EXCEEDED" "✓")))
      (princ (format "Compaction: %.0f%% %s\n"
                     (* 100 sage-context-compaction-threshold)
                     (if (>= (/ (float total) max-tokens)
                            sage-context-compaction-threshold)
                         "⚠ EXCEEDED" "✓")))
      (princ (format "Auto-compact: %s\n"
                     (if sage-context-auto-compact "enabled" "disabled")))
      (princ (format "Compaction Strategy: %s\n"
                     sage-context-default-strategy))
      (princ (format "\nCompactions this session: %d\n"
                     sage-context-compaction-count))
      (when (> usage-pct 80)
        (princ "\n⚠ WARNING: Context usage is high!\n")
        (princ "Consider clearing old messages or enabling auto-compaction.\n")))))

(defun sage-context-reset-stats ()
  "Reset all context tracking statistics."
  (interactive)
  (setq sage-context-total-tokens 0
        sage-context-message-count 0
        sage-context-compaction-count 0
        sage-context-last-warning nil
        sage-context-tokens-by-role (make-hash-table :test 'equal))
  (message "Context statistics reset"))

;;; Utility Functions

(defun sage-context-format-number (num)
  "Format NUM with thousands separators."
  (let ((str (number-to-string num))
        (result ""))
    (while (> (length str) 3)
      (setq result (concat "," (substring str -3) result)
            str (substring str 0 -3)))
    (concat str result)))

(defun format-number (num)
  "Format NUM with thousands separators (wrapper for compatibility)."
  (sage-context-format-number num))

;;; Integration Hooks

(defun sage-context-track-message (message)
  "Track token usage for MESSAGE.
Updates session statistics."
  (let* ((role (alist-get 'role message))
         (tokens (sage-context-estimate-tokens
                 (alist-get 'content message))))
    (cl-incf sage-context-total-tokens tokens)
    (cl-incf (gethash role sage-context-tokens-by-role 0) tokens)
    (cl-incf sage-context-message-count)))

(defun sage-context-maybe-compact (messages)
  "Check MESSAGES and auto-compact if needed.
Returns possibly compacted message list."
  (if (and sage-context-auto-compact
           (sage-context-needs-compaction-p messages))
      (progn
        (message "Auto-compacting context...")
        (sage-context-compact messages))
    messages))

;;; Interactive Commands

;;;###autoload
(defun sage-context-show-status ()
  "Show context status in a buffer."
  (interactive)
  (sage-context-status))

;;;###autoload
(defun sage-context-compact-now (&optional strategy)
  "Manually trigger context compaction with STRATEGY.
If called interactively, prompts for strategy."
  (interactive
   (list (intern (completing-read
                  "Compaction strategy: "
                  '("sliding-window" "summarization" "hybrid")
                  nil t nil nil
                  (symbol-name sage-context-default-strategy)))))
  (if (and (boundp 'sage-conversation)
           sage-conversation)
      (progn
        (setq sage-conversation
              (sage-context-compact sage-conversation strategy))
        (message "Context compacted using %s strategy" strategy))
    (message "No active conversation to compact")))

(provide 'sage-context)
;;; sage-context.el ends here
