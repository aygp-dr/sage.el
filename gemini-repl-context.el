;;; gemini-repl-context.el --- Context and token management for gemini-repl -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, llm
;; URL: https://github.com/aygp-dr/gemini-repl-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Token counting and context management for gemini-repl.
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
;;   (require 'gemini-repl-context)
;;   (gemini-repl-context-tokens messages)
;;   (gemini-repl-context-usage messages max-tokens)
;;   (gemini-repl-context-compact messages 'sliding-window)
;;   (gemini-repl-context-status)

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup gemini-repl-context nil
  "Context and token management for gemini-repl."
  :group 'gemini-repl
  :prefix "gemini-repl-context-")

(defcustom gemini-repl-context-warning-threshold 0.80
  "Warn when context usage exceeds this percentage (0.0-1.0)."
  :type 'float
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-compaction-threshold 0.90
  "Trigger automatic compaction at this percentage (0.0-1.0)."
  :type 'float
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-auto-compact t
  "Automatically compact context when threshold is reached."
  :type 'boolean
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-default-strategy 'sliding-window
  "Default compaction strategy.
Options:
  'sliding-window - Drop oldest messages
  'summarization  - Use LLM to summarize old messages
  'hybrid         - Combine both strategies"
  :type '(choice (const :tag "Sliding Window" sliding-window)
                 (const :tag "Summarization" summarization)
                 (const :tag "Hybrid" hybrid))
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-window-size 20
  "Number of messages to keep in sliding window strategy."
  :type 'integer
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-summary-ratio 0.5
  "Target token ratio after summarization (0.0-1.0)."
  :type 'float
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-chars-per-token 4
  "Estimated characters per token for rough counting.
This is a heuristic - actual tokenization varies by model.
Common values: 3-4 for English text, 2-3 for code."
  :type 'integer
  :group 'gemini-repl-context)

(defcustom gemini-repl-context-provider-limits
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
  :group 'gemini-repl-context)

;;; Variables

(defvar gemini-repl-context-total-tokens 0
  "Total tokens used in current session.")

(defvar gemini-repl-context-tokens-by-role (make-hash-table :test 'equal)
  "Hash table tracking tokens by role (user, assistant, function).")

(defvar gemini-repl-context-message-count 0
  "Total number of messages in current session.")

(defvar gemini-repl-context-compaction-count 0
  "Number of times context has been compacted.")

(defvar gemini-repl-context-last-warning nil
  "Timestamp of last context warning.")

;;; Core Functions

(defun gemini-repl-context-estimate-tokens (text)
  "Estimate token count for TEXT using character-based heuristic.
This is a rough approximation - actual tokenization varies by model."
  (if (stringp text)
      (/ (length text) gemini-repl-context-chars-per-token)
    0))

(defun gemini-repl-context-tokens (messages)
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
             (tokens (gemini-repl-context-estimate-tokens content)))
        (cl-incf total tokens)
        (cl-incf (gethash role by-role 0) tokens)
        (cl-incf count)))
    `((total . ,total)
      (by-role . ,by-role)
      (count . ,count))))

(defun gemini-repl-context-get-max-tokens (&optional model)
  "Get maximum token limit for MODEL.
If MODEL is nil, uses current provider's default model.
Returns token limit from `gemini-repl-context-provider-limits'."
  (let* ((model-name (or model
                        (and (boundp 'gemini-repl-model)
                             (symbol-name gemini-repl-model))
                        (and (boundp 'gemini-repl-provider)
                             (pcase gemini-repl-provider
                               ('gemini "gemini-2.0-flash-exp")
                               ('ollama "llama3.2")
                               ('openai "gpt-4o")
                               (_ "default")))))
         (model-sym (intern model-name)))
    (or (alist-get model-sym gemini-repl-context-provider-limits)
        (alist-get 'default gemini-repl-context-provider-limits)
        8192)))

(defun gemini-repl-context-usage (messages &optional max-tokens)
  "Calculate context usage percentage for MESSAGES.
MAX-TOKENS defaults to current provider's limit.
Returns float between 0.0 and 1.0."
  (let* ((max (or max-tokens (gemini-repl-context-get-max-tokens)))
         (stats (gemini-repl-context-tokens messages))
         (total (alist-get 'total stats)))
    (/ (float total) max)))

(defun gemini-repl-context-needs-compaction-p (messages)
  "Check if MESSAGES exceed compaction threshold.
Returns non-nil if compaction is needed."
  (>= (gemini-repl-context-usage messages)
      gemini-repl-context-compaction-threshold))

(defun gemini-repl-context-needs-warning-p (messages)
  "Check if MESSAGES exceed warning threshold.
Returns non-nil if warning should be shown."
  (and (>= (gemini-repl-context-usage messages)
           gemini-repl-context-warning-threshold)
       (< (gemini-repl-context-usage messages)
          gemini-repl-context-compaction-threshold)))

;;; Compaction Strategies

(defun gemini-repl-context-compact-sliding-window (messages)
  "Compact MESSAGES using sliding window strategy.
Keeps most recent N messages (N = `gemini-repl-context-window-size').
Always preserves system messages."
  (let ((system-msgs (seq-filter
                      (lambda (msg) (string= (alist-get 'role msg) "system"))
                      messages))
        (other-msgs (seq-filter
                     (lambda (msg) (not (string= (alist-get 'role msg) "system")))
                     messages)))
    (append system-msgs
            (seq-take (nreverse other-msgs)
                     gemini-repl-context-window-size))))

(defun gemini-repl-context-compact-summarization (messages)
  "Compact MESSAGES using LLM summarization.
Summarizes older messages, keeping recent ones intact.
Requires active gemini-repl session."
  (if (not (and (boundp 'gemini-repl-conversation)
                (fboundp 'gemini-repl--request)))
      (progn
        (message "Summarization requires active gemini-repl session, falling back to sliding window")
        (gemini-repl-context-compact-sliding-window messages))
    (let* ((split-point (max 1 (- (length messages)
                                 gemini-repl-context-window-size)))
           (to-summarize (seq-take messages split-point))
           (to-keep (seq-drop messages split-point))
           (summary nil)
           (done nil))
      ;; Request summary from LLM
      (gemini-repl--request
       `(((role . "user")
          (content . ,(gemini-repl-context--summarization-prompt to-summarize))))
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

(defun gemini-repl-context-compact-hybrid (messages)
  "Compact MESSAGES using hybrid strategy.
Summarizes very old messages, uses sliding window for recent ones."
  (if (< (length messages) (* 2 gemini-repl-context-window-size))
      ;; Not enough messages for hybrid, use sliding window
      (gemini-repl-context-compact-sliding-window messages)
    (let* ((quarter (/ (length messages) 4))
           (very-old (seq-take messages quarter))
           (rest (seq-drop messages quarter)))
      ;; Summarize very old, then sliding window on the rest
      (if (not (and (boundp 'gemini-repl-conversation)
                    (fboundp 'gemini-repl--request)))
          (gemini-repl-context-compact-sliding-window messages)
        (let ((summary nil)
              (done nil))
          (gemini-repl--request
           `(((role . "user")
              (content . ,(gemini-repl-context--summarization-prompt very-old))))
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
                  (gemini-repl-context-compact-sliding-window rest)))))))

(defun gemini-repl-context--summarization-prompt (messages)
  "Generate summarization prompt for MESSAGES."
  (format "Please provide a concise summary of the following conversation, \
preserving key facts, decisions, and context. Be brief but comprehensive.

Conversation to summarize:
%s

Summary:" (gemini-repl-context--format-messages-for-summary messages)))

(defun gemini-repl-context--format-messages-for-summary (messages)
  "Format MESSAGES as readable text for summarization."
  (mapconcat
   (lambda (msg)
     (format "[%s]: %s"
             (alist-get 'role msg)
             (truncate-string-to-width (alist-get 'content msg) 500 nil nil "...")))
   messages
   "\n"))

(defun gemini-repl-context-compact (messages &optional strategy)
  "Compact MESSAGES using STRATEGY.
STRATEGY defaults to `gemini-repl-context-default-strategy'.
Available strategies:
  - 'sliding-window: Keep most recent N messages
  - 'summarization: Summarize old messages with LLM
  - 'hybrid: Combine both approaches

Returns compacted message list."
  (let ((strat (or strategy gemini-repl-context-default-strategy)))
    (cl-incf gemini-repl-context-compaction-count)
    (message "Compacting context using %s strategy..." strat)
    (pcase strat
      ('sliding-window (gemini-repl-context-compact-sliding-window messages))
      ('summarization (gemini-repl-context-compact-summarization messages))
      ('hybrid (gemini-repl-context-compact-hybrid messages))
      (_ (error "Unknown compaction strategy: %s" strat)))))

;;; Monitoring and Status

(defun gemini-repl-context-check-and-warn (messages)
  "Check MESSAGES and warn/compact if needed.
Called automatically if `gemini-repl-context-auto-compact' is non-nil."
  (let ((usage (gemini-repl-context-usage messages)))
    (cond
     ;; Needs compaction
     ((>= usage gemini-repl-context-compaction-threshold)
      (when gemini-repl-context-auto-compact
        (message "Context usage at %.0f%%, compacting..." (* usage 100))
        (gemini-repl-context-compact messages)))
     ;; Just warning
     ((and (>= usage gemini-repl-context-warning-threshold)
           (or (null gemini-repl-context-last-warning)
               (> (float-time (time-since gemini-repl-context-last-warning)) 60)))
      (setq gemini-repl-context-last-warning (current-time))
      (message "Context usage at %.0f%% (warning threshold: %.0f%%)"
               (* usage 100)
               (* gemini-repl-context-warning-threshold 100))))))

(defun gemini-repl-context-status (&optional messages)
  "Display context status for MESSAGES.
If MESSAGES is nil, uses current conversation."
  (interactive)
  (let* ((msgs (or messages
                   (and (boundp 'gemini-repl-conversation)
                        gemini-repl-conversation)
                   '()))
         (stats (gemini-repl-context-tokens msgs))
         (total (alist-get 'total stats))
         (by-role (alist-get 'by-role stats))
         (count (alist-get 'count stats))
         (max-tokens (gemini-repl-context-get-max-tokens))
         (usage-pct (* 100 (/ (float total) max-tokens))))
    (with-output-to-temp-buffer "*gemini-repl-context-status*"
      (princ "=== Gemini REPL Context Status ===\n\n")
      (princ (format "Model: %s\n"
                     (or (and (boundp 'gemini-repl-model) gemini-repl-model)
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
                     (* 100 gemini-repl-context-warning-threshold)
                     (if (>= (/ (float total) max-tokens)
                            gemini-repl-context-warning-threshold)
                         "⚠ EXCEEDED" "✓")))
      (princ (format "Compaction: %.0f%% %s\n"
                     (* 100 gemini-repl-context-compaction-threshold)
                     (if (>= (/ (float total) max-tokens)
                            gemini-repl-context-compaction-threshold)
                         "⚠ EXCEEDED" "✓")))
      (princ (format "Auto-compact: %s\n"
                     (if gemini-repl-context-auto-compact "enabled" "disabled")))
      (princ (format "Compaction Strategy: %s\n"
                     gemini-repl-context-default-strategy))
      (princ (format "\nCompactions this session: %d\n"
                     gemini-repl-context-compaction-count))
      (when (> usage-pct 80)
        (princ "\n⚠ WARNING: Context usage is high!\n")
        (princ "Consider clearing old messages or enabling auto-compaction.\n")))))

(defun gemini-repl-context-reset-stats ()
  "Reset all context tracking statistics."
  (interactive)
  (setq gemini-repl-context-total-tokens 0
        gemini-repl-context-message-count 0
        gemini-repl-context-compaction-count 0
        gemini-repl-context-last-warning nil
        gemini-repl-context-tokens-by-role (make-hash-table :test 'equal))
  (message "Context statistics reset"))

;;; Utility Functions

(defun gemini-repl-context-format-number (num)
  "Format NUM with thousands separators."
  (let ((str (number-to-string num))
        (result ""))
    (while (> (length str) 3)
      (setq result (concat "," (substring str -3) result)
            str (substring str 0 -3)))
    (concat str result)))

(defun format-number (num)
  "Format NUM with thousands separators (wrapper for compatibility)."
  (gemini-repl-context-format-number num))

;;; Integration Hooks

(defun gemini-repl-context-track-message (message)
  "Track token usage for MESSAGE.
Updates session statistics."
  (let* ((role (alist-get 'role message))
         (tokens (gemini-repl-context-estimate-tokens
                 (alist-get 'content message))))
    (cl-incf gemini-repl-context-total-tokens tokens)
    (cl-incf (gethash role gemini-repl-context-tokens-by-role 0) tokens)
    (cl-incf gemini-repl-context-message-count)))

(defun gemini-repl-context-maybe-compact (messages)
  "Check MESSAGES and auto-compact if needed.
Returns possibly compacted message list."
  (if (and gemini-repl-context-auto-compact
           (gemini-repl-context-needs-compaction-p messages))
      (progn
        (message "Auto-compacting context...")
        (gemini-repl-context-compact messages))
    messages))

;;; Interactive Commands

;;;###autoload
(defun gemini-repl-context-show-status ()
  "Show context status in a buffer."
  (interactive)
  (gemini-repl-context-status))

;;;###autoload
(defun gemini-repl-context-compact-now (&optional strategy)
  "Manually trigger context compaction with STRATEGY.
If called interactively, prompts for strategy."
  (interactive
   (list (intern (completing-read
                  "Compaction strategy: "
                  '("sliding-window" "summarization" "hybrid")
                  nil t nil nil
                  (symbol-name gemini-repl-context-default-strategy)))))
  (if (and (boundp 'gemini-repl-conversation)
           gemini-repl-conversation)
      (progn
        (setq gemini-repl-conversation
              (gemini-repl-context-compact gemini-repl-conversation strategy))
        (message "Context compacted using %s strategy" strategy))
    (message "No active conversation to compact")))

(provide 'gemini-repl-context)
;;; gemini-repl-context.el ends here
