;;; sage-reflect.el --- Reflection and self-awareness for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, llm, reflection
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reflection patterns for agentic AI workflows.
;;
;; This module implements the Reflection pattern from agentic AI development:
;; - Context awareness (token usage, warnings at thresholds)
;; - Session retrospectives (what worked, what didn't)
;; - Tool call analysis (patterns, frequency, errors)
;; - Memory integration (persistent learnings)
;;
;; Agentic Patterns Implemented:
;; | Pattern    | Implementation              | Notes                    |
;; |------------|-----------------------------|--------------------------|
;; | Tool Use   | sage-tools.el               | AI executes real commands|
;; | Memory     | sage-memory.el              | Persists across sessions |
;; | Planning   | sage-project.el             | Structured work breakdown|
;; | Reflection | sage-reflect.el (this file) | Learn from each session  |
;;
;; Usage:
;;   (require 'sage-reflect)
;;   (sage-reflect-enable)               ; Enable reflection hooks
;;   (sage-reflect-session-summary)      ; Get session retrospective
;;   (sage-reflect-tool-analysis)        ; Analyze tool usage patterns

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup sage-reflect nil
  "Reflection and self-awareness for sage."
  :group 'sage
  :prefix "sage-reflect-")

(defcustom sage-reflect-enabled t
  "Whether reflection is enabled."
  :type 'boolean
  :group 'sage-reflect)

(defcustom sage-reflect-warning-thresholds '(0.50 0.75 0.90)
  "Context usage thresholds that trigger warnings.
Each threshold triggers a different level of warning:
- 0.50: Info - suggest considering compaction soon
- 0.75: Warning - recommend compaction
- 0.90: Urgent - compaction required"
  :type '(repeat float)
  :group 'sage-reflect)

(defcustom sage-reflect-auto-summarize-session t
  "Automatically generate session summary on close."
  :type 'boolean
  :group 'sage-reflect)

(defcustom sage-reflect-track-tool-patterns t
  "Track tool usage patterns for analysis."
  :type 'boolean
  :group 'sage-reflect)

(defcustom sage-reflect-log-insights t
  "Log reflection insights to sage-log."
  :type 'boolean
  :group 'sage-reflect)

;;; Internal state

(defvar sage-reflect--session-start nil
  "Timestamp when current session started.")

(defvar sage-reflect--tool-calls nil
  "List of tool calls in current session.
Each entry: (timestamp tool-name args result duration-ms success-p)")

(defvar sage-reflect--context-warnings nil
  "Context warnings issued in current session.
Each entry: (timestamp threshold-level message)")

(defvar sage-reflect--last-threshold-warned 0.0
  "Last context threshold that triggered a warning.")

(defvar sage-reflect--errors nil
  "Errors encountered in current session.")

(defvar sage-reflect--insights nil
  "Insights gathered during session.")

;;; Context awareness

(defun sage-reflect-check-context (usage-ratio)
  "Check context USAGE-RATIO and issue warnings at thresholds.
Returns a warning message if threshold crossed, nil otherwise."
  (when sage-reflect-enabled
    (let ((warning nil)
          (thresholds sage-reflect-warning-thresholds))
      ;; Find highest threshold crossed
      (dolist (threshold thresholds)
        (when (and (>= usage-ratio threshold)
                   (< sage-reflect--last-threshold-warned threshold))
          (setq warning (sage-reflect--make-context-warning usage-ratio threshold))
          (setq sage-reflect--last-threshold-warned threshold)))

      ;; Record warning
      (when warning
        (push (list (current-time) sage-reflect--last-threshold-warned warning)
              sage-reflect--context-warnings)
        ;; Log if enabled
        (when (and sage-reflect-log-insights
                   (fboundp 'sage-log-message))
          (sage-log-message 'warn "context_warning"
                            `((usage . ,usage-ratio)
                              (threshold . ,sage-reflect--last-threshold-warned)
                              (message . ,warning)))))
      warning)))

(defun sage-reflect--make-context-warning (usage threshold)
  "Create context warning message for USAGE at THRESHOLD."
  (let ((pct (round (* 100 usage))))
    (cond
     ((< threshold 0.60)
      (format "Context at %d%% - Consider planning for compaction soon. \
Current conversation may benefit from summarization." pct))
     ((< threshold 0.80)
      (format "Context at %d%% - Recommend compacting conversation. \
Use /compact or M-x sage-context-compact to summarize history." pct))
     (t
      (format "Context at %d%% - URGENT: Compaction required! \
Context limit approaching. Run /compact now to avoid truncation." pct)))))

(defun sage-reflect-context-status ()
  "Get current context status with reflection insights.
Returns an alist with usage info and recommendations."
  (let* ((messages (if (boundp 'sage-conversation) sage-conversation nil))
         (model (if (boundp 'sage-model) sage-model 'default))
         ;; sage-context-tokens returns an alist with 'total, 'by-role, 'count
         ;; Extract the total token count, handling various return types
         (token-result (if (fboundp 'sage-context-tokens)
                           (sage-context-tokens messages)
                         nil))
         (tokens (cond
                  ;; If it's an alist with 'total key, extract the value
                  ((and (listp token-result)
                        (alist-get 'total token-result))
                   (alist-get 'total token-result))
                  ;; If it's already a number, use it directly
                  ((numberp token-result)
                   token-result)
                  ;; Otherwise default to 0
                  (t 0)))
         ;; Use sage-context-get-max-tokens (the actual function name)
         ;; Convert symbol to string if needed, as sage-context-get-max-tokens expects string
         (model-str (cond
                     ((stringp model) model)
                     ((symbolp model) (symbol-name model))
                     (t nil)))
         (limit (if (fboundp 'sage-context-get-max-tokens)
                    (sage-context-get-max-tokens model-str)
                  8192))
         (usage (if (and (numberp limit) (> limit 0))
                    (/ (float tokens) limit)
                  0.0))
         (warnings-count (length sage-reflect--context-warnings)))

    `((tokens . ,tokens)
      (limit . ,limit)
      (usage . ,usage)
      (usage-pct . ,(round (* 100 usage)))
      (warnings-issued . ,warnings-count)
      (recommendation . ,(sage-reflect--context-recommendation usage))
      (session-duration . ,(sage-reflect--session-duration)))))

(defun sage-reflect--context-recommendation (usage)
  "Get recommendation based on context USAGE."
  (cond
   ((< usage 0.25) "Healthy context - plenty of room for conversation")
   ((< usage 0.50) "Good context - monitor as conversation grows")
   ((< usage 0.75) "Consider compacting soon to maintain performance")
   ((< usage 0.90) "Recommend compaction now - use M-x sage-context-compact")
   (t "URGENT: Compact immediately to prevent context overflow")))

;;; Tool call tracking

(defun sage-reflect-record-tool-call (tool-name args result duration-ms success-p)
  "Record a tool call for analysis.
TOOL-NAME is the tool, ARGS the arguments, RESULT the outcome,
DURATION-MS the execution time, SUCCESS-P whether it succeeded."
  (when sage-reflect-track-tool-patterns
    (push (list (current-time) tool-name args result duration-ms success-p)
          sage-reflect--tool-calls)
    ;; Track errors
    (unless success-p
      (push (list (current-time) tool-name args result)
            sage-reflect--errors))))

(defun sage-reflect-tool-analysis ()
  "Analyze tool usage patterns in current session.
Returns insights about tool usage."
  (let ((calls sage-reflect--tool-calls)
        (tool-counts (make-hash-table :test 'equal))
        (tool-times (make-hash-table :test 'equal))
        (tool-errors (make-hash-table :test 'equal))
        (total-calls 0)
        (total-time 0)
        (total-errors 0))

    ;; Aggregate stats
    (dolist (call calls)
      (let ((tool (nth 1 call))
            (duration (nth 4 call))
            (success (nth 5 call)))
        (cl-incf total-calls)
        (cl-incf (gethash tool tool-counts 0))
        (cl-incf total-time (or duration 0))
        (cl-incf (gethash tool tool-times 0) (or duration 0))
        (unless success
          (cl-incf total-errors)
          (cl-incf (gethash tool tool-errors 0)))))

    ;; Build analysis
    (let ((top-tools nil)
          (slow-tools nil)
          (error-prone nil))

      ;; Find top used tools
      (maphash (lambda (tool count)
                 (push (cons tool count) top-tools))
               tool-counts)
      (setq top-tools (seq-take (sort top-tools (lambda (a b) (> (cdr a) (cdr b)))) 5))

      ;; Find slow tools (avg > 1000ms)
      (maphash (lambda (tool total-time)
                 (let ((count (gethash tool tool-counts 1)))
                   (when (> (/ total-time count) 1000)
                     (push (cons tool (/ total-time count)) slow-tools))))
               tool-times)

      ;; Find error-prone tools
      (maphash (lambda (tool errors)
                 (let ((count (gethash tool tool-counts 1)))
                   (when (> (/ (float errors) count) 0.1)  ; >10% error rate
                     (push (cons tool (round (* 100 (/ (float errors) count))))
                           error-prone))))
               tool-errors)

      `((total-calls . ,total-calls)
        (total-time-ms . ,total-time)
        (total-errors . ,total-errors)
        (error-rate . ,(if (> total-calls 0)
                           (round (* 100 (/ (float total-errors) total-calls)))
                         0))
        (top-tools . ,top-tools)
        (slow-tools . ,slow-tools)
        (error-prone-tools . ,error-prone)
        (insights . ,(sage-reflect--tool-insights top-tools slow-tools error-prone))))))

(defun sage-reflect--tool-insights (top-tools slow-tools error-prone)
  "Generate insights from tool analysis."
  (let ((insights nil))
    ;; Usage pattern insights
    (when top-tools
      (let ((top (car top-tools)))
        (push (format "Most used tool: %s (%d calls)" (car top) (cdr top))
              insights)))

    ;; Performance insights
    (when slow-tools
      (push (format "Slow tools detected: %s - consider caching or optimization"
                    (mapconcat (lambda (tl) (format "%s" (car tl))) slow-tools ", "))
            insights))

    ;; Reliability insights
    (when error-prone
      (push (format "High error rate tools: %s - review usage patterns"
                    (mapconcat (lambda (tl) (format "%s (%d%%)" (car tl) (cdr tl)))
                               error-prone ", "))
            insights))

    (nreverse insights)))

;;; Session retrospective

(defun sage-reflect-session-summary ()
  "Generate a session retrospective summary.
This implements the Reflection pattern for learning from sessions."
  (let* ((duration (sage-reflect--session-duration))
         (tool-analysis (sage-reflect-tool-analysis))
         (context-status (sage-reflect-context-status))
         (errors sage-reflect--errors)
         (warnings sage-reflect--context-warnings))

    `((session-duration . ,duration)
      (context . ,context-status)
      (tools . ,tool-analysis)
      (errors-count . ,(length errors))
      (warnings-count . ,(length warnings))
      (learnings . ,(sage-reflect--generate-learnings
                     tool-analysis context-status errors))
      (recommendations . ,(sage-reflect--generate-recommendations
                           tool-analysis context-status)))))

(defun sage-reflect--generate-learnings (tool-analysis context-status errors)
  "Generate learnings from session data."
  (let ((learnings nil))
    ;; Tool efficiency learning
    (let ((total-calls (alist-get 'total-calls tool-analysis 0)))
      (when (> total-calls 10)
        (push (format "Session used %d tool calls - %s"
                      total-calls
                      (if (> (alist-get 'error-rate tool-analysis 0) 5)
                          "high error rate suggests need for better prompts"
                        "good tool execution success rate"))
              learnings)))

    ;; Context management learning
    (let ((usage (alist-get 'usage context-status 0)))
      (cond
       ((> usage 0.90)
        (push "Context nearly exhausted - start with /compact in future sessions"
              learnings))
       ((> usage 0.75)
        (push "Context usage high - consider more frequent summarization"
              learnings))))

    ;; Error pattern learning
    (when (> (length errors) 3)
      (push (format "%d errors occurred - review tool usage patterns"
                    (length errors))
            learnings))

    (or learnings '("Session completed normally - no significant learnings"))))

(defun sage-reflect--generate-recommendations (tool-analysis context-status)
  "Generate recommendations for future sessions."
  (let ((recs nil))
    ;; Context recommendations
    (when (> (alist-get 'usage context-status 0) 0.5)
      (push "Use /compact more frequently to manage context"
            recs))

    ;; Tool recommendations
    (let ((slow-tools (alist-get 'slow-tools tool-analysis)))
      (when slow-tools
        (push (format "Consider alternatives for slow tools: %s"
                      (mapconcat #'car slow-tools ", "))
              recs)))

    ;; Error recommendations
    (let ((error-rate (alist-get 'error-rate tool-analysis 0)))
      (when (> error-rate 10)
        (push "Review tool arguments - high error rate detected"
              recs)))

    (or recs '("Continue current patterns - session was efficient"))))

(defun sage-reflect--session-duration ()
  "Get session duration as human-readable string."
  (if sage-reflect--session-start
      (let* ((elapsed (float-time (time-subtract (current-time)
                                                  sage-reflect--session-start)))
             (hours (floor (/ elapsed 3600)))
             (mins (floor (/ (mod elapsed 3600) 60)))
             (secs (floor (mod elapsed 60))))
        (cond
         ((> hours 0) (format "%dh %dm" hours mins))
         ((> mins 0) (format "%dm %ds" mins secs))
         (t (format "%ds" secs))))
    "unknown"))

;;; Hooks and integration

(defun sage-reflect-enable ()
  "Enable reflection for the current session."
  (interactive)
  (setq sage-reflect-enabled t)
  (setq sage-reflect--session-start (current-time))
  (setq sage-reflect--tool-calls nil)
  (setq sage-reflect--context-warnings nil)
  (setq sage-reflect--last-threshold-warned 0.0)
  (setq sage-reflect--errors nil)
  (setq sage-reflect--insights nil)
  (message "Reflection enabled for this session"))

(defun sage-reflect-disable ()
  "Disable reflection."
  (interactive)
  (setq sage-reflect-enabled nil)
  (message "Reflection disabled"))

(defun sage-reflect-reset ()
  "Reset reflection state for a new session."
  (interactive)
  (sage-reflect-enable)
  (message "Reflection state reset"))

;;; Interactive commands

(defun sage-reflect-show-status ()
  "Show current reflection status in a buffer."
  (interactive)
  (let ((status (sage-reflect-context-status))
        (tool-analysis (sage-reflect-tool-analysis)))
    (with-current-buffer (get-buffer-create "*sage-reflect*")
      (erase-buffer)
      (insert "=== Sage Reflection Status ===\n\n")

      ;; Context
      (insert "** Context **\n")
      (insert (format "Tokens: %d / %d (%d%%)\n"
                      (alist-get 'tokens status)
                      (alist-get 'limit status)
                      (alist-get 'usage-pct status)))
      (insert (format "Session: %s\n" (alist-get 'session-duration status)))
      (insert (format "Recommendation: %s\n\n" (alist-get 'recommendation status)))

      ;; Tools
      (insert "** Tool Usage **\n")
      (insert (format "Total calls: %d\n" (alist-get 'total-calls tool-analysis)))
      (insert (format "Error rate: %d%%\n" (alist-get 'error-rate tool-analysis)))
      (when-let* ((top (alist-get 'top-tools tool-analysis)))
        (insert "Top tools: ")
        (insert (mapconcat (lambda (tl) (format "%s(%d)" (car tl) (cdr tl)))
                           top ", "))
        (insert "\n"))

      ;; Insights
      (insert "\n** Insights **\n")
      (dolist (insight (alist-get 'insights tool-analysis))
        (insert (format "- %s\n" insight)))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun sage-reflect-show-summary ()
  "Show session summary/retrospective."
  (interactive)
  (let ((summary (sage-reflect-session-summary)))
    (with-current-buffer (get-buffer-create "*sage-reflect-summary*")
      (erase-buffer)
      (insert "=== Session Retrospective ===\n\n")

      (insert (format "Duration: %s\n\n" (alist-get 'session-duration summary)))

      (insert "** Learnings **\n")
      (dolist (learning (alist-get 'learnings summary))
        (insert (format "- %s\n" learning)))

      (insert "\n** Recommendations **\n")
      (dolist (rec (alist-get 'recommendations summary))
        (insert (format "- %s\n" rec)))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Memory integration

(defun sage-reflect-save-learnings ()
  "Save session learnings to persistent memory."
  (interactive)
  (when (fboundp 'sage-memory-add)
    (let ((summary (sage-reflect-session-summary)))
      (dolist (learning (alist-get 'learnings summary))
        (sage-memory-add (format "learning-%s" (format-time-string "%Y%m%d%H%M%S"))
                         learning
                         'reflection))
      (message "Saved %d learnings to memory"
               (length (alist-get 'learnings summary))))))

(provide 'sage-reflect)
;;; sage-reflect.el ends here
