;;; sage-log.el --- JSON/file logging for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, llm, logging
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Logging functionality for sage to track API requests, responses,
;; and tool calls.  Supports both JSON (JSONL) and plain text formats.
;;
;; Log levels (from least to most verbose):
;; - error: Errors only
;; - warn: Warnings and errors
;; - info: General information, warnings, and errors
;; - debug: All messages including debug details
;;
;; JSON format outputs one JSON object per line (JSONL):
;; {"timestamp": "2026-01-10T12:00:00Z", "level": "info", "event": "request", "data": {...}}

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup sage-log nil
  "Logging configuration for sage."
  :group 'sage
  :prefix "sage-log-")

(defcustom sage-log-file nil
  "File path for sage logs.
When nil, logging to file is disabled.
When set, logs will be appended to this file."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Log file path"))
  :group 'sage-log)

(defcustom sage-log-level 'info
  "Minimum log level to record.
Messages below this level will be ignored."
  :type '(choice (const :tag "Error only" error)
                 (const :tag "Warn and above" warn)
                 (const :tag "Info and above" info)
                 (const :tag "Debug (all)" debug))
  :group 'sage-log)

(defcustom sage-log-format 'json
  "Format for log output.
- json: One JSON object per line (JSONL format)
- text: Human-readable plain text format"
  :type '(choice (const :tag "JSON (JSONL)" json)
                 (const :tag "Plain text" text))
  :group 'sage-log)

(defcustom sage-log-include-backtrace nil
  "Whether to include backtrace in error logs."
  :type 'boolean
  :group 'sage-log)

(defcustom sage-log-max-data-length 10000
  "Maximum length of data field in log entries.
Longer data will be truncated.  Set to nil for no limit."
  :type '(choice (integer :tag "Max characters")
                 (const :tag "No limit" nil))
  :group 'sage-log)

;;; Variables

(defvar sage-log--level-priority
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3))
  "Priority values for log levels.  Higher number = higher priority.")

(defvar sage-log--buffer-name "*sage-log*"
  "Name of the log buffer for in-memory logging.")

;;; Internal Functions

(defun sage-log--level-enabled-p (level)
  "Check if LEVEL is enabled based on current `sage-log-level'."
  (>= (alist-get level sage-log--level-priority 0)
      (alist-get sage-log-level sage-log--level-priority 0)))

(defun sage-log--format-timestamp ()
  "Return current timestamp in ISO 8601 format (UTC)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun sage-log--truncate-data (data)
  "Truncate DATA if it exceeds `sage-log-max-data-length'."
  (if (and sage-log-max-data-length
           (stringp data)
           (> (length data) sage-log-max-data-length))
      (concat (substring data 0 sage-log-max-data-length) "...[truncated]")
    data))

(defun sage-log--format-json (timestamp level event data)
  "Format log entry as JSON.
TIMESTAMP is ISO 8601 string, LEVEL is log level symbol,
EVENT is event type string, DATA is additional data."
  (let ((entry `((timestamp . ,timestamp)
                 (level . ,(symbol-name level))
                 (event . ,event))))
    (when data
      (push (cons 'data data) entry))
    (json-encode (nreverse entry))))

(defun sage-log--format-text (timestamp level event data)
  "Format log entry as plain text.
TIMESTAMP is ISO 8601 string, LEVEL is log level symbol,
EVENT is event type string, DATA is additional data."
  (let ((level-str (upcase (symbol-name level))))
    (if data
        (format "[%s] %s %s: %s"
                timestamp level-str event
                (if (stringp data)
                    data
                  (json-encode data)))
      (format "[%s] %s %s" timestamp level-str event))))

(defun sage-log--write-to-file (line)
  "Append LINE to the log file."
  (when sage-log-file
    (let ((dir (file-name-directory sage-log-file)))
      (when (and dir (not (file-exists-p dir)))
        (make-directory dir t)))
    (write-region (concat line "\n") nil sage-log-file 'append 'silent)))

(defun sage-log--write-to-buffer (line)
  "Write LINE to the log buffer."
  (with-current-buffer (get-buffer-create sage-log--buffer-name)
    (goto-char (point-max))
    (insert line "\n")))

;;; Public API

;;;###autoload
(defun sage-log-message (level event &optional data)
  "Log a message with LEVEL, EVENT type, and optional DATA.
LEVEL should be one of: debug, info, warn, error.
EVENT is a string describing the event type.
DATA is optional additional data (will be included in output)."
  (when (sage-log--level-enabled-p level)
    (let* ((timestamp (sage-log--format-timestamp))
           (processed-data (when data
                             (if (stringp data)
                                 (sage-log--truncate-data data)
                               data)))
           (line (if (eq sage-log-format 'json)
                     (sage-log--format-json timestamp level event processed-data)
                   (sage-log--format-text timestamp level event processed-data))))
      (sage-log--write-to-buffer line)
      (sage-log--write-to-file line)
      line)))

;;;###autoload
(defun sage-log-request (provider model prompt &optional options)
  "Log an API request.
PROVIDER is the API provider (e.g., \"gemini\", \"openai\").
MODEL is the model name.
PROMPT is the user prompt (may be truncated).
OPTIONS is optional additional request options."
  (sage-log-message
   'info
   "request"
   `((provider . ,provider)
     (model . ,model)
     (prompt . ,(sage-log--truncate-data prompt))
     ,@(when options `((options . ,options))))))

;;;###autoload
(defun sage-log-response (provider model response &optional tokens duration)
  "Log an API response.
PROVIDER is the API provider.
MODEL is the model name.
RESPONSE is the response content (may be truncated).
TOKENS is optional token count.
DURATION is optional request duration in seconds."
  (sage-log-message
   'info
   "response"
   `((provider . ,provider)
     (model . ,model)
     (response . ,(sage-log--truncate-data response))
     ,@(when tokens `((tokens . ,tokens)))
     ,@(when duration `((duration_ms . ,(round (* duration 1000))))))))

;;;###autoload
(defun sage-log-tool-call (tool-name args &optional result error)
  "Log a tool execution.
TOOL-NAME is the name of the tool.
ARGS is the arguments passed to the tool.
RESULT is the optional result of the tool execution.
ERROR is the optional error if the tool failed."
  (sage-log-message
   (if error 'error 'info)
   "tool_call"
   `((tool . ,tool-name)
     (args . ,args)
     ,@(when result `((result . ,(sage-log--truncate-data
                                  (if (stringp result) result
                                    (format "%S" result))))))
     ,@(when error `((error . ,(if (stringp error) error
                                 (format "%S" error))))))))

;;;###autoload
(defun sage-log-error (message &optional context)
  "Log an error MESSAGE with optional CONTEXT.
If `sage-log-include-backtrace' is non-nil, includes backtrace."
  (let ((data `((message . ,message)
                ,@(when context `((context . ,context)))
                ,@(when sage-log-include-backtrace
                    `((backtrace . ,(with-output-to-string
                                      (backtrace))))))))
    (sage-log-message 'error "error" data)))

;;;###autoload
(defun sage-log-warn (message &optional context)
  "Log a warning MESSAGE with optional CONTEXT."
  (sage-log-message 'warn "warning"
                    (if context
                        `((message . ,message) (context . ,context))
                      message)))

;;;###autoload
(defun sage-log-info (message &optional context)
  "Log an info MESSAGE with optional CONTEXT."
  (sage-log-message 'info "info"
                    (if context
                        `((message . ,message) (context . ,context))
                      message)))

;;;###autoload
(defun sage-log-debug (message &optional context)
  "Log a debug MESSAGE with optional CONTEXT."
  (sage-log-message 'debug "debug"
                    (if context
                        `((message . ,message) (context . ,context))
                      message)))

;;; Interactive Commands

;;;###autoload
(defun sage-log-view ()
  "View the sage log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create sage-log--buffer-name)))

;;;###autoload
(defun sage-log-clear ()
  "Clear the sage log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create sage-log--buffer-name)
    (erase-buffer))
  (message "Sage log buffer cleared"))

;;;###autoload
(defun sage-log-set-level (level)
  "Set the log LEVEL interactively."
  (interactive
   (list (intern (completing-read "Log level: "
                                  '("debug" "info" "warn" "error")
                                  nil t))))
  (setq sage-log-level level)
  (message "Sage log level set to %s" level))

;;;###autoload
(defun sage-log-set-format (format)
  "Set the log FORMAT interactively."
  (interactive
   (list (intern (completing-read "Log format: "
                                  '("json" "text")
                                  nil t))))
  (setq sage-log-format format)
  (message "Sage log format set to %s" format))

;;;###autoload
(defun sage-log-set-file (file)
  "Set the log FILE path interactively."
  (interactive
   (list (read-file-name "Log file: " nil nil nil "sage.log")))
  (setq sage-log-file file)
  (message "Sage log file set to %s" file))

(provide 'sage-log)
;;; sage-log.el ends here
