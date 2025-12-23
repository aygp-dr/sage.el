;;; gemini-repl-queue.el --- File-based queue system for inter-agent communication -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (filenotify "1.0"))
;; Keywords: ai, tools, queue, communication
;; URL: https://github.com/aygp-dr/gemini-repl-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; File-based queue system for inter-agent communication.
;; Enables multiple AI agents or processes to communicate asynchronously
;; through JSON files in a watched directory structure.
;;
;; Directory Structure:
;;   ~/.emacs.d/gemini-repl/queues/
;;   ├── input/    # Incoming requests (JSON files)
;;   ├── output/   # Responses
;;   └── archive/  # Processed requests
;;
;; Request Format (JSON):
;;   {
;;     "id": "uuid",
;;     "type": "prompt|command|ping",
;;     "content": "...",
;;     "context": {...},
;;     "created_at": "timestamp"
;;   }
;;
;; Response Format:
;;   {
;;     "request_id": "uuid",
;;     "status": "success|error",
;;     "content": "...",
;;     "processed_at": "timestamp"
;;   }
;;
;; Usage:
;;   (gemini-repl-queue-submit 'prompt "What is 2+2?")
;;   (gemini-repl-queue-poll)
;;   (gemini-repl-queue-watch-mode 1)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'filenotify)

;;; Customization

(defgroup gemini-repl-queue nil
  "File-based queue system for inter-agent communication."
  :group 'gemini-repl
  :prefix "gemini-repl-queue-")

(defcustom gemini-repl-queue-directory
  (expand-file-name "gemini-repl/queues" user-emacs-directory)
  "Root directory for queue files."
  :type 'directory
  :group 'gemini-repl-queue)

(defcustom gemini-repl-queue-poll-interval 2.0
  "Polling interval in seconds when file-notify is unavailable."
  :type 'number
  :group 'gemini-repl-queue)

(defcustom gemini-repl-queue-auto-process t
  "When non-nil, automatically process incoming requests in watch mode."
  :type 'boolean
  :group 'gemini-repl-queue)

(defcustom gemini-repl-queue-max-retries 3
  "Maximum number of retries for failed requests."
  :type 'integer
  :group 'gemini-repl-queue)

(defcustom gemini-repl-queue-retention-days 7
  "Number of days to keep archived requests."
  :type 'integer
  :group 'gemini-repl-queue)

;;; Variables

(defvar gemini-repl-queue--input-dir nil
  "Input queue directory.")

(defvar gemini-repl-queue--output-dir nil
  "Output queue directory.")

(defvar gemini-repl-queue--archive-dir nil
  "Archive directory.")

(defvar gemini-repl-queue--watch-descriptor nil
  "File-notify watch descriptor.")

(defvar gemini-repl-queue--poll-timer nil
  "Timer for polling when file-notify is unavailable.")

(defvar gemini-repl-queue--request-handlers (make-hash-table :test 'equal)
  "Hash table of request type handlers.
Keys are request types (symbols), values are functions.")

(defvar gemini-repl-queue--pending-requests (make-hash-table :test 'equal)
  "Hash table tracking pending requests.
Keys are request IDs, values are request metadata.")

(defvar gemini-repl-queue--stats
  '((submitted . 0)
    (processed . 0)
    (errors . 0)
    (archived . 0))
  "Queue statistics.")

;;; Directory Management

(defun gemini-repl-queue--ensure-directories ()
  "Ensure queue directories exist."
  (let ((base gemini-repl-queue-directory))
    (setq gemini-repl-queue--input-dir (expand-file-name "input" base))
    (setq gemini-repl-queue--output-dir (expand-file-name "output" base))
    (setq gemini-repl-queue--archive-dir (expand-file-name "archive" base))
    (dolist (dir (list gemini-repl-queue--input-dir
                       gemini-repl-queue--output-dir
                       gemini-repl-queue--archive-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))))

(defun gemini-repl-queue--generate-id ()
  "Generate a unique ID for requests."
  (format "%s-%s"
          (format-time-string "%Y%m%d-%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun gemini-repl-queue--timestamp ()
  "Generate ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

;;; File Operations

(defun gemini-repl-queue--write-json (file data)
  "Write DATA as JSON to FILE atomically."
  (let ((temp-file (concat file ".tmp")))
    (with-temp-file temp-file
      (insert (json-encode data)))
    (rename-file temp-file file t)))

(defun gemini-repl-queue--read-json (file)
  "Read JSON from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (json-read))))

(defun gemini-repl-queue--list-requests (directory)
  "List all .json files in DIRECTORY sorted by modification time."
  (let ((files (directory-files directory t "\\.json$")))
    (sort files
          (lambda (a b)
            (time-less-p (file-attribute-modification-time (file-attributes b))
                        (file-attribute-modification-time (file-attributes a)))))))

;;; Request Management

;;;###autoload
(defun gemini-repl-queue-submit (type content &optional context)
  "Submit a request to the queue.
TYPE is the request type (symbol: prompt, command, ping).
CONTENT is the request content (string).
CONTEXT is optional metadata (alist)."
  (interactive
   (list (intern (completing-read "Request type: " '("prompt" "command" "ping")))
         (read-string "Content: ")
         nil))
  (gemini-repl-queue--ensure-directories)

  (let* ((id (gemini-repl-queue--generate-id))
         (request `((id . ,id)
                   (type . ,(symbol-name type))
                   (content . ,content)
                   (context . ,(or context (make-hash-table)))
                   (created_at . ,(gemini-repl-queue--timestamp))))
         (file (expand-file-name (format "%s.json" id) gemini-repl-queue--input-dir)))

    (gemini-repl-queue--write-json file request)
    (puthash id request gemini-repl-queue--pending-requests)
    (cl-incf (alist-get 'submitted gemini-repl-queue--stats))

    (message "Queue: Submitted request %s" id)
    id))

;;;###autoload
(defun gemini-repl-queue-respond (request-id status content &optional metadata)
  "Write a response for REQUEST-ID.
STATUS is 'success or 'error.
CONTENT is the response content.
METADATA is optional additional data."
  (interactive
   (list (read-string "Request ID: ")
         (intern (completing-read "Status: " '("success" "error")))
         (read-string "Content: ")
         nil))
  (gemini-repl-queue--ensure-directories)

  (let* ((response `((request_id . ,request-id)
                    (status . ,(symbol-name status))
                    (content . ,content)
                    (metadata . ,(or metadata (make-hash-table)))
                    (processed_at . ,(gemini-repl-queue--timestamp))))
         (file (expand-file-name (format "%s.json" request-id) gemini-repl-queue--output-dir)))

    (gemini-repl-queue--write-json file response)
    (cl-incf (alist-get 'processed gemini-repl-queue--stats))

    (message "Queue: Wrote response for %s" request-id)
    request-id))

;;;###autoload
(defun gemini-repl-queue-archive (request-id)
  "Archive processed request REQUEST-ID."
  (interactive
   (list (completing-read "Request ID: "
                         (mapcar #'file-name-base
                                (gemini-repl-queue--list-requests gemini-repl-queue--input-dir)))))
  (gemini-repl-queue--ensure-directories)

  (let ((input-file (expand-file-name (format "%s.json" request-id) gemini-repl-queue--input-dir))
        (output-file (expand-file-name (format "%s.json" request-id) gemini-repl-queue--output-dir))
        (archive-file (expand-file-name (format "%s.json" request-id) gemini-repl-queue--archive-dir)))

    ;; Move input file to archive
    (when (file-exists-p input-file)
      (rename-file input-file archive-file t))

    ;; Move output file to archive with response suffix
    (when (file-exists-p output-file)
      (rename-file output-file
                   (expand-file-name (format "%s-response.json" request-id) gemini-repl-queue--archive-dir)
                   t))

    (remhash request-id gemini-repl-queue--pending-requests)
    (cl-incf (alist-get 'archived gemini-repl-queue--stats))

    (message "Queue: Archived request %s" request-id)))

;;;###autoload
(defun gemini-repl-queue-poll ()
  "Check for pending requests and return the oldest one."
  (interactive)
  (gemini-repl-queue--ensure-directories)

  (let ((files (gemini-repl-queue--list-requests gemini-repl-queue--input-dir)))
    (if files
        (let* ((file (car (last files))) ; oldest file
               (request (gemini-repl-queue--read-json file)))
          (when request
            (message "Queue: Found pending request %s" (alist-get 'id request))
            request))
      (when (called-interactively-p 'any)
        (message "Queue: No pending requests"))
      nil)))

;;;###autoload
(defun gemini-repl-queue-status ()
  "Show queue status and statistics."
  (interactive)
  (gemini-repl-queue--ensure-directories)

  (let* ((input-count (length (gemini-repl-queue--list-requests gemini-repl-queue--input-dir)))
         (output-count (length (gemini-repl-queue--list-requests gemini-repl-queue--output-dir)))
         (archive-count (length (gemini-repl-queue--list-requests gemini-repl-queue--archive-dir)))
         (stats gemini-repl-queue--stats)
         (buf (get-buffer-create "*gemini-repl-queue-status*")))

    (with-current-buffer buf
      (erase-buffer)
      (insert "Gemini REPL Queue Status\n")
      (insert "========================\n\n")
      (insert (format "Queue Directory: %s\n\n" gemini-repl-queue-directory))

      (insert "Current Counts:\n")
      (insert (format "  Input (pending):   %d\n" input-count))
      (insert (format "  Output (ready):    %d\n" output-count))
      (insert (format "  Archive:           %d\n\n" archive-count))

      (insert "Session Statistics:\n")
      (insert (format "  Submitted:         %d\n" (alist-get 'submitted stats)))
      (insert (format "  Processed:         %d\n" (alist-get 'processed stats)))
      (insert (format "  Errors:            %d\n" (alist-get 'errors stats)))
      (insert (format "  Archived:          %d\n\n" (alist-get 'archived stats)))

      (insert "Watch Mode:          ")
      (insert (if gemini-repl-queue-watch-mode "ACTIVE" "INACTIVE"))
      (insert "\n")

      (when gemini-repl-queue--watch-descriptor
        (insert (format "File Notify:         Active (descriptor: %s)\n" gemini-repl-queue--watch-descriptor)))

      (when gemini-repl-queue--poll-timer
        (insert "Polling Timer:       Active\n"))

      (goto-char (point-min))
      (special-mode))

    (pop-to-buffer buf)))

;;; Request Processing

(defun gemini-repl-queue-register-handler (type handler-fn)
  "Register HANDLER-FN for request TYPE.
HANDLER-FN should accept a request alist and return (status . content)."
  (puthash (symbol-name type) handler-fn gemini-repl-queue--request-handlers))

(defun gemini-repl-queue--process-request (request)
  "Process REQUEST and generate response."
  (let* ((id (alist-get 'id request))
         (type (alist-get 'type request))
         (content (alist-get 'content request))
         (handler (gethash type gemini-repl-queue--request-handlers)))

    (if handler
        (condition-case err
            (let ((result (funcall handler request)))
              (gemini-repl-queue-respond id
                                        (car result)
                                        (cdr result))
              (gemini-repl-queue-archive id))
          (error
           (gemini-repl-queue-respond id 'error (format "Handler error: %s" err))
           (cl-incf (alist-get 'errors gemini-repl-queue--stats))))
      (gemini-repl-queue-respond id 'error (format "No handler for type: %s" type))
      (cl-incf (alist-get 'errors gemini-repl-queue--stats)))))

(defun gemini-repl-queue--process-all ()
  "Process all pending requests."
  (let ((files (gemini-repl-queue--list-requests gemini-repl-queue--input-dir)))
    (dolist (file files)
      (when-let ((request (gemini-repl-queue--read-json file)))
        (gemini-repl-queue--process-request request)))))

;;; File Watching

(defun gemini-repl-queue--on-file-change (event)
  "Handle file change EVENT."
  (let ((type (nth 1 event))
        (file (nth 2 event)))
    (when (and (eq type 'created)
               (string-suffix-p ".json" file)
               (not (string-suffix-p ".tmp" file))
               gemini-repl-queue-auto-process)
      (when-let ((request (gemini-repl-queue--read-json file)))
        (message "Queue: Processing new request %s" (alist-get 'id request))
        (gemini-repl-queue--process-request request)))))

(defun gemini-repl-queue--start-watching ()
  "Start watching input directory for new requests."
  (gemini-repl-queue--ensure-directories)

  ;; Try file-notify first
  (condition-case err
      (progn
        (setq gemini-repl-queue--watch-descriptor
              (file-notify-add-watch gemini-repl-queue--input-dir
                                    '(change)
                                    #'gemini-repl-queue--on-file-change))
        (message "Queue: File notify active"))
    (error
     (message "Queue: File notify unavailable, using polling: %s" err)
     ;; Fall back to polling
     (setq gemini-repl-queue--poll-timer
           (run-with-timer gemini-repl-queue-poll-interval
                          gemini-repl-queue-poll-interval
                          (lambda ()
                            (when (and gemini-repl-queue-watch-mode
                                      gemini-repl-queue-auto-process)
                              (gemini-repl-queue--process-all))))))))

(defun gemini-repl-queue--stop-watching ()
  "Stop watching input directory."
  (when gemini-repl-queue--watch-descriptor
    (file-notify-rm-watch gemini-repl-queue--watch-descriptor)
    (setq gemini-repl-queue--watch-descriptor nil))

  (when gemini-repl-queue--poll-timer
    (cancel-timer gemini-repl-queue--poll-timer)
    (setq gemini-repl-queue--poll-timer nil)))

;;; Minor Mode

;;;###autoload
(define-minor-mode gemini-repl-queue-watch-mode
  "Minor mode to automatically process queue requests."
  :global t
  :lighter " GRQ"
  :group 'gemini-repl-queue

  (if gemini-repl-queue-watch-mode
      (progn
        (gemini-repl-queue--start-watching)
        (message "Gemini REPL Queue watch mode enabled"))
    (gemini-repl-queue--stop-watching)
    (message "Gemini REPL Queue watch mode disabled")))

;;; Cleanup

(defun gemini-repl-queue-cleanup-archives (&optional days)
  "Remove archived requests older than DAYS (default: `gemini-repl-queue-retention-days')."
  (interactive)
  (gemini-repl-queue--ensure-directories)

  (let* ((days (or days gemini-repl-queue-retention-days))
         (cutoff (time-subtract (current-time) (days-to-time days)))
         (files (directory-files gemini-repl-queue--archive-dir t "\\.json$"))
         (removed 0))

    (dolist (file files)
      (when (time-less-p (file-attribute-modification-time (file-attributes file)) cutoff)
        (delete-file file)
        (cl-incf removed)))

    (message "Queue: Removed %d archived request(s) older than %d days" removed days)))

;;; Built-in Handlers

(defun gemini-repl-queue--ping-handler (request)
  "Handle ping REQUEST."
  (cons 'success (format "pong: %s" (alist-get 'created_at request))))

(defun gemini-repl-queue--prompt-handler (request)
  "Handle prompt REQUEST using gemini-repl."
  (require 'gemini-repl)
  (let ((content (alist-get 'content request)))
    (condition-case err
        (let ((response (gemini-repl-exec content)))
          (cons 'success response))
      (error (cons 'error (format "Prompt error: %s" err))))))

;; Register built-in handlers
(gemini-repl-queue-register-handler 'ping #'gemini-repl-queue--ping-handler)
(gemini-repl-queue-register-handler 'prompt #'gemini-repl-queue--prompt-handler)

;;; Integration Functions

;;;###autoload
(defun gemini-repl-queue-send-to-agent (agent-id message)
  "Send MESSAGE to AGENT-ID via queue system."
  (interactive
   (list (read-string "Agent ID: ")
         (read-string "Message: ")))
  (gemini-repl-queue-submit 'prompt message `((target_agent . ,agent-id))))

;;;###autoload
(defun gemini-repl-queue-broadcast (message)
  "Broadcast MESSAGE to all listening agents."
  (interactive "sMessage: ")
  (gemini-repl-queue-submit 'command message '((broadcast . t))))

(provide 'gemini-repl-queue)
;;; gemini-repl-queue.el ends here
