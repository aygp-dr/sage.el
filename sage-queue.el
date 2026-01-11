;;; sage-queue.el --- File-based queue system for inter-agent communication -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (filenotify "1.0"))
;; Keywords: ai, tools, queue, communication
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; File-based queue system for inter-agent communication.
;; Enables multiple AI agents or processes to communicate asynchronously
;; through JSON files in a watched directory structure.
;;
;; Directory Structure:
;;   ~/.emacs.d/sage/queues/
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
;;   (sage-queue-submit 'prompt "What is 2+2?")
;;   (sage-queue-poll)
;;   (sage-queue-watch-mode 1)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'filenotify)

;;; Customization

(defgroup sage-queue nil
  "File-based queue system for inter-agent communication."
  :group 'sage
  :prefix "sage-queue-")

(defcustom sage-queue-directory
  (expand-file-name "sage/queues" user-emacs-directory)
  "Root directory for queue files."
  :type 'directory
  :group 'sage-queue)

(defcustom sage-queue-poll-interval 2.0
  "Polling interval in seconds when file-notify is unavailable."
  :type 'number
  :group 'sage-queue)

(defcustom sage-queue-auto-process t
  "When non-nil, automatically process incoming requests in watch mode."
  :type 'boolean
  :group 'sage-queue)

(defcustom sage-queue-max-retries 3
  "Maximum number of retries for failed requests."
  :type 'integer
  :group 'sage-queue)

(defcustom sage-queue-retention-days 7
  "Number of days to keep archived requests."
  :type 'integer
  :group 'sage-queue)

;;; Variables

(defvar sage-queue--input-dir nil
  "Input queue directory.")

(defvar sage-queue--output-dir nil
  "Output queue directory.")

(defvar sage-queue--archive-dir nil
  "Archive directory.")

(defvar sage-queue--watch-descriptor nil
  "File-notify watch descriptor.")

(defvar sage-queue--poll-timer nil
  "Timer for polling when file-notify is unavailable.")

(defvar sage-queue--request-handlers (make-hash-table :test 'equal)
  "Hash table of request type handlers.
Keys are request types (symbols), values are functions.")

(defvar sage-queue--pending-requests (make-hash-table :test 'equal)
  "Hash table tracking pending requests.
Keys are request IDs, values are request metadata.")

(defvar sage-queue--stats
  '((submitted . 0)
    (processed . 0)
    (errors . 0)
    (archived . 0))
  "Queue statistics.")

;;; Directory Management

(defun sage-queue--ensure-directories ()
  "Ensure queue directories exist."
  (let ((base sage-queue-directory))
    (setq sage-queue--input-dir (expand-file-name "input" base))
    (setq sage-queue--output-dir (expand-file-name "output" base))
    (setq sage-queue--archive-dir (expand-file-name "archive" base))
    (dolist (dir (list sage-queue--input-dir
                       sage-queue--output-dir
                       sage-queue--archive-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))))

(defun sage-queue--generate-id ()
  "Generate a unique ID for requests."
  (format "%s-%s"
          (format-time-string "%Y%m%d-%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun sage-queue--timestamp ()
  "Generate ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

;;; File Operations

(defun sage-queue--write-json (file data)
  "Write DATA as JSON to FILE atomically."
  (let ((temp-file (concat file ".tmp")))
    (with-temp-file temp-file
      (insert (json-encode data)))
    (rename-file temp-file file t)))

(defun sage-queue--read-json (file)
  "Read JSON from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (json-read))))

(defun sage-queue--list-requests (directory)
  "List all .json files in DIRECTORY sorted by modification time."
  (let ((files (directory-files directory t "\\.json$")))
    (sort files
          (lambda (a b)
            (time-less-p (file-attribute-modification-time (file-attributes b))
                        (file-attribute-modification-time (file-attributes a)))))))

;;; Request Management

;;;###autoload
(defun sage-queue-submit (type content &optional context)
  "Submit a request to the queue.
TYPE is the request type (symbol: prompt, command, ping).
CONTENT is the request content (string).
CONTEXT is optional metadata (alist)."
  (interactive
   (list (intern (completing-read "Request type: " '("prompt" "command" "ping")))
         (read-string "Content: ")
         nil))
  (sage-queue--ensure-directories)

  (let* ((id (sage-queue--generate-id))
         (request `((id . ,id)
                   (type . ,(symbol-name type))
                   (content . ,content)
                   (context . ,(or context (make-hash-table)))
                   (created_at . ,(sage-queue--timestamp))))
         (file (expand-file-name (format "%s.json" id) sage-queue--input-dir)))

    (sage-queue--write-json file request)
    (puthash id request sage-queue--pending-requests)
    (cl-incf (alist-get 'submitted sage-queue--stats))

    (message "Queue: Submitted request %s" id)
    id))

;;;###autoload
(defun sage-queue-respond (request-id status content &optional metadata)
  "Write a response for REQUEST-ID.
STATUS is 'success or 'error.
CONTENT is the response content.
METADATA is optional additional data."
  (interactive
   (list (read-string "Request ID: ")
         (intern (completing-read "Status: " '("success" "error")))
         (read-string "Content: ")
         nil))
  (sage-queue--ensure-directories)

  (let* ((response `((request_id . ,request-id)
                    (status . ,(symbol-name status))
                    (content . ,content)
                    (metadata . ,(or metadata (make-hash-table)))
                    (processed_at . ,(sage-queue--timestamp))))
         (file (expand-file-name (format "%s.json" request-id) sage-queue--output-dir)))

    (sage-queue--write-json file response)
    (cl-incf (alist-get 'processed sage-queue--stats))

    (message "Queue: Wrote response for %s" request-id)
    request-id))

;;;###autoload
(defun sage-queue-archive (request-id)
  "Archive processed request REQUEST-ID."
  (interactive
   (list (completing-read "Request ID: "
                         (mapcar #'file-name-base
                                (sage-queue--list-requests sage-queue--input-dir)))))
  (sage-queue--ensure-directories)

  (let ((input-file (expand-file-name (format "%s.json" request-id) sage-queue--input-dir))
        (output-file (expand-file-name (format "%s.json" request-id) sage-queue--output-dir))
        (archive-file (expand-file-name (format "%s.json" request-id) sage-queue--archive-dir)))

    ;; Move input file to archive
    (when (file-exists-p input-file)
      (rename-file input-file archive-file t))

    ;; Move output file to archive with response suffix
    (when (file-exists-p output-file)
      (rename-file output-file
                   (expand-file-name (format "%s-response.json" request-id) sage-queue--archive-dir)
                   t))

    (remhash request-id sage-queue--pending-requests)
    (cl-incf (alist-get 'archived sage-queue--stats))

    (message "Queue: Archived request %s" request-id)))

;;;###autoload
(defun sage-queue-poll ()
  "Check for pending requests and return the oldest one."
  (interactive)
  (sage-queue--ensure-directories)

  (let ((files (sage-queue--list-requests sage-queue--input-dir)))
    (if files
        (let* ((file (car (last files))) ; oldest file
               (request (sage-queue--read-json file)))
          (when request
            (message "Queue: Found pending request %s" (alist-get 'id request))
            request))
      (when (called-interactively-p 'any)
        (message "Queue: No pending requests"))
      nil)))

;;;###autoload
(defun sage-queue-status ()
  "Show queue status and statistics."
  (interactive)
  (sage-queue--ensure-directories)

  (let* ((input-count (length (sage-queue--list-requests sage-queue--input-dir)))
         (output-count (length (sage-queue--list-requests sage-queue--output-dir)))
         (archive-count (length (sage-queue--list-requests sage-queue--archive-dir)))
         (stats sage-queue--stats)
         (buf (get-buffer-create "*sage-queue-status*")))

    (with-current-buffer buf
      (erase-buffer)
      (insert "Sage Queue Status\n")
      (insert "========================\n\n")
      (insert (format "Queue Directory: %s\n\n" sage-queue-directory))

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
      (insert (if sage-queue-watch-mode "ACTIVE" "INACTIVE"))
      (insert "\n")

      (when sage-queue--watch-descriptor
        (insert (format "File Notify:         Active (descriptor: %s)\n" sage-queue--watch-descriptor)))

      (when sage-queue--poll-timer
        (insert "Polling Timer:       Active\n"))

      (goto-char (point-min))
      (special-mode))

    (pop-to-buffer buf)))

;;; Request Processing

(defun sage-queue-register-handler (type handler-fn)
  "Register HANDLER-FN for request TYPE.
HANDLER-FN should accept a request alist and return (status . content)."
  (puthash (symbol-name type) handler-fn sage-queue--request-handlers))

(defun sage-queue--process-request (request)
  "Process REQUEST and generate response."
  (let* ((id (alist-get 'id request))
         (type (alist-get 'type request))
         (content (alist-get 'content request))
         (handler (gethash type sage-queue--request-handlers)))

    (if handler
        (condition-case err
            (let ((result (funcall handler request)))
              (sage-queue-respond id
                                        (car result)
                                        (cdr result))
              (sage-queue-archive id))
          (error
           (sage-queue-respond id 'error (format "Handler error: %s" err))
           (cl-incf (alist-get 'errors sage-queue--stats))))
      (sage-queue-respond id 'error (format "No handler for type: %s" type))
      (cl-incf (alist-get 'errors sage-queue--stats)))))

(defun sage-queue--process-all ()
  "Process all pending requests."
  (let ((files (sage-queue--list-requests sage-queue--input-dir)))
    (dolist (file files)
      (when-let* ((request (sage-queue--read-json file)))
        (sage-queue--process-request request)))))

;;; File Watching

(defun sage-queue--on-file-change (event)
  "Handle file change EVENT."
  (let ((type (nth 1 event))
        (file (nth 2 event)))
    (when (and (eq type 'created)
               (string-suffix-p ".json" file)
               (not (string-suffix-p ".tmp" file))
               sage-queue-auto-process)
      (when-let* ((request (sage-queue--read-json file)))
        (message "Queue: Processing new request %s" (alist-get 'id request))
        (sage-queue--process-request request)))))

(defun sage-queue--start-watching ()
  "Start watching input directory for new requests."
  (sage-queue--ensure-directories)

  ;; Try file-notify first
  (condition-case err
      (progn
        (setq sage-queue--watch-descriptor
              (file-notify-add-watch sage-queue--input-dir
                                    '(change)
                                    #'sage-queue--on-file-change))
        (message "Queue: File notify active"))
    (error
     (message "Queue: File notify unavailable, using polling: %s" err)
     ;; Fall back to polling
     (setq sage-queue--poll-timer
           (run-with-timer sage-queue-poll-interval
                          sage-queue-poll-interval
                          (lambda ()
                            (when (and sage-queue-watch-mode
                                      sage-queue-auto-process)
                              (sage-queue--process-all))))))))

(defun sage-queue--stop-watching ()
  "Stop watching input directory."
  (when sage-queue--watch-descriptor
    (file-notify-rm-watch sage-queue--watch-descriptor)
    (setq sage-queue--watch-descriptor nil))

  (when sage-queue--poll-timer
    (cancel-timer sage-queue--poll-timer)
    (setq sage-queue--poll-timer nil)))

;;; Minor Mode

;;;###autoload
(define-minor-mode sage-queue-watch-mode
  "Minor mode to automatically process queue requests."
  :global t
  :lighter " GRQ"
  :group 'sage-queue

  (if sage-queue-watch-mode
      (progn
        (sage-queue--start-watching)
        (message "Sage Queue watch mode enabled"))
    (sage-queue--stop-watching)
    (message "Sage Queue watch mode disabled")))

;;; Cleanup

(defun sage-queue-cleanup-archives (&optional days)
  "Remove archived requests older than DAYS (default: `sage-queue-retention-days')."
  (interactive)
  (sage-queue--ensure-directories)

  (let* ((days (or days sage-queue-retention-days))
         (cutoff (time-subtract (current-time) (days-to-time days)))
         (files (directory-files sage-queue--archive-dir t "\\.json$"))
         (removed 0))

    (dolist (file files)
      (when (time-less-p (file-attribute-modification-time (file-attributes file)) cutoff)
        (delete-file file)
        (cl-incf removed)))

    (message "Queue: Removed %d archived request(s) older than %d days" removed days)))

;;; Built-in Handlers

(defun sage-queue--ping-handler (request)
  "Handle ping REQUEST."
  (cons 'success (format "pong: %s" (alist-get 'created_at request))))

(defun sage-queue--prompt-handler (request)
  "Handle prompt REQUEST using sage."
  (require 'sage)
  (let ((content (alist-get 'content request)))
    (condition-case err
        (let ((response (sage-exec content)))
          (cons 'success response))
      (error (cons 'error (format "Prompt error: %s" err))))))

;; Register built-in handlers
(sage-queue-register-handler 'ping #'sage-queue--ping-handler)
(sage-queue-register-handler 'prompt #'sage-queue--prompt-handler)

;;; Integration Functions

;;;###autoload
(defun sage-queue-send-to-agent (agent-id message)
  "Send MESSAGE to AGENT-ID via queue system."
  (interactive
   (list (read-string "Agent ID: ")
         (read-string "Message: ")))
  (sage-queue-submit 'prompt message `((target_agent . ,agent-id))))

;;;###autoload
(defun sage-queue-broadcast (message)
  "Broadcast MESSAGE to all listening agents."
  (interactive "sMessage: ")
  (sage-queue-submit 'command message '((broadcast . t))))

(provide 'sage-queue)
;;; sage-queue.el ends here
