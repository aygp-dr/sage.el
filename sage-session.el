;;; sage-session.el --- Session persistence for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides session persistence for sage, including:
;; - Save/load conversation sessions
;; - Export to JSON and Markdown
;; - Session statistics tracking
;; - Auto-save functionality
;; - Session management (list, delete, rename)

;;; Code:

(require 'cl-lib)
(require 'json)

;; Declare functions from iso8601
(declare-function parse-iso8601-time-string "iso8601" (string))

;;; Customization

(defgroup sage-session nil
  "Session persistence for Sage."
  :group 'sage
  :prefix "sage-session-")

(defcustom sage-session-directory
  (expand-file-name "sage/sessions" user-emacs-directory)
  "Directory to store session files."
  :type 'directory
  :group 'sage-session)

(defcustom sage-session-auto-save t
  "When non-nil, automatically save sessions."
  :type 'boolean
  :group 'sage-session)

(defcustom sage-session-auto-save-interval 300
  "Auto-save interval in seconds (default: 5 minutes)."
  :type 'integer
  :group 'sage-session)

;;; Data Structures

(cl-defstruct (sage-session
               (:constructor sage-session--create)
               (:copier nil))
  "Session data structure."
  (name nil :type string
        :documentation "Session name")
  (model nil :type string
         :documentation "Model used for this session")
  (provider nil :type symbol
            :documentation "Provider (gemini, ollama, openai)")
  (messages nil :type list
            :documentation "Conversation messages")
  (created-at nil :type string
              :documentation "ISO 8601 timestamp of creation")
  (updated-at nil :type string
              :documentation "ISO 8601 timestamp of last update")
  (message-count 0 :type integer
                 :documentation "Number of messages")
  (total-tokens 0 :type integer
                :documentation "Estimated total tokens")
  (workspace nil :type string
             :documentation "Workspace directory"))

;;; Variables

(defvar sage-session--current nil
  "Current active session.")

(defvar sage-session--auto-save-timer nil
  "Timer for auto-saving sessions.")

;;; Session Management

(defun sage-session--ensure-directory ()
  "Ensure session directory exists."
  (unless (file-directory-p sage-session-directory)
    (make-directory sage-session-directory t)))

(defun sage-session--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun sage-session--session-file (name)
  "Return file path for session NAME."
  (expand-file-name
   (concat (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" name) ".json")
   sage-session-directory))

(defun sage-session--estimate-tokens (text)
  "Estimate token count for TEXT (rough approximation: 1 token ≈ 4 chars)."
  (/ (length text) 4))

(defun sage-session--update-stats (session)
  "Update statistics for SESSION."
  (setf (sage-session-message-count session)
        (length (sage-session-messages session)))

  (setf (sage-session-total-tokens session)
        (apply #'+
               (mapcar (lambda (msg)
                         (sage-session--estimate-tokens
                          (alist-get 'content msg)))
                       (sage-session-messages session))))

  (setf (sage-session-updated-at session)
        (sage-session--timestamp)))

(defun sage-session-new (name &optional model provider workspace)
  "Create a new session with NAME.
Optional MODEL, PROVIDER, and WORKSPACE can be specified."
  (interactive "sSession name: ")
  (sage-session--ensure-directory)

  (let ((session (sage-session--create
                  :name name
                  :model (or model
                             (when (fboundp 'sage--get-model)
                               (sage--get-model))
                             "gemini-2.0-flash-exp")
                  :provider (or provider
                                (when (boundp 'sage-provider)
                                  sage-provider)
                                'gemini)
                  :messages nil
                  :created-at (sage-session--timestamp)
                  :updated-at (sage-session--timestamp)
                  :message-count 0
                  :total-tokens 0
                  :workspace (or workspace
                                 (when (fboundp 'sage--get-workspace)
                                   (sage--get-workspace))
                                 default-directory))))

    (setq sage-session--current session)
    (sage-session--start-auto-save)

    (when (called-interactively-p 'any)
      (message "Created session: %s" name))

    session))

(defun sage-session-add-message (role content)
  "Add a message to current session with ROLE and CONTENT."
  (when sage-session--current
    (let ((message `((role . ,role)
                     (content . ,content)
                     (timestamp . ,(sage-session--timestamp)))))
      (push message (sage-session-messages sage-session--current))
      (sage-session--update-stats sage-session--current))))

(defun sage-session--to-json (session)
  "Convert SESSION to JSON-encodable alist."
  `((name . ,(sage-session-name session))
    (model . ,(sage-session-model session))
    (provider . ,(symbol-name (sage-session-provider session)))
    (messages . ,(vconcat (reverse (sage-session-messages session))))
    (created_at . ,(sage-session-created-at session))
    (updated_at . ,(sage-session-updated-at session))
    (message_count . ,(sage-session-message-count session))
    (total_tokens . ,(sage-session-total-tokens session))
    (workspace . ,(sage-session-workspace session))))

(defun sage-session--from-json (data)
  "Convert JSON DATA to session structure."
  (sage-session--create
   :name (alist-get 'name data)
   :model (alist-get 'model data)
   :provider (intern (or (alist-get 'provider data) "gemini"))
   :messages (reverse (append (alist-get 'messages data) nil))
   :created-at (alist-get 'created_at data)
   :updated-at (alist-get 'updated_at data)
   :message-count (or (alist-get 'message_count data) 0)
   :total-tokens (or (alist-get 'total_tokens data) 0)
   :workspace (alist-get 'workspace data)))

(defun sage-session-save (&optional session)
  "Save SESSION (or current session) to file."
  (interactive)
  (let ((sess (or session sage-session--current)))
    (unless sess
      (user-error "No active session to save"))

    (sage-session--ensure-directory)
    (sage-session--update-stats sess)

    (let ((file (sage-session--session-file
                 (sage-session-name sess)))
          (json-encoding-pretty-print t))
      (with-temp-file file
        (insert (json-encode (sage-session--to-json sess))))

      (when (called-interactively-p 'any)
        (message "Saved session: %s (%d messages, ~%d tokens)"
                 (sage-session-name sess)
                 (sage-session-message-count sess)
                 (sage-session-total-tokens sess)))

      file)))

(defun sage-session-load (name)
  "Load session from file by NAME."
  (interactive
   (list (completing-read "Load session: "
                          (sage-session-list-names)
                          nil t)))

  (let ((file (sage-session--session-file name)))
    (unless (file-exists-p file)
      (user-error "Session not found: %s" name))

    (let* ((json-object-type 'alist)
           (json-array-type 'vector)
           (json-key-type 'symbol)
           (data (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (json-read)))
           (session (sage-session--from-json data)))

      (setq sage-session--current session)
      (sage-session--start-auto-save)

      (when (called-interactively-p 'any)
        (message "Loaded session: %s (%d messages)"
                 name (sage-session-message-count session)))

      session)))

(defun sage-session-list-names ()
  "Return list of available session names."
  (when (file-directory-p sage-session-directory)
    (mapcar (lambda (f)
              (file-name-sans-extension (file-name-nondirectory f)))
            (directory-files sage-session-directory t "\\.json$"))))

(defun sage-session-list ()
  "List all available sessions."
  (interactive)
  (let ((sessions (sage-session-list-names)))
    (if sessions
        (if (called-interactively-p 'any)
            (message "Available sessions: %s" (string-join sessions ", "))
          sessions)
      (message "No saved sessions found"))))

(defun sage-session-delete (name)
  "Delete session by NAME."
  (interactive
   (list (completing-read "Delete session: "
                          (sage-session-list-names)
                          nil t)))

  (let ((file (sage-session--session-file name)))
    (when (and (file-exists-p file)
               (yes-or-no-p (format "Delete session '%s'? " name)))
      (delete-file file)
      (message "Deleted session: %s" name))))

(defun sage-session-rename (old-name new-name)
  "Rename session from OLD-NAME to NEW-NAME."
  (interactive
   (let ((old (completing-read "Rename session: "
                               (sage-session-list-names)
                               nil t)))
     (list old (read-string "New name: " old))))

  (let ((old-file (sage-session--session-file old-name))
        (new-file (sage-session--session-file new-name)))
    (unless (file-exists-p old-file)
      (user-error "Session not found: %s" old-name))
    (when (file-exists-p new-file)
      (user-error "Session already exists: %s" new-name))

    ;; Load, update name, save to new file, delete old
    (let* ((json-object-type 'alist)
           (json-array-type 'vector)
           (json-key-type 'symbol)
           (data (with-temp-buffer
                   (insert-file-contents old-file)
                   (goto-char (point-min))
                   (json-read)))
           (session (sage-session--from-json data)))

      (setf (sage-session-name session) new-name)

      (let ((json-encoding-pretty-print t))
        (with-temp-file new-file
          (insert (json-encode (sage-session--to-json session)))))

      (delete-file old-file)

      (when (and sage-session--current
                 (string= (sage-session-name sage-session--current)
                          old-name))
        (setq sage-session--current session))

      (message "Renamed session: %s -> %s" old-name new-name))))

;;; Export

(defun sage-session-export-json (filename &optional session)
  "Export SESSION (or current) to FILENAME as JSON."
  (interactive "FExport to JSON file: ")
  (let ((sess (or session sage-session--current)))
    (unless sess
      (user-error "No active session to export"))

    (let ((json-encoding-pretty-print t))
      (with-temp-file filename
        (insert (json-encode (sage-session--to-json sess))))

      (message "Exported session to: %s" filename))))

(defun sage-session-export-markdown (filename &optional session)
  "Export SESSION (or current) to FILENAME as Markdown."
  (interactive "FExport to Markdown file: ")
  (let ((sess (or session sage-session--current)))
    (unless sess
      (user-error "No active session to export"))

    (with-temp-file filename
      (insert (format "# Session: %s\n\n" (sage-session-name sess)))
      (insert (format "- **Model**: %s\n" (sage-session-model sess)))
      (insert (format "- **Provider**: %s\n" (sage-session-provider sess)))
      (insert (format "- **Created**: %s\n" (sage-session-created-at sess)))
      (insert (format "- **Updated**: %s\n" (sage-session-updated-at sess)))
      (insert (format "- **Messages**: %d\n" (sage-session-message-count sess)))
      (insert (format "- **Tokens**: ~%d\n" (sage-session-total-tokens sess)))
      (when (sage-session-workspace sess)
        (insert (format "- **Workspace**: %s\n" (sage-session-workspace sess))))
      (insert "\n---\n\n")

      (insert "## Conversation\n\n")
      (dolist (msg (reverse (sage-session-messages sess)))
        (let ((role (alist-get 'role msg))
              (content (alist-get 'content msg))
              (timestamp (alist-get 'timestamp msg)))
          (insert (format "### %s" (capitalize role)))
          (when timestamp
            (insert (format " _%s_" timestamp)))
          (insert "\n\n")
          (insert content)
          (insert "\n\n"))))

    (message "Exported session to: %s" filename)))

;;; Statistics

(defun sage-session-stats (&optional session)
  "Display statistics for SESSION (or current)."
  (interactive)
  (let ((sess (or session sage-session--current)))
    (unless sess
      (user-error "No active session"))

    (sage-session--update-stats sess)

    (let* ((messages (sage-session-messages sess))
           (user-msgs (seq-count (lambda (m) (string= (alist-get 'role m) "user"))
                                 messages))
           (assistant-msgs (seq-count (lambda (m) (string= (alist-get 'role m) "assistant"))
                                      messages))
           (created (sage-session-created-at sess))
           (updated (sage-session-updated-at sess))
           (duration (if (and created updated)
                         (format-seconds "%h hours, %m minutes"
                                         (float-time
                                          (time-subtract
                                           (parse-iso8601-time-string updated)
                                           (parse-iso8601-time-string created))))
                       "N/A")))

      (message "Session: %s | Model: %s | Messages: %d (user: %d, assistant: %d) | Tokens: ~%d | Duration: %s"
               (sage-session-name sess)
               (sage-session-model sess)
               (sage-session-message-count sess)
               user-msgs
               assistant-msgs
               (sage-session-total-tokens sess)
               duration))))

;;; Auto-save

(defun sage-session--auto-save ()
  "Auto-save current session."
  (when (and sage-session--current
             sage-session-auto-save)
    (condition-case err
        (sage-session-save sage-session--current)
      (error
       (message "Auto-save failed: %s" (error-message-string err))))))

(defun sage-session--start-auto-save ()
  "Start auto-save timer."
  (when sage-session--auto-save-timer
    (cancel-timer sage-session--auto-save-timer))

  (when sage-session-auto-save
    (setq sage-session--auto-save-timer
          (run-with-timer sage-session-auto-save-interval
                          sage-session-auto-save-interval
                          #'sage-session--auto-save))))

(defun sage-session-stop-auto-save ()
  "Stop auto-save timer."
  (interactive)
  (when sage-session--auto-save-timer
    (cancel-timer sage-session--auto-save-timer)
    (setq sage-session--auto-save-timer nil)
    (message "Auto-save stopped")))

;;; Integration Helpers

(defun sage-session-current ()
  "Return current session or nil."
  sage-session--current)

(defun sage-session-get-messages ()
  "Get messages from current session."
  (when sage-session--current
    (reverse (sage-session-messages sage-session--current))))

(defun sage-session-set-messages (messages)
  "Set MESSAGES in current session."
  (when sage-session--current
    (setf (sage-session-messages sage-session--current)
          (reverse messages))
    (sage-session--update-stats sage-session--current)))

(provide 'sage-session)
;;; sage-session.el ends here
