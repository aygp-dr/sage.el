;;; gemini-repl-session.el --- Session persistence for gemini-repl -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides session persistence for gemini-repl, including:
;; - Save/load conversation sessions
;; - Export to JSON and Markdown
;; - Session statistics tracking
;; - Auto-save functionality
;; - Session management (list, delete, rename)

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup gemini-repl-session nil
  "Session persistence for Gemini REPL."
  :group 'gemini-repl
  :prefix "gemini-repl-session-")

(defcustom gemini-repl-session-directory
  (expand-file-name "gemini-repl/sessions" user-emacs-directory)
  "Directory to store session files."
  :type 'directory
  :group 'gemini-repl-session)

(defcustom gemini-repl-session-auto-save t
  "When non-nil, automatically save sessions."
  :type 'boolean
  :group 'gemini-repl-session)

(defcustom gemini-repl-session-auto-save-interval 300
  "Auto-save interval in seconds (default: 5 minutes)."
  :type 'integer
  :group 'gemini-repl-session)

;;; Data Structures

(cl-defstruct (gemini-repl-session
               (:constructor gemini-repl-session--create)
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

(defvar gemini-repl-session--current nil
  "Current active session.")

(defvar gemini-repl-session--auto-save-timer nil
  "Timer for auto-saving sessions.")

;;; Session Management

(defun gemini-repl-session--ensure-directory ()
  "Ensure session directory exists."
  (unless (file-directory-p gemini-repl-session-directory)
    (make-directory gemini-repl-session-directory t)))

(defun gemini-repl-session--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun gemini-repl-session--session-file (name)
  "Return file path for session NAME."
  (expand-file-name
   (concat (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" name) ".json")
   gemini-repl-session-directory))

(defun gemini-repl-session--estimate-tokens (text)
  "Estimate token count for TEXT (rough approximation: 1 token ≈ 4 chars)."
  (/ (length text) 4))

(defun gemini-repl-session--update-stats (session)
  "Update statistics for SESSION."
  (setf (gemini-repl-session-message-count session)
        (length (gemini-repl-session-messages session)))

  (setf (gemini-repl-session-total-tokens session)
        (apply #'+
               (mapcar (lambda (msg)
                         (gemini-repl-session--estimate-tokens
                          (alist-get 'content msg)))
                       (gemini-repl-session-messages session))))

  (setf (gemini-repl-session-updated-at session)
        (gemini-repl-session--timestamp)))

(defun gemini-repl-session-new (name &optional model provider workspace)
  "Create a new session with NAME.
Optional MODEL, PROVIDER, and WORKSPACE can be specified."
  (interactive "sSession name: ")
  (gemini-repl-session--ensure-directory)

  (let ((session (gemini-repl-session--create
                  :name name
                  :model (or model
                             (when (fboundp 'gemini-repl--get-model)
                               (gemini-repl--get-model))
                             "gemini-2.0-flash-exp")
                  :provider (or provider
                                (when (boundp 'gemini-repl-provider)
                                  gemini-repl-provider)
                                'gemini)
                  :messages nil
                  :created-at (gemini-repl-session--timestamp)
                  :updated-at (gemini-repl-session--timestamp)
                  :message-count 0
                  :total-tokens 0
                  :workspace (or workspace
                                 (when (fboundp 'gemini-repl--get-workspace)
                                   (gemini-repl--get-workspace))
                                 default-directory))))

    (setq gemini-repl-session--current session)
    (gemini-repl-session--start-auto-save)

    (when (called-interactively-p 'any)
      (message "Created session: %s" name))

    session))

(defun gemini-repl-session-add-message (role content)
  "Add a message to current session with ROLE and CONTENT."
  (when gemini-repl-session--current
    (let ((message `((role . ,role)
                     (content . ,content)
                     (timestamp . ,(gemini-repl-session--timestamp)))))
      (push message (gemini-repl-session-messages gemini-repl-session--current))
      (gemini-repl-session--update-stats gemini-repl-session--current))))

(defun gemini-repl-session--to-json (session)
  "Convert SESSION to JSON-encodable alist."
  `((name . ,(gemini-repl-session-name session))
    (model . ,(gemini-repl-session-model session))
    (provider . ,(symbol-name (gemini-repl-session-provider session)))
    (messages . ,(vconcat (reverse (gemini-repl-session-messages session))))
    (created_at . ,(gemini-repl-session-created-at session))
    (updated_at . ,(gemini-repl-session-updated-at session))
    (message_count . ,(gemini-repl-session-message-count session))
    (total_tokens . ,(gemini-repl-session-total-tokens session))
    (workspace . ,(gemini-repl-session-workspace session))))

(defun gemini-repl-session--from-json (data)
  "Convert JSON DATA to session structure."
  (gemini-repl-session--create
   :name (alist-get 'name data)
   :model (alist-get 'model data)
   :provider (intern (or (alist-get 'provider data) "gemini"))
   :messages (reverse (append (alist-get 'messages data) nil))
   :created-at (alist-get 'created_at data)
   :updated-at (alist-get 'updated_at data)
   :message-count (or (alist-get 'message_count data) 0)
   :total-tokens (or (alist-get 'total_tokens data) 0)
   :workspace (alist-get 'workspace data)))

(defun gemini-repl-session-save (&optional session)
  "Save SESSION (or current session) to file."
  (interactive)
  (let ((sess (or session gemini-repl-session--current)))
    (unless sess
      (user-error "No active session to save"))

    (gemini-repl-session--ensure-directory)
    (gemini-repl-session--update-stats sess)

    (let ((file (gemini-repl-session--session-file
                 (gemini-repl-session-name sess)))
          (json-encoding-pretty-print t))
      (with-temp-file file
        (insert (json-encode (gemini-repl-session--to-json sess))))

      (when (called-interactively-p 'any)
        (message "Saved session: %s (%d messages, ~%d tokens)"
                 (gemini-repl-session-name sess)
                 (gemini-repl-session-message-count sess)
                 (gemini-repl-session-total-tokens sess)))

      file)))

(defun gemini-repl-session-load (name)
  "Load session from file by NAME."
  (interactive
   (list (completing-read "Load session: "
                          (gemini-repl-session-list-names)
                          nil t)))

  (let ((file (gemini-repl-session--session-file name)))
    (unless (file-exists-p file)
      (user-error "Session not found: %s" name))

    (let* ((json-object-type 'alist)
           (json-array-type 'vector)
           (json-key-type 'symbol)
           (data (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (json-read)))
           (session (gemini-repl-session--from-json data)))

      (setq gemini-repl-session--current session)
      (gemini-repl-session--start-auto-save)

      (when (called-interactively-p 'any)
        (message "Loaded session: %s (%d messages)"
                 name (gemini-repl-session-message-count session)))

      session)))

(defun gemini-repl-session-list-names ()
  "Return list of available session names."
  (when (file-directory-p gemini-repl-session-directory)
    (mapcar (lambda (f)
              (file-name-sans-extension (file-name-nondirectory f)))
            (directory-files gemini-repl-session-directory t "\\.json$"))))

(defun gemini-repl-session-list ()
  "List all available sessions."
  (interactive)
  (let ((sessions (gemini-repl-session-list-names)))
    (if sessions
        (if (called-interactively-p 'any)
            (message "Available sessions: %s" (string-join sessions ", "))
          sessions)
      (message "No saved sessions found"))))

(defun gemini-repl-session-delete (name)
  "Delete session by NAME."
  (interactive
   (list (completing-read "Delete session: "
                          (gemini-repl-session-list-names)
                          nil t)))

  (let ((file (gemini-repl-session--session-file name)))
    (when (and (file-exists-p file)
               (yes-or-no-p (format "Delete session '%s'? " name)))
      (delete-file file)
      (message "Deleted session: %s" name))))

(defun gemini-repl-session-rename (old-name new-name)
  "Rename session from OLD-NAME to NEW-NAME."
  (interactive
   (let ((old (completing-read "Rename session: "
                               (gemini-repl-session-list-names)
                               nil t)))
     (list old (read-string "New name: " old))))

  (let ((old-file (gemini-repl-session--session-file old-name))
        (new-file (gemini-repl-session--session-file new-name)))
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
           (session (gemini-repl-session--from-json data)))

      (setf (gemini-repl-session-name session) new-name)

      (let ((json-encoding-pretty-print t))
        (with-temp-file new-file
          (insert (json-encode (gemini-repl-session--to-json session)))))

      (delete-file old-file)

      (when (and gemini-repl-session--current
                 (string= (gemini-repl-session-name gemini-repl-session--current)
                          old-name))
        (setq gemini-repl-session--current session))

      (message "Renamed session: %s -> %s" old-name new-name))))

;;; Export

(defun gemini-repl-session-export-json (filename &optional session)
  "Export SESSION (or current) to FILENAME as JSON."
  (interactive "FExport to JSON file: ")
  (let ((sess (or session gemini-repl-session--current)))
    (unless sess
      (user-error "No active session to export"))

    (let ((json-encoding-pretty-print t))
      (with-temp-file filename
        (insert (json-encode (gemini-repl-session--to-json sess))))

      (message "Exported session to: %s" filename))))

(defun gemini-repl-session-export-markdown (filename &optional session)
  "Export SESSION (or current) to FILENAME as Markdown."
  (interactive "FExport to Markdown file: ")
  (let ((sess (or session gemini-repl-session--current)))
    (unless sess
      (user-error "No active session to export"))

    (with-temp-file filename
      (insert (format "# Session: %s\n\n" (gemini-repl-session-name sess)))
      (insert (format "- **Model**: %s\n" (gemini-repl-session-model sess)))
      (insert (format "- **Provider**: %s\n" (gemini-repl-session-provider sess)))
      (insert (format "- **Created**: %s\n" (gemini-repl-session-created-at sess)))
      (insert (format "- **Updated**: %s\n" (gemini-repl-session-updated-at sess)))
      (insert (format "- **Messages**: %d\n" (gemini-repl-session-message-count sess)))
      (insert (format "- **Tokens**: ~%d\n" (gemini-repl-session-total-tokens sess)))
      (when (gemini-repl-session-workspace sess)
        (insert (format "- **Workspace**: %s\n" (gemini-repl-session-workspace sess))))
      (insert "\n---\n\n")

      (insert "## Conversation\n\n")
      (dolist (msg (reverse (gemini-repl-session-messages sess)))
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

(defun gemini-repl-session-stats (&optional session)
  "Display statistics for SESSION (or current)."
  (interactive)
  (let ((sess (or session gemini-repl-session--current)))
    (unless sess
      (user-error "No active session"))

    (gemini-repl-session--update-stats sess)

    (let* ((messages (gemini-repl-session-messages sess))
           (user-msgs (seq-count (lambda (m) (string= (alist-get 'role m) "user"))
                                 messages))
           (assistant-msgs (seq-count (lambda (m) (string= (alist-get 'role m) "assistant"))
                                      messages))
           (created (gemini-repl-session-created-at sess))
           (updated (gemini-repl-session-updated-at sess))
           (duration (if (and created updated)
                         (format-seconds "%h hours, %m minutes"
                                         (float-time
                                          (time-subtract
                                           (parse-iso8601-time-string updated)
                                           (parse-iso8601-time-string created))))
                       "N/A")))

      (message "Session: %s | Model: %s | Messages: %d (user: %d, assistant: %d) | Tokens: ~%d | Duration: %s"
               (gemini-repl-session-name sess)
               (gemini-repl-session-model sess)
               (gemini-repl-session-message-count sess)
               user-msgs
               assistant-msgs
               (gemini-repl-session-total-tokens sess)
               duration))))

;;; Auto-save

(defun gemini-repl-session--auto-save ()
  "Auto-save current session."
  (when (and gemini-repl-session--current
             gemini-repl-session-auto-save)
    (condition-case err
        (gemini-repl-session-save gemini-repl-session--current)
      (error
       (message "Auto-save failed: %s" (error-message-string err))))))

(defun gemini-repl-session--start-auto-save ()
  "Start auto-save timer."
  (when gemini-repl-session--auto-save-timer
    (cancel-timer gemini-repl-session--auto-save-timer))

  (when gemini-repl-session-auto-save
    (setq gemini-repl-session--auto-save-timer
          (run-with-timer gemini-repl-session-auto-save-interval
                          gemini-repl-session-auto-save-interval
                          #'gemini-repl-session--auto-save))))

(defun gemini-repl-session-stop-auto-save ()
  "Stop auto-save timer."
  (interactive)
  (when gemini-repl-session--auto-save-timer
    (cancel-timer gemini-repl-session--auto-save-timer)
    (setq gemini-repl-session--auto-save-timer nil)
    (message "Auto-save stopped")))

;;; Integration Helpers

(defun gemini-repl-session-current ()
  "Return current session or nil."
  gemini-repl-session--current)

(defun gemini-repl-session-get-messages ()
  "Get messages from current session."
  (when gemini-repl-session--current
    (reverse (gemini-repl-session-messages gemini-repl-session--current))))

(defun gemini-repl-session-set-messages (messages)
  "Set MESSAGES in current session."
  (when gemini-repl-session--current
    (setf (gemini-repl-session-messages gemini-repl-session--current)
          (reverse messages))
    (gemini-repl-session--update-stats gemini-repl-session--current)))

(provide 'gemini-repl-session)
;;; gemini-repl-session.el ends here
