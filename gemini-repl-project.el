;;; gemini-repl-project.el --- Project-based conversation history for gemini-repl -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (project "0.9.8"))
;; Keywords: ai, tools, project, history
;; URL: https://github.com/aygp-dr/gemini-repl-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gemini-repl-project provides project-based conversation history
;; management for the gemini-repl package. Each project directory
;; gets its own conversation history that persists across sessions.
;;
;; Features:
;; - Per-directory conversation storage
;; - Auto-load project conversations on REPL start
;; - JSONL format for efficient append operations
;; - Archive old conversations
;; - Project metadata tracking
;; - Integration with project.el and projectile
;;
;; Storage structure:
;;   ~/.emacs.d/gemini-repl/projects/<encoded-path>/
;;   ├── conversation.jsonl   # Current conversation (append-only)
;;   ├── metadata.json        # Project metadata
;;   └── history/            # Archived conversations
;;
;; Path encoding:
;;   "/" → "-"
;;   Example: /home/user/project → -home-user-project
;;
;; Usage:
;;   (gemini-repl-project-load)         ; Load current project conversation
;;   (gemini-repl-project-append msg)   ; Append message
;;   (gemini-repl-project-archive)      ; Archive current conversation
;;   (gemini-repl-project-clear)        ; Clear conversation
;;   (gemini-repl-project-metadata)     ; Get/set metadata

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'project nil t)  ; Optional dependency

;;; Customization

(defgroup gemini-repl-project nil
  "Project-based conversation history for gemini-repl."
  :group 'gemini-repl
  :prefix "gemini-repl-project-")

(defcustom gemini-repl-project-directory
  (expand-file-name "gemini-repl/projects" user-emacs-directory)
  "Directory to store project-based conversations."
  :type 'directory
  :group 'gemini-repl-project)

(defcustom gemini-repl-project-auto-load t
  "When non-nil, automatically load project conversation on REPL start."
  :type 'boolean
  :group 'gemini-repl-project)

(defcustom gemini-repl-project-auto-save t
  "When non-nil, save each message immediately after appending."
  :type 'boolean
  :group 'gemini-repl-project)

(defcustom gemini-repl-project-max-messages 1000
  "Maximum number of messages to keep in conversation.
When exceeded, oldest messages are automatically archived."
  :type 'integer
  :group 'gemini-repl-project)

(defcustom gemini-repl-project-prefer-projectile nil
  "When non-nil, prefer projectile over project.el for root detection."
  :type 'boolean
  :group 'gemini-repl-project)

;;; Variables

(defvar gemini-repl-project--current-dir nil
  "Current project directory.")

(defvar gemini-repl-project--current-conversation nil
  "Current conversation messages (list of plists).")

(defvar gemini-repl-project--current-metadata nil
  "Current project metadata (plist).")

;;; Path Encoding

(defun gemini-repl-project-encode-path (path)
  "Encode PATH for use as directory name.
Replaces '/' with '-' (like Claude Code does)."
  (let ((expanded (expand-file-name path)))
    (replace-regexp-in-string "/" "-" expanded)))

(defun gemini-repl-project--storage-dir (project-dir)
  "Return storage directory for PROJECT-DIR."
  (expand-file-name
   (gemini-repl-project-encode-path project-dir)
   gemini-repl-project-directory))

(defun gemini-repl-project--conversation-file (project-dir)
  "Return conversation JSONL file path for PROJECT-DIR."
  (expand-file-name "conversation.jsonl"
                    (gemini-repl-project--storage-dir project-dir)))

(defun gemini-repl-project--metadata-file (project-dir)
  "Return metadata JSON file path for PROJECT-DIR."
  (expand-file-name "metadata.json"
                    (gemini-repl-project--storage-dir project-dir)))

(defun gemini-repl-project--history-dir (project-dir)
  "Return history archive directory for PROJECT-DIR."
  (expand-file-name "history"
                    (gemini-repl-project--storage-dir project-dir)))

;;; Project Root Detection

(defun gemini-repl-project-dir ()
  "Get project root directory.
Uses project.el or projectile (if available) to detect root.
Falls back to `default-directory' if no project found."
  (or gemini-repl-project--current-dir
      (cond
       ;; Projectile (if enabled and available)
       ((and gemini-repl-project-prefer-projectile
             (fboundp 'projectile-project-root))
        (condition-case nil
            (projectile-project-root)
          (error default-directory)))

       ;; project.el
       ((and (fboundp 'project-current)
             (project-current))
        (project-root (project-current)))

       ;; Git root fallback
       ((locate-dominating-file default-directory ".git")
        (expand-file-name (locate-dominating-file default-directory ".git")))

       ;; Default to current directory
       (t default-directory))))

(defun gemini-repl-project-set-dir (dir)
  "Manually set project directory to DIR."
  (interactive "DProject directory: ")
  (setq gemini-repl-project--current-dir (expand-file-name dir))
  (message "Project directory set to: %s" gemini-repl-project--current-dir))

;;; Conversation Management

(defun gemini-repl-project--ensure-storage ()
  "Ensure project storage directories exist."
  (let* ((proj-dir (gemini-repl-project-dir))
         (storage-dir (gemini-repl-project--storage-dir proj-dir))
         (history-dir (gemini-repl-project--history-dir proj-dir)))
    (unless (file-directory-p storage-dir)
      (make-directory storage-dir t))
    (unless (file-directory-p history-dir)
      (make-directory history-dir t))))

(defun gemini-repl-project--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun gemini-repl-project--create-message (role content)
  "Create a message plist with ROLE and CONTENT."
  `(:role ,role
    :content ,content
    :timestamp ,(gemini-repl-project--timestamp)))

;;;###autoload
(defun gemini-repl-project-load ()
  "Load conversation for current project.
Returns list of messages or nil if no conversation exists."
  (interactive)
  (let* ((proj-dir (gemini-repl-project-dir))
         (conv-file (gemini-repl-project--conversation-file proj-dir))
         (meta-file (gemini-repl-project--metadata-file proj-dir)))

    (gemini-repl-project--ensure-storage)

    ;; Load conversation
    (setq gemini-repl-project--current-conversation
          (if (file-exists-p conv-file)
              (with-temp-buffer
                (insert-file-contents conv-file)
                (let ((messages nil))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let ((line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
                      (unless (string-empty-p (string-trim line))
                        (condition-case err
                            (push (json-read-from-string line) messages)
                          (error
                           (message "Error parsing JSONL line: %s" err)))))
                    (forward-line 1))
                  (nreverse messages)))
            nil))

    ;; Load metadata
    (setq gemini-repl-project--current-metadata
          (if (file-exists-p meta-file)
              (with-temp-buffer
                (insert-file-contents meta-file)
                (let ((json-object-type 'plist)
                      (json-key-type 'keyword))
                  (json-read)))
            (gemini-repl-project--create-metadata proj-dir)))

    (when (called-interactively-p 'any)
      (message "Loaded %d messages for project: %s"
               (length gemini-repl-project--current-conversation)
               proj-dir))

    gemini-repl-project--current-conversation))

(defun gemini-repl-project--create-metadata (proj-dir)
  "Create initial metadata for PROJ-DIR."
  `(:project_dir ,proj-dir
    :created_at ,(gemini-repl-project--timestamp)
    :updated_at ,(gemini-repl-project--timestamp)
    :message_count 0
    :total_size 0))

;;;###autoload
(defun gemini-repl-project-append (message)
  "Append MESSAGE to current project conversation.
MESSAGE can be:
  - A plist with :role and :content
  - A list (role content) which will be converted to plist

Automatically saves if `gemini-repl-project-auto-save' is t."
  (interactive)
  (let* ((proj-dir (gemini-repl-project-dir))
         (conv-file (gemini-repl-project--conversation-file proj-dir))
         (msg (cond
               ;; Already a plist with timestamp
               ((and (listp message) (plist-get message :timestamp))
                message)
               ;; Plist without timestamp
               ((and (listp message) (plist-get message :role))
                (plist-put message :timestamp (gemini-repl-project--timestamp)))
               ;; List form (role content)
               ((and (listp message) (= (length message) 2))
                (gemini-repl-project--create-message (car message) (cadr message)))
               (t
                (error "Invalid message format")))))

    (gemini-repl-project--ensure-storage)

    ;; Append to in-memory conversation
    (push msg gemini-repl-project--current-conversation)

    ;; Check if we need to archive old messages
    (when (> (length gemini-repl-project--current-conversation)
             gemini-repl-project-max-messages)
      (gemini-repl-project--auto-archive))

    ;; Auto-save to disk if enabled
    (when gemini-repl-project-auto-save
      (condition-case err
          (progn
            ;; Append to JSONL file
            (with-temp-buffer
              (insert (json-encode msg))
              (insert "\n")
              (append-to-file (point-min) (point-max) conv-file))

            ;; Update metadata
            (gemini-repl-project--update-metadata))
        (error
         (message "Error saving message: %s" err))))

    msg))

(defun gemini-repl-project--update-metadata ()
  "Update and save project metadata."
  (let* ((proj-dir (gemini-repl-project-dir))
         (meta-file (gemini-repl-project--metadata-file proj-dir))
         (conv-file (gemini-repl-project--conversation-file proj-dir)))

    (setq gemini-repl-project--current-metadata
          (plist-put gemini-repl-project--current-metadata
                     :updated_at (gemini-repl-project--timestamp)))

    (setq gemini-repl-project--current-metadata
          (plist-put gemini-repl-project--current-metadata
                     :message_count
                     (length gemini-repl-project--current-conversation)))

    (when (file-exists-p conv-file)
      (setq gemini-repl-project--current-metadata
            (plist-put gemini-repl-project--current-metadata
                       :total_size
                       (file-attribute-size (file-attributes conv-file)))))

    ;; Save metadata
    (with-temp-file meta-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode gemini-repl-project--current-metadata))))))

(defun gemini-repl-project--auto-archive ()
  "Automatically archive oldest messages when limit is exceeded."
  (let* ((excess (- (length gemini-repl-project--current-conversation)
                    gemini-repl-project-max-messages))
         (to-archive (nthcdr gemini-repl-project-max-messages
                            (reverse gemini-repl-project--current-conversation))))

    (when (> excess 0)
      (message "Auto-archiving %d old messages..." excess)
      (gemini-repl-project-archive nil to-archive)

      ;; Keep only recent messages
      (setq gemini-repl-project--current-conversation
            (seq-take gemini-repl-project--current-conversation
                     gemini-repl-project-max-messages))

      ;; Rewrite conversation file
      (let ((conv-file (gemini-repl-project--conversation-file
                       (gemini-repl-project-dir))))
        (with-temp-file conv-file
          (dolist (msg (reverse gemini-repl-project--current-conversation))
            (insert (json-encode msg))
            (insert "\n")))))))

;;;###autoload
(defun gemini-repl-project-archive (&optional name messages)
  "Archive current conversation to history.
NAME is optional archive name (defaults to timestamp).
MESSAGES is optional list of messages to archive (defaults to current conversation)."
  (interactive)
  (let* ((proj-dir (gemini-repl-project-dir))
         (history-dir (gemini-repl-project--history-dir proj-dir))
         (archive-name (or name (format-time-string "%Y%m%d-%H%M%S")))
         (archive-file (expand-file-name
                       (concat archive-name ".jsonl")
                       history-dir))
         (msgs (or messages gemini-repl-project--current-conversation)))

    (gemini-repl-project--ensure-storage)

    (when msgs
      (with-temp-file archive-file
        (dolist (msg (reverse msgs))
          (insert (json-encode msg))
          (insert "\n")))

      (when (called-interactively-p 'any)
        (message "Archived %d messages to: %s"
                 (length msgs)
                 archive-file))

      archive-file)))

;;;###autoload
(defun gemini-repl-project-clear ()
  "Clear current project conversation.
Archives existing conversation before clearing."
  (interactive)
  (when (or (null gemini-repl-project--current-conversation)
            (yes-or-no-p "Clear current conversation? (will be archived) "))
    (let ((proj-dir (gemini-repl-project-dir)))

      ;; Archive current conversation if it exists
      (when gemini-repl-project--current-conversation
        (gemini-repl-project-archive))

      ;; Clear in-memory conversation
      (setq gemini-repl-project--current-conversation nil)

      ;; Delete conversation file
      (let ((conv-file (gemini-repl-project--conversation-file proj-dir)))
        (when (file-exists-p conv-file)
          (delete-file conv-file)))

      ;; Update metadata
      (gemini-repl-project--update-metadata)

      (message "Conversation cleared for project: %s" proj-dir))))

;;;###autoload
(defun gemini-repl-project-metadata (&optional key value)
  "Get or set project metadata.
With no arguments, returns entire metadata plist.
With KEY, returns value for that key.
With KEY and VALUE, sets the key to value and saves metadata."
  (interactive)
  (cond
   ;; Get entire metadata
   ((and (null key) (null value))
    (or gemini-repl-project--current-metadata
        (gemini-repl-project--create-metadata (gemini-repl-project-dir))))

   ;; Get specific key
   ((and key (null value))
    (plist-get gemini-repl-project--current-metadata key))

   ;; Set key-value
   ((and key value)
    (unless gemini-repl-project--current-metadata
      (setq gemini-repl-project--current-metadata
            (gemini-repl-project--create-metadata (gemini-repl-project-dir))))

    (setq gemini-repl-project--current-metadata
          (plist-put gemini-repl-project--current-metadata key value))

    (gemini-repl-project--update-metadata)
    value)))

;;; History Management

;;;###autoload
(defun gemini-repl-project-list-archives ()
  "List all archived conversations for current project."
  (interactive)
  (let* ((proj-dir (gemini-repl-project-dir))
         (history-dir (gemini-repl-project--history-dir proj-dir)))

    (if (file-directory-p history-dir)
        (let ((archives (directory-files history-dir nil "\\.jsonl$")))
          (if archives
              (progn
                (when (called-interactively-p 'any)
                  (message "Archives for %s:\n%s"
                           proj-dir
                           (mapconcat #'identity archives "\n")))
                archives)
            (when (called-interactively-p 'any)
              (message "No archives found for project: %s" proj-dir))
            nil))
      (when (called-interactively-p 'any)
        (message "No history directory for project: %s" proj-dir))
      nil)))

;;;###autoload
(defun gemini-repl-project-load-archive (name)
  "Load archived conversation by NAME.
Replaces current conversation with archived one."
  (interactive
   (list (completing-read "Load archive: "
                         (gemini-repl-project-list-archives)
                         nil t)))

  (let* ((proj-dir (gemini-repl-project-dir))
         (history-dir (gemini-repl-project--history-dir proj-dir))
         (archive-file (expand-file-name name history-dir)))

    (unless (file-exists-p archive-file)
      (user-error "Archive not found: %s" name))

    ;; Archive current conversation first
    (when gemini-repl-project--current-conversation
      (gemini-repl-project-archive))

    ;; Load archive
    (with-temp-buffer
      (insert-file-contents archive-file)
      (let ((messages nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (condition-case err
                  (push (json-read-from-string line) messages)
                (error
                 (message "Error parsing archive line: %s" err)))))
          (forward-line 1))

        (setq gemini-repl-project--current-conversation (nreverse messages))

        ;; Save as current conversation
        (let ((conv-file (gemini-repl-project--conversation-file proj-dir)))
          (with-temp-file conv-file
            (dolist (msg (reverse gemini-repl-project--current-conversation))
              (insert (json-encode msg))
              (insert "\n"))))

        (gemini-repl-project--update-metadata)

        (message "Loaded archive: %s (%d messages)"
                 name
                 (length gemini-repl-project--current-conversation))))))

;;; Statistics

;;;###autoload
(defun gemini-repl-project-stats ()
  "Display statistics for current project conversation."
  (interactive)
  (let* ((proj-dir (gemini-repl-project-dir))
         (meta (or gemini-repl-project--current-metadata
                  (gemini-repl-project--create-metadata proj-dir)))
         (msgs gemini-repl-project--current-conversation)
         (user-msgs (seq-count (lambda (m) (string= (plist-get m :role) "user"))
                              msgs))
         (assistant-msgs (seq-count (lambda (m) (string= (plist-get m :role) "assistant"))
                                   msgs))
         (archives (length (or (gemini-repl-project-list-archives) '()))))

    (message "Project: %s\nMessages: %d (user: %d, assistant: %d)\nSize: %d bytes\nArchives: %d\nCreated: %s\nUpdated: %s"
             proj-dir
             (length msgs)
             user-msgs
             assistant-msgs
             (or (plist-get meta :total_size) 0)
             archives
             (or (plist-get meta :created_at) "N/A")
             (or (plist-get meta :updated_at) "N/A"))))

;;; Integration Helpers

(defun gemini-repl-project--get-conversation ()
  "Get current project conversation.
Returns list of messages in chronological order."
  (reverse gemini-repl-project--current-conversation))

(defun gemini-repl-project--set-conversation (messages)
  "Set current project conversation to MESSAGES.
MESSAGES should be in chronological order."
  (setq gemini-repl-project--current-conversation (reverse messages))
  (when gemini-repl-project-auto-save
    (let ((conv-file (gemini-repl-project--conversation-file
                     (gemini-repl-project-dir))))
      (with-temp-file conv-file
        (dolist (msg messages)
          (insert (json-encode msg))
          (insert "\n")))
      (gemini-repl-project--update-metadata))))

;;; Export

;;;###autoload
(defun gemini-repl-project-export (filename &optional format)
  "Export current project conversation to FILENAME.
FORMAT can be 'json or 'markdown (default: 'json)."
  (interactive "FExport to file: ")
  (let ((fmt (or format 'json))
        (msgs (reverse gemini-repl-project--current-conversation))
        (proj-dir (gemini-repl-project-dir)))

    (pcase fmt
      ('json
       (with-temp-file filename
         (let ((json-encoding-pretty-print t))
           (insert (json-encode `((project . ,proj-dir)
                                 (messages . ,(vconcat msgs))
                                 (metadata . ,gemini-repl-project--current-metadata))))))
       (message "Exported to JSON: %s" filename))

      ('markdown
       (with-temp-file filename
         (insert (format "# Project: %s\n\n" proj-dir))
         (when gemini-repl-project--current-metadata
           (insert (format "Created: %s\n"
                          (plist-get gemini-repl-project--current-metadata :created_at)))
           (insert (format "Updated: %s\n"
                          (plist-get gemini-repl-project--current-metadata :updated_at)))
           (insert (format "Messages: %d\n\n"
                          (length msgs))))
         (insert "---\n\n")

         (dolist (msg msgs)
           (insert (format "## %s\n\n"
                          (capitalize (plist-get msg :role))))
           (when-let ((timestamp (plist-get msg :timestamp)))
             (insert (format "_%s_\n\n" timestamp)))
           (insert (plist-get msg :content))
           (insert "\n\n")))
       (message "Exported to Markdown: %s" filename))

      (_ (error "Unknown format: %s (use 'json or 'markdown)" fmt)))))

(provide 'gemini-repl-project)
;;; gemini-repl-project.el ends here
