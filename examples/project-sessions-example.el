;;; project-sessions-example.el --- Example usage of gemini-repl-project -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates how to use gemini-repl-project for
;; per-directory conversation history management.

;;; Code:

(require 'gemini-repl-project)

;;; Example 1: Basic Usage

(defun example-basic-project-session ()
  "Demonstrate basic project session operations."
  (interactive)

  ;; Load conversation for current project
  ;; This happens automatically if gemini-repl-project-auto-load is t
  (gemini-repl-project-load)

  ;; Append a user message
  (gemini-repl-project-append
   '(:role "user" :content "How do I optimize this code?"))

  ;; Append an assistant response
  (gemini-repl-project-append
   '(:role "assistant" :content "Here are some optimization tips..."))

  ;; Or use the shorthand list form
  (gemini-repl-project-append '("user" "What about memory usage?"))
  (gemini-repl-project-append '("assistant" "For memory optimization..."))

  ;; View statistics
  (gemini-repl-project-stats))

;;; Example 2: Working with Multiple Projects

(defun example-multi-project-workflow ()
  "Demonstrate switching between projects."
  (interactive)

  ;; Project A
  (let ((default-directory "/home/user/project-a/"))
    (gemini-repl-project-load)
    (message "Loaded project A: %d messages"
             (length gemini-repl-project--current-conversation))

    ;; Work on project A...
    (gemini-repl-project-append '("user" "Fix authentication bug"))
    )

  ;; Switch to Project B
  (let ((default-directory "/home/user/project-b/"))
    (gemini-repl-project-load)
    (message "Loaded project B: %d messages"
             (length gemini-repl-project--current-conversation))

    ;; Work on project B...
    (gemini-repl-project-append '("user" "Add new API endpoint"))
    )

  ;; Each project has its own isolated conversation
  )

;;; Example 3: Archiving and History Management

(defun example-archiving-workflow ()
  "Demonstrate archiving conversations."
  (interactive)

  ;; Before major refactor, archive current conversation
  (gemini-repl-project-archive "before-refactor")

  ;; Start fresh for the refactor work
  (gemini-repl-project-clear)

  ;; Work on refactor...
  (gemini-repl-project-append '("user" "Help me refactor this module"))

  ;; Later, list all archives
  (let ((archives (gemini-repl-project-list-archives)))
    (message "Available archives: %s" (string-join archives ", ")))

  ;; Load an old archive
  (gemini-repl-project-load-archive "before-refactor.jsonl"))

;;; Example 4: Custom Metadata

(defun example-metadata-tracking ()
  "Demonstrate custom metadata tracking."
  (interactive)

  ;; Set custom metadata fields
  (gemini-repl-project-metadata :git_branch "feature/new-auth")
  (gemini-repl-project-metadata :task_id "PROJ-123")
  (gemini-repl-project-metadata :developer "alice")

  ;; Get metadata
  (let ((branch (gemini-repl-project-metadata :git_branch))
        (task (gemini-repl-project-metadata :task_id)))
    (message "Working on %s for task %s" branch task))

  ;; Get all metadata
  (let ((meta (gemini-repl-project-metadata)))
    (message "Project metadata: %S" meta)))

;;; Example 5: Export Workflows

(defun example-export-conversations ()
  "Demonstrate exporting conversations."
  (interactive)

  ;; Export to JSON for backup
  (gemini-repl-project-export
   (expand-file-name "conversation-backup.json" default-directory)
   'json)

  ;; Export to Markdown for documentation
  (gemini-repl-project-export
   (expand-file-name "conversation-notes.md" default-directory)
   'markdown)

  (message "Conversations exported"))

;;; Example 6: Integration with Hooks

(defun example-hook-integration ()
  "Demonstrate integration with Emacs hooks."
  (interactive)

  ;; Auto-save git context when starting a conversation
  (defun my-save-git-context ()
    "Save current git commit to metadata."
    (when-let ((rev (vc-git-working-revision default-directory)))
      (gemini-repl-project-metadata :git_commit rev)
      (gemini-repl-project-metadata :git_branch (vc-git--symbolic-ref default-directory))))

  ;; Auto-archive conversations weekly
  (defun my-weekly-archive ()
    "Archive conversation with week-based naming."
    (when (and gemini-repl-project--current-conversation
               (> (length gemini-repl-project--current-conversation) 10))
      (gemini-repl-project-archive
       (format-time-string "week-%Y-w%U"))))

  ;; Add to hooks
  (add-hook 'gemini-repl-mode-hook #'my-save-git-context)

  ;; Run weekly archive on Mondays
  (run-at-time "00:00 Mon" (* 7 24 60 60) #'my-weekly-archive))

;;; Example 7: Searching Conversations

(defun example-search-conversation (pattern)
  "Search current conversation for PATTERN."
  (interactive "sSearch pattern: ")

  (let ((msgs (gemini-repl-project--get-conversation))
        (matches nil))

    (dolist (msg msgs)
      (when (string-match-p pattern (plist-get msg :content))
        (push msg matches)))

    (if matches
        (progn
          (message "Found %d matches" (length matches))
          (with-current-buffer (get-buffer-create "*Gemini Search Results*")
            (erase-buffer)
            (insert (format "Search results for: %s\n\n" pattern))
            (dolist (msg (nreverse matches))
              (insert (format "=== %s ===\n"
                            (capitalize (plist-get msg :role))))
              (insert (plist-get msg :content))
              (insert "\n\n"))
            (goto-char (point-min))
            (special-mode)
            (pop-to-buffer (current-buffer))))
      (message "No matches found"))))

;;; Example 8: Auto-Archive Strategy

(defun example-smart-archiving ()
  "Implement smart archiving based on message count and age."
  (interactive)

  (when-let* ((meta (gemini-repl-project-metadata))
              (created (plist-get meta :created_at))
              (msg-count (plist-get meta :message_count)))

    ;; Archive if:
    ;; 1. More than 500 messages
    ;; 2. OR conversation older than 1 week
    (let ((age-days (/ (float-time
                       (time-subtract (current-time)
                                     (parse-iso8601-time-string created)))
                      86400)))

      (when (or (> msg-count 500)
                (> age-days 7))
        (message "Auto-archiving conversation (age: %.1f days, messages: %d)"
                 age-days msg-count)
        (gemini-repl-project-archive
         (format-time-string "auto-%Y%m%d-%H%M%S"))
        (gemini-repl-project-clear)))))

;;; Example 9: Per-Branch Conversations

(defun example-branch-based-sessions ()
  "Use different conversations per git branch."
  (interactive)

  ;; Get current git branch
  (when-let ((branch (vc-git--symbolic-ref default-directory)))

    ;; Store branch in metadata
    (gemini-repl-project-metadata :git_branch branch)

    ;; Archive conversation when switching branches
    (defun my-archive-on-branch-switch ()
      "Archive conversation when git branch changes."
      (let ((current-branch (vc-git--symbolic-ref default-directory))
            (stored-branch (gemini-repl-project-metadata :git_branch)))

        (when (and stored-branch
                   (not (string= current-branch stored-branch)))
          (message "Branch changed: %s -> %s, archiving..."
                   stored-branch current-branch)
          (gemini-repl-project-archive
           (format "branch-%s" stored-branch))
          (gemini-repl-project-clear)
          (gemini-repl-project-metadata :git_branch current-branch))))

    ;; Check on file save
    (add-hook 'after-save-hook #'my-archive-on-branch-switch nil t)))

;;; Example 10: Integration with org-mode

(defun example-org-integration ()
  "Export conversation to org-mode with rich formatting."
  (interactive)

  (let ((msgs (gemini-repl-project--get-conversation))
        (proj-dir (gemini-repl-project-dir))
        (meta (gemini-repl-project-metadata)))

    (with-current-buffer (get-buffer-create "*Gemini Org Export*")
      (erase-buffer)
      (org-mode)

      ;; Header
      (insert (format "#+TITLE: Conversation: %s\n" proj-dir))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
      (insert (format "#+PROPERTY: messages %d\n\n"
                    (length msgs)))

      ;; Metadata table
      (insert "* Metadata\n\n")
      (insert "| Property | Value |\n")
      (insert "|----------+-------|\n")
      (insert (format "| Project | %s |\n" proj-dir))
      (insert (format "| Created | %s |\n"
                    (plist-get meta :created_at)))
      (insert (format "| Updated | %s |\n"
                    (plist-get meta :updated_at)))
      (insert (format "| Messages | %d |\n\n"
                    (length msgs)))

      ;; Conversation
      (insert "* Conversation\n\n")
      (dolist (msg msgs)
        (let ((role (capitalize (plist-get msg :role)))
              (content (plist-get msg :content))
              (timestamp (plist-get msg :timestamp)))

          (insert (format "** %s\n" role))
          (insert (format ":PROPERTIES:\n"))
          (insert (format ":TIMESTAMP: %s\n" timestamp))
          (insert (format ":END:\n\n"))

          ;; Format content based on role
          (if (string= (plist-get msg :role) "user")
              (insert (format "#+begin_quote\n%s\n#+end_quote\n\n" content))
            (insert (format "%s\n\n" content)))))

      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Running Examples

(defun run-all-examples ()
  "Run all examples (for testing)."
  (interactive)

  (message "Running gemini-repl-project examples...")

  ;; Create test directory
  (let ((test-dir "/tmp/gemini-repl-test")
        (gemini-repl-project-directory "/tmp/gemini-repl-test-storage"))

    (make-directory test-dir t)

    (let ((default-directory test-dir))

      ;; Example 1
      (message "Example 1: Basic usage")
      (example-basic-project-session)

      ;; Example 4
      (message "Example 4: Metadata")
      (example-metadata-tracking)

      ;; Example 5
      (message "Example 5: Export")
      (example-export-conversations)

      ;; Example 7
      (message "Example 7: Search")
      (example-search-conversation "optimize")

      ;; Cleanup
      (when (file-directory-p test-dir)
        (delete-directory test-dir t))
      (when (file-directory-p gemini-repl-project-directory)
        (delete-directory gemini-repl-project-directory t))

      (message "Examples completed!"))))

(provide 'project-sessions-example)
;;; project-sessions-example.el ends here
