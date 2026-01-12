;;; sage-tools.el --- Tool definitions for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tool definitions for sage.
;; Includes file operations, git integration, search, and Emacs-specific tools.

;;; Code:

(require 'json)
(require 'xref nil t)
(require 'project nil t)
(require 'projectile nil t)

;; Declare functions from projectile
(declare-function projectile-project-files "projectile" (project-root))

;; Declare functions from sage.el
(declare-function sage--get-workspace "sage" ())
(declare-function sage--safe-path-p "sage" (path))
(declare-function sage-register-tool "sage" (name description parameters execute-fn))

;; Declare magit functions
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-status "magit" (&optional directory cache))
(declare-function magit-diff "magit-diff" (&optional rev-or-range))
(declare-function magit-log "magit-log" (&optional revs args files))

;; Declare org-mode functions
(declare-function org-mode "org" ())
(declare-function org-map-entries "org" (func &optional match scope &rest skip))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-get-todo-state "org" ())
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-todo "org" (&optional arg))
(declare-function org-archive-subtree "org-archive" (&optional find-done))

;;; Tool Registry Variables

(defvar sage-tools nil
  "List of registered tools.
Each tool is an alist with keys: name, description, parameters, execute.")
(defvar sage-workspace)
(defvar sage-yolo-mode)
(defvar sage-confirm-safe-tools)
(defvar sage-enable-dangerous-tools
  (or (getenv "SAGE_ENABLE_DANGEROUS_TOOLS")
      (getenv "SAGE_YOLO"))
  "When non-nil, enable dangerous tools like eval_elisp.
These tools can execute arbitrary code and should only be enabled
in trusted environments.

Can be set via environment variables:
  SAGE_ENABLE_DANGEROUS_TOOLS=1
  SAGE_YOLO=1

Or via .dir-locals.el for per-project settings:
  ((nil . ((sage-enable-dangerous-tools . t))))")

;; Update safe tools list when this module loads
(defvar sage-safe-tools
  '("read_file" "list_files" "git_status" "git_diff" "git_log"
    "git_branch" "git_blame" "code_search" "glob_files" "search_preview"
    "describe_function" "describe_variable" "find_definition"
    "list_buffers" "project_map" "get_capabilities"
    "org_todo_list" "web_fetch" "web_search")
  "Tools that are safe (read-only) and don't require confirmation.")

;;; Helper Functions
;; Provide fallback implementations if sage.el not loaded

(defun sage-tools--get-workspace ()
  "Get current workspace directory."
  (if (fboundp 'sage--get-workspace)
      (sage--get-workspace)
    (or (bound-and-true-p sage-workspace) default-directory)))

(defun sage-tools--safe-path-p (path)
  "Check if PATH is safe (within workspace, no traversal)."
  (if (fboundp 'sage--safe-path-p)
      (sage--safe-path-p path)
    (let* ((workspace (sage-tools--get-workspace))
           (expanded (expand-file-name path workspace))
           ;; Normalize both paths by removing trailing slashes for comparison
           (norm-workspace (directory-file-name (expand-file-name workspace)))
           (norm-expanded (directory-file-name expanded)))
      (and (not (string-match-p "\\.\\." path))
           (or (string= norm-workspace norm-expanded)  ; "." case
               (string-prefix-p (file-name-as-directory norm-workspace) norm-expanded))
           (not (string-match-p "\\(\\.env\\|\\.git/\\|\\.ssh\\|\\.gnupg\\)" path))))))

;;; FILE TOOLS

(defun sage--tool-read-file (args)
  "Read contents of a file."
  (let ((path (alist-get 'path args)))
    (if (sage-tools--safe-path-p path)
        (let ((full-path (expand-file-name path (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-temp-buffer
                (insert-file-contents full-path)
                (buffer-string))
            (format "File not found: %s" path)))
      (format "Unsafe path: %s" path))))

(defun sage--tool-write-file (args)
  "Write content to a file."
  (let ((path (alist-get 'path args))
        (content (alist-get 'content args)))
    (if (sage-tools--safe-path-p path)
        (let ((full-path (expand-file-name path (sage-tools--get-workspace))))
          (with-temp-file full-path
            (insert content))
          (format "Wrote %d bytes to %s" (length content) path))
      (format "Unsafe path: %s" path))))

(defun sage--tool-list-files (args)
  "List files in a directory.
PATTERN uses glob syntax (e.g., *.el, *.org) not regex."
  (let ((path (or (alist-get 'path args) "."))
        (pattern (or (alist-get 'pattern args) "*")))
    (if (sage-tools--safe-path-p path)
        (let* ((full-path (expand-file-name path (sage-tools--get-workspace)))
               (glob-pattern (expand-file-name pattern full-path)))
          (if (file-directory-p full-path)
              (let ((files (file-expand-wildcards glob-pattern)))
                (if files
                    (mapconcat (lambda (f) (file-name-nondirectory f))
                               files
                               "\n")
                  ""))
            (format "Not a directory: %s" path)))
      (format "Unsafe path: %s" path))))

(defun sage--tool-edit-file (args)
  "Edit file using diff-mode patterns.
Applies unified diff patches to files."
  (let ((path (alist-get 'path args))
        (old-text (alist-get 'old_text args))
        (new-text (alist-get 'new_text args)))
    (if (sage-tools--safe-path-p path)
        (let ((full-path (expand-file-name path (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-temp-buffer
                (insert-file-contents full-path)
                (goto-char (point-min))
                (if (search-forward old-text nil t)
                    (progn
                      (replace-match new-text)
                      (write-region (point-min) (point-max) full-path)
                      (format "Edited %s: replaced text" path))
                  (format "Old text not found in %s" path)))
            (format "File not found: %s" path)))
      (format "Unsafe path: %s" path))))

;;; GIT TOOLS
;;
;; All git tools require magit. No shell fallback.
;; Install magit: M-x package-install RET magit RET

(defconst sage-git-requires-magit-error
  "Git tools require magit. Install with: M-x package-install RET magit RET"
  "Error message when magit is not available.")

(defun sage--require-magit ()
  "Ensure magit is available, error otherwise."
  (unless (and (require 'magit nil t)
               (fboundp 'magit-git-insert))
    (error sage-git-requires-magit-error)))

(defun sage--tool-git-status (_args)
  "Get git status using magit."
  (sage--require-magit)
  (let ((default-directory (sage-tools--get-workspace)))
    (with-temp-buffer
      (magit-git-insert "status" "--porcelain")
      (buffer-string))))

(defun sage--tool-git-diff (args)
  "Get git diff using magit."
  (sage--require-magit)
  (let ((default-directory (sage-tools--get-workspace))
        (staged (alist-get 'staged args))
        (path (alist-get 'path args)))
    (with-temp-buffer
      (if staged
          (magit-git-insert "diff" "--staged" path)
        (magit-git-insert "diff" path))
      (buffer-string))))

(defun sage--tool-git-log (args)
  "Get git log using magit."
  (sage--require-magit)
  (let ((default-directory (sage-tools--get-workspace))
        (count (or (alist-get 'count args) 10))
        (path (alist-get 'path args)))
    (with-temp-buffer
      (magit-git-insert "log" "--oneline" (format "-n%d" count) path)
      (buffer-string))))

(defun sage--tool-git-branch (_args)
  "Get git branches using magit."
  (sage--require-magit)
  (let ((default-directory (sage-tools--get-workspace)))
    (with-temp-buffer
      (magit-git-insert "branch" "-a")
      (buffer-string))))

(defun sage--tool-git-blame (args)
  "Get git blame for a file using magit."
  (sage--require-magit)
  (let ((path (alist-get 'path args))
        (default-directory (sage-tools--get-workspace)))
    (if (sage-tools--safe-path-p path)
        (let ((full-path (expand-file-name path (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-temp-buffer
                (magit-git-insert "blame" path)
                (buffer-string))
            (format "File not found: %s" path)))
      (format "Unsafe path: %s" path))))

;;; SEARCH TOOLS

(defun sage--tool-code-search (args)
  "Search code using Emacs primitives.
Uses `directory-files-recursively' and `string-match' for portable search."
  (let* ((workspace (sage-tools--get-workspace))
         (pattern (alist-get 'pattern args))
         (file-ext (alist-get 'file_type args))  ; e.g., "el", "py", "clj"
         ;; Note: context lines not yet implemented in pure elisp version
         ;; Convert file type to regex (e.g., "el" -> "\\.el$")
         (file-regexp (if file-ext
                          (format "\\.%s$" (regexp-quote file-ext))
                        nil))
         (files (directory-files-recursively
                 workspace
                 (or file-regexp ".*")
                 nil  ; don't include directories
                 (lambda (dir) ; predicate to skip hidden dirs
                   (not (string-match-p "/\\." dir)))))
         (results '()))
    ;; Search each file
    (dolist (file (seq-take files 100))  ; Limit to 100 files
      (when (and (file-readable-p file)
                 (not (file-directory-p file))
                 ;; Skip binary files
                 (not (string-match-p "\\.\\(png\\|jpg\\|gif\\|pdf\\|zip\\|tar\\|gz\\)$" file)))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (let ((line-num 0))
                (while (not (eobp))
                  (setq line-num (1+ line-num))
                  (let ((line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
                    (when (string-match-p pattern line)
                      (push (format "%s:%d:%s"
                                    (file-relative-name file workspace)
                                    line-num
                                    line)
                            results)))
                  (forward-line 1))))
          (error nil))))
    (if results
        (mapconcat #'identity (nreverse (seq-take results 50)) "\n")
      (format "No matches found for: %s" pattern))))

(defun sage--tool-glob-files (args)
  "Find files matching a glob pattern."
  (let ((pattern (alist-get 'pattern args)))
    (mapconcat #'identity
               (file-expand-wildcards
                (expand-file-name pattern (sage-tools--get-workspace))
                t)
               "\n")))

(defun sage--tool-search-preview (args)
  "Search with context preview using Emacs primitives."
  (let* ((workspace (sage-tools--get-workspace))
         (pattern (alist-get 'pattern args))
         (file-pattern (alist-get 'file_pattern args))  ; e.g., "*.el"
         (context-lines (or (alist-get 'context_lines args) 3))
         ;; Convert glob to regex (e.g., "*.el" -> "\\.el$")
         (file-regexp (when file-pattern
                        (concat (regexp-quote
                                 (replace-regexp-in-string "\\*" "" file-pattern))
                                "$")))
         (files (directory-files-recursively
                 workspace
                 (or file-regexp ".*")
                 nil
                 (lambda (dir) (not (string-match-p "/\\." dir)))))
         (results '()))
    (dolist (file (seq-take files 50))
      (when (and (file-readable-p file)
                 (not (string-match-p "\\.\\(png\\|jpg\\|gif\\|pdf\\|zip\\|tar\\|gz\\)$" file)))
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (let ((lines (split-string (buffer-string) "\n"))
                    (rel-file (file-relative-name file workspace)))
                (cl-loop for line in lines
                         for line-num from 1
                         when (string-match-p pattern line)
                         do (let ((start (max 0 (- line-num context-lines 1)))
                                  (end (min (length lines) (+ line-num context-lines))))
                              (push (format "--\n%s:"
                                            rel-file)
                                    results)
                              (cl-loop for i from start below end
                                       do (push (format "%d%s%s"
                                                        (1+ i)
                                                        (if (= (1+ i) line-num) ":" "-")
                                                        (nth i lines))
                                                results))))))
          (error nil))))
    (if results
        (mapconcat #'identity (nreverse (seq-take results 100)) "\n")
      (format "No matches found for: %s" pattern))))

;;; EMACS-SPECIFIC TOOLS

(defun sage--tool-eval-elisp (args)
  "Evaluate Elisp code.
WARNING: This tool executes arbitrary code. Only available when
`sage-enable-dangerous-tools' is non-nil."
  (if (bound-and-true-p sage-enable-dangerous-tools)
      (let ((code (alist-get 'code args)))
        (condition-case err
            (format "%S" (eval (read code) t))
          (error (format "Eval error: %s" (error-message-string err)))))
    "ERROR: eval_elisp is disabled. Set sage-enable-dangerous-tools to t to enable."))

(defun sage--tool-describe-function (args)
  "Get function documentation."
  (let ((function (intern (alist-get 'function args))))
    (if (fboundp function)
        (with-temp-buffer
          (describe-function function)
          (with-current-buffer "*Help*"
            (buffer-string)))
      (format "Function not found: %s" function))))

(defun sage--tool-describe-variable (args)
  "Get variable documentation."
  (let ((variable (intern (alist-get 'variable args))))
    (if (boundp variable)
        (with-temp-buffer
          (describe-variable variable)
          (with-current-buffer "*Help*"
            (buffer-string)))
      (format "Variable not found: %s" variable))))

(defun sage--tool-find-definition (args)
  "Use xref to find definitions."
  (let ((symbol (alist-get 'symbol args)))
    (if (require 'xref nil t)
        (condition-case err
            (let ((xrefs (xref-backend-definitions
                          (xref-find-backend)
                          symbol)))
              (if xrefs
                  (mapconcat
                   (lambda (xref)
                     (let ((loc (xref-item-location xref)))
                       (format "%s: %s"
                               (xref-location-group loc)
                               (xref-item-summary xref))))
                   xrefs "\n")
                (format "No definition found for: %s" symbol)))
          (error (format "Xref error: %s" (error-message-string err))))
      "xref not available")))

(defun sage--tool-list-buffers (_args)
  "List open buffers."
  (mapconcat
   (lambda (buf)
     (let ((name (buffer-name buf))
           (file (buffer-file-name buf))
           (mode (buffer-local-value 'major-mode buf)))
       (format "%s [%s]%s"
               name
               mode
               (if file (format " - %s" file) ""))))
   (buffer-list)
   "\n"))

(defun sage--tool-switch-buffer (args)
  "Switch to buffer."
  (let ((buffer-name (alist-get 'buffer_name args)))
    (if (get-buffer buffer-name)
        (progn
          (switch-to-buffer buffer-name)
          (format "Switched to buffer: %s" buffer-name))
      (format "Buffer not found: %s" buffer-name))))

(defun sage--tool-insert-at-point (args)
  "Insert text at point in current buffer."
  (let ((text (alist-get 'text args))
        (buffer-name (alist-get 'buffer_name args)))
    (if buffer-name
        (if-let* ((buf (get-buffer buffer-name)))
            (with-current-buffer buf
              (insert text)
              (format "Inserted %d chars into %s" (length text) buffer-name))
          (format "Buffer not found: %s" buffer-name))
      (insert text)
      (format "Inserted %d chars at point" (length text)))))

;;; ORG-MODE TOOLS
;;
;; All org tools require org-mode. No fallback.
;; org-mode is built into Emacs but may need explicit loading.

(defconst sage-org-requires-org-error
  "Org tools require org-mode. Ensure org is loaded: (require 'org)"
  "Error message when org-mode is not available.")

(defun sage--require-org ()
  "Ensure org-mode is available, error otherwise."
  (unless (and (require 'org nil t)
               (fboundp 'org-mode))
    (error sage-org-requires-org-error)))

(defun sage--tool-org-todo-list (args)
  "List todos from an org file or agenda files.
Requires org-mode."
  (sage--require-org)
  (let ((file (alist-get 'file args)))
    (if file
        ;; List todos from specific file
        (let ((full-path (expand-file-name file (sage-tools--get-workspace))))
          (if (and (file-exists-p full-path)
                   (string-match-p "\\.org$" full-path))
              (with-temp-buffer
                (insert-file-contents full-path)
                (org-mode)
                (let ((todos '()))
                  (org-map-entries
                   (lambda ()
                     (let ((heading (org-get-heading t t t t))
                           (state (org-get-todo-state))
                           (priority (when (org-entry-get nil "PRIORITY")
                                       (string-to-char (org-entry-get nil "PRIORITY"))))
                           (tags (org-get-tags)))
                       (when state
                         (push (format "[%s] %s%s%s"
                                       state
                                       (if priority
                                           (format "[#%c] " priority)
                                         "")
                                       heading
                                       (if tags (format " :%s:" (string-join tags ":")) ""))
                               todos)))))
                  (if todos
                      (mapconcat #'identity (nreverse todos) "\n")
                    "No TODOs found")))
            (format "File not found or not .org: %s" file)))
      ;; List from agenda files
      (if (and (boundp 'org-agenda-files) org-agenda-files)
          (let ((results '()))
            (dolist (afile org-agenda-files)
              (when (file-exists-p afile)
                (with-temp-buffer
                  (insert-file-contents afile)
                  (org-mode)
                  (org-map-entries
                   (lambda ()
                     (let ((state (org-get-todo-state)))
                       (when state
                         (push (format "%s: [%s] %s"
                                       (file-name-nondirectory afile)
                                       state
                                       (org-get-heading t t t t))
                               results))))))))
            (if results
                (mapconcat #'identity (nreverse results) "\n")
              "No TODOs found"))
        "No org-agenda-files configured"))))

(defun sage--tool-org-add-todo (args)
  "Add a TODO item to an org file.
Requires org-mode."
  (sage--require-org)
  (let ((file (alist-get 'file args))
        (heading (alist-get 'heading args))
        (state (or (alist-get 'state args) "TODO"))
        (priority (alist-get 'priority args)))
    (if (sage-tools--safe-path-p file)
        (let ((full-path (expand-file-name file (sage-tools--get-workspace))))
          (with-current-buffer (find-file-noselect full-path)
            (org-mode)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* %s%s %s\n"
                            state
                            (if priority (format " [#%s]" priority) "")
                            heading))
            (save-buffer)
            (format "Added TODO: %s %s in %s" state heading file)))
      (format "Unsafe path: %s" file))))

(defun sage--tool-org-set-todo-state (args)
  "Change the TODO state of an item.
Requires org-mode."
  (sage--require-org)
  (let ((file (alist-get 'file args))
        (heading (alist-get 'heading args))
        (new-state (alist-get 'state args)))
    (if (sage-tools--safe-path-p file)
        (let ((full-path (expand-file-name file (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-current-buffer (find-file-noselect full-path)
                (org-mode)
                (goto-char (point-min))
                (if (re-search-forward (format "^\\*+ \\(TODO\\|DONE\\|IN-PROGRESS\\|WAITING\\|CANCELLED\\) %s"
                                               (regexp-quote heading))
                                       nil t)
                    (progn
                      (org-todo new-state)
                      (save-buffer)
                      (format "Changed state to %s: %s" new-state heading))
                  (format "Heading not found: %s" heading)))
            (format "File not found: %s" file)))
      (format "Unsafe path: %s" file))))

(defun sage--tool-org-close-todo (args)
  "Mark a TODO item as DONE.
Requires org-mode."
  (sage--require-org)
  (let ((file (alist-get 'file args))
        (heading (alist-get 'heading args)))
    (if (sage-tools--safe-path-p file)
        (let ((full-path (expand-file-name file (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-current-buffer (find-file-noselect full-path)
                (org-mode)
                (goto-char (point-min))
                (if (re-search-forward (format "^\\*+ \\(TODO\\|IN-PROGRESS\\|WAITING\\) %s"
                                               (regexp-quote heading))
                                       nil t)
                    (progn
                      (org-todo "DONE")
                      (save-buffer)
                      (format "Closed TODO: %s" heading))
                  (format "Open TODO not found: %s" heading)))
            (format "File not found: %s" file)))
      (format "Unsafe path: %s" file))))

(defun sage--tool-org-archive-todo (args)
  "Archive a DONE item to archive file.
Requires org-mode."
  (sage--require-org)
  (let ((file (alist-get 'file args))
        (heading (alist-get 'heading args)))
    (if (sage-tools--safe-path-p file)
        (let ((full-path (expand-file-name file (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (with-current-buffer (find-file-noselect full-path)
                (org-mode)
                (goto-char (point-min))
                (if (re-search-forward (format "^\\*+ DONE %s"
                                               (regexp-quote heading))
                                       nil t)
                    (progn
                      (org-archive-subtree)
                      (save-buffer)
                      (format "Archived: %s" heading))
                  (format "DONE item not found: %s" heading)))
            (format "File not found: %s" file)))
      (format "Unsafe path: %s" file))))

;;; WEB TOOLS

(defun sage--tool-web-fetch (args)
  "Fetch content from a URL using Emacs url-retrieve.
Returns the page content as text."
  (require 'url nil t)
  (let ((url (alist-get 'url args))
        (timeout (or (alist-get 'timeout args) 30)))
    (if (and url (string-match-p "^https?://" url))
        (condition-case err
            (let ((buffer (url-retrieve-synchronously url t nil timeout)))
              (if buffer
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    ;; Skip HTTP headers
                    (when (re-search-forward "\n\n" nil t)
                      (let ((content (buffer-substring-no-properties
                                      (point) (point-max))))
                        (kill-buffer buffer)
                        ;; Basic HTML to text conversion
                        (with-temp-buffer
                          (insert content)
                          ;; Remove scripts and style
                          (goto-char (point-min))
                          (while (re-search-forward
                                  "<\\(script\\|style\\)[^>]*>.*?</\\1>" nil t)
                            (replace-match ""))
                          ;; Remove HTML tags
                          (goto-char (point-min))
                          (while (re-search-forward "<[^>]+>" nil t)
                            (replace-match " "))
                          ;; Clean up whitespace
                          (goto-char (point-min))
                          (while (re-search-forward "[ \t]+" nil t)
                            (replace-match " "))
                          (goto-char (point-min))
                          (while (re-search-forward "\n\\s-*\n+" nil t)
                            (replace-match "\n\n"))
                          ;; Decode HTML entities
                          (goto-char (point-min))
                          (while (re-search-forward "&amp;" nil t)
                            (replace-match "&"))
                          (goto-char (point-min))
                          (while (re-search-forward "&lt;" nil t)
                            (replace-match "<"))
                          (goto-char (point-min))
                          (while (re-search-forward "&gt;" nil t)
                            (replace-match ">"))
                          (goto-char (point-min))
                          (while (re-search-forward "&quot;" nil t)
                            (replace-match "\""))
                          (goto-char (point-min))
                          (while (re-search-forward "&nbsp;" nil t)
                            (replace-match " "))
                          (string-trim (buffer-string))))))
                "Failed to fetch URL"))
          (error (format "Error fetching %s: %s" url (error-message-string err))))
      (format "Invalid URL: %s (must be http:// or https://)" url))))

(defun sage--tool-web-search (args)
  "Search the web using DuckDuckGo HTML interface.
Returns search results as text."
  (require 'url nil t)
  (let* ((query (alist-get 'query args))
         (max-results (or (alist-get 'max_results args) 5))
         (encoded-query (url-hexify-string query))
         (url (format "https://html.duckduckgo.com/html/?q=%s" encoded-query)))
    (if query
        (condition-case err
            (let ((buffer (url-retrieve-synchronously url t nil 30)))
              (if buffer
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (when (re-search-forward "\n\n" nil t)
                      (let ((results '())
                            (count 0))
                        ;; Parse DuckDuckGo results
                        (while (and (< count max-results)
                                    (re-search-forward
                                     "class=\"result__a\"[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)"
                                     nil t))
                          (let ((link (match-string 1))
                                (title (match-string 2)))
                            (push (format "%d. %s\n   %s"
                                          (1+ count)
                                          (string-trim title)
                                          link)
                                  results)
                            (setq count (1+ count))))
                        (kill-buffer buffer)
                        (if results
                            (mapconcat #'identity (nreverse results) "\n\n")
                          "No results found"))))
                "Search failed"))
          (error (format "Search error: %s" (error-message-string err))))
      "No search query provided")))

;;; SELF-AWARENESS TOOLS

(defun sage--tool-project-map (_args)
  "Get project structure using project.el or projectile."
  (cond
   ((and (require 'projectile nil t) (fboundp 'projectile-project-root))
    (let ((root (projectile-project-root)))
      (if root
          (format "Project: %s\nFiles:\n%s"
                  root
                  (mapconcat #'identity
                             (projectile-project-files root)
                             "\n"))
        "Not in a project")))
   ((and (require 'project nil t) (fboundp 'project-current))
    (if-let* ((proj (project-current)))
        (let ((root (project-root proj)))
          (format "Project: %s\nFiles:\n%s"
                  root
                  (mapconcat
                   #'identity
                   (project-files proj)
                   "\n")))
      "Not in a project"))
   (t "No project system available")))

(defun sage--tool-get-capabilities (_args)
  "List available tools and capabilities."
  (let ((tools (mapcar (lambda (tool)
                         (format "- %s: %s"
                                 (alist-get 'name tool)
                                 (alist-get 'description tool)))
                       sage-tools)))
    (format "Available tools (%d):\n%s\n\nEmacs version: %s\nFeatures: magit=%s xref=%s project=%s projectile=%s"
            (length sage-tools)
            (mapconcat #'identity tools "\n")
            emacs-version
            (featurep 'magit)
            (featurep 'xref)
            (featurep 'project)
            (featurep 'projectile))))

;;; TOOL INITIALIZATION

;; Provide fallback implementation of register function if not available
(defun sage-tools--register (name description parameters execute-fn)
  "Register a tool (fallback implementation).
NAME is the tool name.
DESCRIPTION explains what it does.
PARAMETERS is a JSON schema for arguments.
EXECUTE-FN is called with arguments and returns result."
  (if (fboundp 'sage-register-tool)
      (sage-register-tool name description parameters execute-fn)
    ;; Fallback: register directly
    (unless (boundp 'sage-tools)
      (setq sage-tools nil))
    (push `((name . ,name)
            (description . ,description)
            (parameters . ,parameters)
            (execute . ,execute-fn))
          sage-tools)))

;;;###autoload
(defun sage--init-default-tools ()
  "Initialize all default tools."
  (when (boundp 'sage-tools)
    (setq sage-tools nil)) ; Clear existing tools

  ;; FILE TOOLS
  (sage-tools--register
   "read_file"
   "Read contents of a file"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))))
     (required . ["path"]))
   #'sage--tool-read-file)

  (sage-tools--register
   "write_file"
   "Write content to a file"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))
                    (content . ((type . "string")
                               (description . "Content to write")))))
     (required . ["path" "content"]))
   #'sage--tool-write-file)

  (sage-tools--register
   "list_files"
   "List files in a directory"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "Directory path relative to workspace")))
                    (pattern . ((type . "string")
                               (description . "Optional glob pattern")))))
     (required . ["path"]))
   #'sage--tool-list-files)

  (sage-tools--register
   "edit_file"
   "Edit file by replacing old text with new text"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))
                    (old_text . ((type . "string")
                                (description . "Text to find and replace")))
                    (new_text . ((type . "string")
                               (description . "Replacement text")))))
     (required . ["path" "old_text" "new_text"]))
   #'sage--tool-edit-file)

  ;; GIT TOOLS
  (sage-tools--register
   "git_status"
   "Get git status (uses magit if available)"
   '((type . "object")
     (properties . ())
     (required . []))
   #'sage--tool-git-status)

  (sage-tools--register
   "git_diff"
   "Get git diff (uses magit if available)"
   '((type . "object")
     (properties . ((staged . ((type . "boolean")
                              (description . "Show staged changes only")))
                    (path . ((type . "string")
                            (description . "Optional path to diff")))))
     (required . []))
   #'sage--tool-git-diff)

  (sage-tools--register
   "git_log"
   "Get git log (uses magit if available)"
   '((type . "object")
     (properties . ((count . ((type . "integer")
                             (description . "Number of commits to show")))
                    (path . ((type . "string")
                            (description . "Optional path to filter log")))))
     (required . []))
   #'sage--tool-git-log)

  (sage-tools--register
   "git_branch"
   "List git branches (uses magit if available)"
   '((type . "object")
     (properties . ())
     (required . []))
   #'sage--tool-git-branch)

  (sage-tools--register
   "git_blame"
   "Get git blame for a file (uses magit if available)"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path to blame")))))
     (required . ["path"]))
   #'sage--tool-git-blame)

  ;; SEARCH TOOLS
  (sage-tools--register
   "code_search"
   "Search code using ripgrep or grep"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Search pattern (regex)")))
                    (file_type . ((type . "string")
                                 (description . "File type filter (e.g., 'rs', 'py')")))
                    (context . ((type . "integer")
                               (description . "Context lines to show (default 2)")))))
     (required . ["pattern"]))
   #'sage--tool-code-search)

  (sage-tools--register
   "glob_files"
   "Find files matching a glob pattern"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Glob pattern (e.g., '**/*.el')")))))
     (required . ["pattern"]))
   #'sage--tool-glob-files)

  (sage-tools--register
   "search_preview"
   "Search with context preview (line numbers and context)"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Search pattern (regex)")))
                    (file_pattern . ((type . "string")
                                    (description . "File pattern to search in")))
                    (context_lines . ((type . "integer")
                                     (description . "Context lines to show (default 3)")))))
     (required . ["pattern"]))
   #'sage--tool-search-preview)

  ;; EMACS-SPECIFIC TOOLS
  (sage-tools--register
   "eval_elisp"
   "Evaluate Elisp code safely"
   '((type . "object")
     (properties . ((code . ((type . "string")
                             (description . "Elisp code to evaluate")))))
     (required . ["code"]))
   #'sage--tool-eval-elisp)

  (sage-tools--register
   "describe_function"
   "Get function documentation"
   '((type . "object")
     (properties . ((function . ((type . "string")
                                (description . "Function name to describe")))))
     (required . ["function"]))
   #'sage--tool-describe-function)

  (sage-tools--register
   "describe_variable"
   "Get variable documentation"
   '((type . "object")
     (properties . ((variable . ((type . "string")
                                (description . "Variable name to describe")))))
     (required . ["variable"]))
   #'sage--tool-describe-variable)

  (sage-tools--register
   "find_definition"
   "Use xref to find symbol definitions"
   '((type . "object")
     (properties . ((symbol . ((type . "string")
                              (description . "Symbol to find definition for")))))
     (required . ["symbol"]))
   #'sage--tool-find-definition)

  (sage-tools--register
   "list_buffers"
   "List open buffers with their modes and files"
   '((type . "object")
     (properties . ())
     (required . []))
   #'sage--tool-list-buffers)

  (sage-tools--register
   "switch_buffer"
   "Switch to a buffer by name"
   '((type . "object")
     (properties . ((buffer_name . ((type . "string")
                                   (description . "Buffer name to switch to")))))
     (required . ["buffer_name"]))
   #'sage--tool-switch-buffer)

  (sage-tools--register
   "insert_at_point"
   "Insert text at point in current buffer or specified buffer"
   '((type . "object")
     (properties . ((text . ((type . "string")
                            (description . "Text to insert")))
                    (buffer_name . ((type . "string")
                                   (description . "Optional buffer name (current if not specified)")))))
     (required . ["text"]))
   #'sage--tool-insert-at-point)

  ;; ORG-MODE TOOLS
  (sage-tools--register
   "org_todo_list"
   "List TODO items from an org file or agenda files"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "Org file path (optional, uses agenda if not specified)")))))
     (required . []))
   #'sage--tool-org-todo-list)

  (sage-tools--register
   "org_add_todo"
   "Add a TODO item to an org file"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "Org file path")))
                    (heading . ((type . "string")
                               (description . "TODO heading text")))
                    (state . ((type . "string")
                             (description . "TODO state (default: TODO)")))
                    (priority . ((type . "string")
                                (description . "Priority A, B, or C (optional)")))))
     (required . ["file" "heading"]))
   #'sage--tool-org-add-todo)

  (sage-tools--register
   "org_set_todo_state"
   "Change the state of a TODO item"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "Org file path")))
                    (heading . ((type . "string")
                               (description . "TODO heading to find")))
                    (state . ((type . "string")
                             (description . "New TODO state")))))
     (required . ["file" "heading" "state"]))
   #'sage--tool-org-set-todo-state)

  (sage-tools--register
   "org_close_todo"
   "Mark a TODO item as DONE"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "Org file path")))
                    (heading . ((type . "string")
                               (description . "TODO heading to close")))))
     (required . ["file" "heading"]))
   #'sage--tool-org-close-todo)

  (sage-tools--register
   "org_archive_todo"
   "Archive a DONE item to archive file"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "Org file path")))
                    (heading . ((type . "string")
                               (description . "DONE heading to archive")))))
     (required . ["file" "heading"]))
   #'sage--tool-org-archive-todo)

  ;; WEB TOOLS
  (sage-tools--register
   "web_fetch"
   "Fetch content from a URL and return as text"
   '((type . "object")
     (properties . ((url . ((type . "string")
                            (description . "URL to fetch (http or https)")))
                    (timeout . ((type . "integer")
                               (description . "Timeout in seconds (default 30)")))))
     (required . ["url"]))
   #'sage--tool-web-fetch)

  (sage-tools--register
   "web_search"
   "Search the web using DuckDuckGo"
   '((type . "object")
     (properties . ((query . ((type . "string")
                              (description . "Search query")))
                    (max_results . ((type . "integer")
                                   (description . "Max results to return (default 5)")))))
     (required . ["query"]))
   #'sage--tool-web-search)

  ;; SELF-AWARENESS TOOLS
  (sage-tools--register
   "project_map"
   "Get project structure using project.el or projectile"
   '((type . "object")
     (properties . ())
     (required . []))
   #'sage--tool-project-map)

  (sage-tools--register
   "get_capabilities"
   "List available tools and system capabilities"
   '((type . "object")
     (properties . ())
     (required . []))
   #'sage--tool-get-capabilities)

  (message "Initialized %d sage tools" (length sage-tools)))

;; Auto-initialize tools when module is loaded
;; This ensures tools are available in batch mode without explicit init
(sage--init-default-tools)

(provide 'sage-tools)
;;; sage-tools.el ends here
