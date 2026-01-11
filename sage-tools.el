;;; sage-tools.el --- Tool definitions for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tool definitions for sage.
;; Includes file operations, git integration, search, and Emacs-specific tools.

;;; Code:

(require 'json)
(require 'xref nil t)
(require 'project nil t)
(require 'projectile nil t)

;; Declare functions from sage.el
(declare-function sage--get-workspace "sage" ())
(declare-function sage--safe-path-p "sage" (path))
(declare-function sage-register-tool "sage" (name description parameters execute-fn))

;; Declare magit functions
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-status "magit" (&optional directory cache))
(declare-function magit-diff "magit-diff" (&optional rev-or-range))
(declare-function magit-log "magit-log" (&optional revs args files))

;;; Tool Registry Variables

(defvar sage-tools)
(defvar sage-workspace)
(defvar sage-yolo-mode)
(defvar sage-confirm-safe-tools)

;; Update safe tools list when this module loads
(defvar sage-safe-tools
  '("read_file" "list_files" "git_status" "git_diff" "git_log"
    "git_branch" "git_blame" "code_search" "glob_files" "search_preview"
    "describe_function" "describe_variable" "find_definition"
    "list_buffers" "project_map" "get_capabilities")
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

(defun sage--use-magit-p ()
  "Check if magit is available and should be used."
  (and (featurep 'magit) (fboundp 'magit-status)))

(defun sage--tool-git-status (_args)
  "Get git status."
  (let ((default-directory (sage-tools--get-workspace)))
    (if (sage--use-magit-p)
        (with-temp-buffer
          (magit-git-insert "status" "--porcelain")
          (buffer-string))
      (shell-command-to-string "git status --porcelain"))))

(defun sage--tool-git-diff (args)
  "Get git diff."
  (let ((default-directory (sage-tools--get-workspace))
        (staged (alist-get 'staged args))
        (path (alist-get 'path args)))
    (if (sage--use-magit-p)
        (with-temp-buffer
          (if staged
              (magit-git-insert "diff" "--staged" path)
            (magit-git-insert "diff" path))
          (buffer-string))
      (shell-command-to-string
       (format "git diff %s %s"
               (if staged "--staged" "")
               (if path (shell-quote-argument path) ""))))))

(defun sage--tool-git-log (args)
  "Get git log."
  (let ((default-directory (sage-tools--get-workspace))
        (count (or (alist-get 'count args) 10))
        (path (alist-get 'path args)))
    (if (sage--use-magit-p)
        (with-temp-buffer
          (magit-git-insert "log" "--oneline" (format "-n%d" count) path)
          (buffer-string))
      (shell-command-to-string
       (format "git log --oneline -n %d %s"
               count
               (if path (shell-quote-argument path) ""))))))

(defun sage--tool-git-branch (_args)
  "Get git branches."
  (let ((default-directory (sage-tools--get-workspace)))
    (if (sage--use-magit-p)
        (with-temp-buffer
          (magit-git-insert "branch" "-a")
          (buffer-string))
      (shell-command-to-string "git branch -a"))))

(defun sage--tool-git-blame (args)
  "Get git blame for a file."
  (let ((path (alist-get 'path args))
        (default-directory (sage-tools--get-workspace)))
    (if (sage-tools--safe-path-p path)
        (let ((full-path (expand-file-name path (sage-tools--get-workspace))))
          (if (file-exists-p full-path)
              (if (sage--use-magit-p)
                  (with-temp-buffer
                    (magit-git-insert "blame" path)
                    (buffer-string))
                (shell-command-to-string (format "git blame %s" (shell-quote-argument path))))
            (format "File not found: %s" path)))
      (format "Unsafe path: %s" path))))

;;; SEARCH TOOLS

(defun sage--tool-code-search (args)
  "Search code using ripgrep or grep."
  (let ((default-directory (sage-tools--get-workspace))
        (pattern (alist-get 'pattern args))
        (file-type (alist-get 'file_type args))
        (context (or (alist-get 'context args) 2)))
    (shell-command-to-string
     (format "rg %s -C %d %s 2>/dev/null || grep -r -C %d %s . 2>/dev/null"
             (if file-type (format "-t %s" (shell-quote-argument file-type)) "")
             context
             (shell-quote-argument pattern)
             context
             (shell-quote-argument pattern)))))

(defun sage--tool-glob-files (args)
  "Find files matching a glob pattern."
  (let ((pattern (alist-get 'pattern args)))
    (mapconcat #'identity
               (file-expand-wildcards
                (expand-file-name pattern (sage-tools--get-workspace))
                t)
               "\n")))

(defun sage--tool-search-preview (args)
  "Search with context preview."
  (let ((pattern (alist-get 'pattern args))
        (file-pattern (alist-get 'file_pattern args))
        (context-lines (or (alist-get 'context_lines args) 3))
        (default-directory (sage-tools--get-workspace)))
    (shell-command-to-string
     (format "rg -n -C %d %s %s 2>/dev/null || grep -rn -C %d %s %s . 2>/dev/null"
             context-lines
             (if file-pattern (format "-g %s" (shell-quote-argument file-pattern)) "")
             (shell-quote-argument pattern)
             context-lines
             (if file-pattern (format "--include=%s" (shell-quote-argument file-pattern)) "")
             (shell-quote-argument pattern)))))

;;; EMACS-SPECIFIC TOOLS

(defun sage--tool-eval-elisp (args)
  "Evaluate Elisp code safely."
  (let ((code (alist-get 'code args)))
    (condition-case err
        (format "%S" (eval (read code) t))
      (error (format "Eval error: %s" (error-message-string err))))))

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
        (if-let ((buf (get-buffer buffer-name)))
            (with-current-buffer buf
              (insert text)
              (format "Inserted %d chars into %s" (length text) buffer-name))
          (format "Buffer not found: %s" buffer-name))
      (insert text)
      (format "Inserted %d chars at point" (length text)))))

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
    (if-let ((proj (project-current)))
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
      (sage-tools--register name description parameters execute-fn)
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

(provide 'sage-tools)
;;; sage-tools.el ends here
