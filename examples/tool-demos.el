;;; tool-demos.el --- Demonstrations of sage-tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Interactive demos for all sage-tools.
;; Run with: emacs -Q -L . -l examples/tool-demos.el -f sage-tools-demo-all
;; Or interactively: M-x sage-tools-demo-all

;;; Code:

(require 'sage-tools)

;; Initialize tools
(sage--init-default-tools)

(defvar sage-tools-demo-buffer "*Sage Tools Demo*"
  "Buffer for demo output.")

(defun sage-tools-demo--output (title result)
  "Output TITLE and RESULT to demo buffer."
  (with-current-buffer (get-buffer-create sage-tools-demo-buffer)
    (goto-char (point-max))
    (insert (format "\n=== %s ===\n" title))
    (insert (format "%s\n" result))
    (insert "\n")))

(defun sage-tools-demo--run (name args)
  "Run tool NAME with ARGS and display result."
  (let* ((tool (cl-find name sage-tools
                        :key (lambda (t) (alist-get 'name t))
                        :test #'string=))
         (fn (alist-get 'execute tool)))
    (if fn
        (condition-case err
            (let ((result (funcall fn args)))
              (sage-tools-demo--output name result)
              result)
          (error
           (sage-tools-demo--output name (format "ERROR: %s" (error-message-string err)))
           nil))
      (sage-tools-demo--output name (format "Tool not found: %s" name))
      nil)))

;;; FILE TOOL DEMOS

(defun sage-tools-demo-file-tools ()
  "Demo file tools."
  (interactive)
  (let ((test-file "examples/demo-test.txt"))
    ;; write_file
    (sage-tools-demo--run "write_file"
                          `((path . ,test-file)
                            (content . "Hello from sage-tools!\nLine 2\nLine 3")))
    ;; read_file
    (sage-tools-demo--run "read_file" `((path . ,test-file)))

    ;; list_files
    (sage-tools-demo--run "list_files" '((path . "examples") (pattern . "*.el")))

    ;; edit_file
    (sage-tools-demo--run "edit_file"
                          `((path . ,test-file)
                            (old_text . "Line 2")
                            (new_text . "Modified Line 2")))

    ;; Verify edit
    (sage-tools-demo--run "read_file" `((path . ,test-file)))

    ;; Cleanup
    (delete-file (expand-file-name test-file (sage-tools--get-workspace)))))

;;; SEARCH TOOL DEMOS

(defun sage-tools-demo-search-tools ()
  "Demo search tools."
  (interactive)
  ;; code_search - find defun in elisp files
  (sage-tools-demo--run "code_search"
                        '((pattern . "defun sage--tool")
                          (file_type . "el")))

  ;; glob_files
  (sage-tools-demo--run "glob_files" '((pattern . "*.el")))

  ;; search_preview
  (sage-tools-demo--run "search_preview"
                        '((pattern . "require")
                          (file_pattern . "*.el")
                          (context_lines . 2))))

;;; GIT TOOL DEMOS (requires magit)

(defun sage-tools-demo-git-tools ()
  "Demo git tools (requires magit)."
  (interactive)
  (if (require 'magit nil t)
      (progn
        (sage-tools-demo--run "git_status" nil)
        (sage-tools-demo--run "git_branch" nil)
        (sage-tools-demo--run "git_log" '((count . 5)))
        (sage-tools-demo--run "git_diff" '((staged . nil))))
    (sage-tools-demo--output "git_tools" "SKIPPED: magit not available")))

;;; ORG-MODE TOOL DEMOS

(defun sage-tools-demo-org-tools ()
  "Demo org-mode tools."
  (interactive)
  (let ((test-org "examples/demo-todos.org"))
    ;; Create test org file
    (with-temp-file (expand-file-name test-org (sage-tools--get-workspace))
      (insert "#+TITLE: Demo TODOs\n\n")
      (insert "* TODO First task\n")
      (insert "* TODO [#A] High priority task\n")
      (insert "* IN-PROGRESS Working on this\n")
      (insert "* DONE Completed task\n"))

    ;; org_todo_list
    (sage-tools-demo--run "org_todo_list" `((file . ,test-org)))

    ;; org_add_todo
    (sage-tools-demo--run "org_add_todo"
                          `((file . ,test-org)
                            (heading . "New task from demo")
                            (state . "TODO")
                            (priority . "B")))

    ;; org_set_todo_state
    (sage-tools-demo--run "org_set_todo_state"
                          `((file . ,test-org)
                            (heading . "First task")
                            (state . "IN-PROGRESS")))

    ;; org_close_todo
    (sage-tools-demo--run "org_close_todo"
                          `((file . ,test-org)
                            (heading . "Working on this")))

    ;; Show final state
    (sage-tools-demo--run "org_todo_list" `((file . ,test-org)))

    ;; Cleanup
    (delete-file (expand-file-name test-org (sage-tools--get-workspace)))))

;;; WEB TOOL DEMOS

(defun sage-tools-demo-web-tools ()
  "Demo web tools."
  (interactive)
  ;; web_fetch
  (sage-tools-demo--run "web_fetch"
                        '((url . "https://httpbin.org/html")
                          (timeout . 10)))

  ;; web_search
  (sage-tools-demo--run "web_search"
                        '((query . "emacs lisp tutorial")
                          (max_results . 3))))

;;; EMACS-SPECIFIC TOOL DEMOS

(defun sage-tools-demo-emacs-tools ()
  "Demo Emacs-specific tools."
  (interactive)
  ;; describe_function
  (sage-tools-demo--run "describe_function" '((function . "mapcar")))

  ;; describe_variable - using version-control as example
  (sage-tools-demo--run "describe_variable" '((variable . "version-control")))

  ;; eval_elisp
  (sage-tools-demo--run "eval_elisp" '((code . "(+ 1 2 3)")))
  (sage-tools-demo--run "eval_elisp" '((code . "(emacs-version)")))

  ;; list_buffers
  (sage-tools-demo--run "list_buffers" nil)

  ;; find_definition (may not work in batch mode)
  (sage-tools-demo--run "find_definition" '((symbol . "defun")))

  ;; project_map
  (sage-tools-demo--run "project_map" nil)

  ;; get_capabilities
  (sage-tools-demo--run "get_capabilities" nil))

;;; MAIN DEMO RUNNER

;;;###autoload
(defun sage-tools-demo-all ()
  "Run all tool demos."
  (interactive)
  (with-current-buffer (get-buffer-create sage-tools-demo-buffer)
    (erase-buffer)
    (insert "╔══════════════════════════════════════════╗\n")
    (insert "║       SAGE-TOOLS DEMONSTRATION           ║\n")
    (insert "║       28 Tools - Pure Emacs              ║\n")
    (insert "╚══════════════════════════════════════════╝\n")
    (insert (format "\nTimestamp: %s\n" (current-time-string)))
    (insert (format "Emacs: %s\n" emacs-version))
    (insert (format "Workspace: %s\n" (sage-tools--get-workspace)))
    (insert (format "Tools loaded: %d\n" (length sage-tools))))

  (message "Running FILE tool demos...")
  (sage-tools-demo-file-tools)

  (message "Running SEARCH tool demos...")
  (sage-tools-demo-search-tools)

  (message "Running GIT tool demos...")
  (sage-tools-demo-git-tools)

  (message "Running ORG-MODE tool demos...")
  (sage-tools-demo-org-tools)

  (message "Running EMACS tool demos...")
  (sage-tools-demo-emacs-tools)

  ;; Skip web demos in batch mode (network)
  (unless noninteractive
    (message "Running WEB tool demos...")
    (sage-tools-demo-web-tools))

  (with-current-buffer sage-tools-demo-buffer
    (goto-char (point-max))
    (insert "\n\n════════════════════════════════════════════\n")
    (insert "Demo complete!\n"))

  (display-buffer sage-tools-demo-buffer)
  (message "Demo complete! See %s" sage-tools-demo-buffer))

;;; Quick test functions

(defun sage-tools-demo-describe-variable-example ()
  "Demo describe_variable with version-control."
  (interactive)
  (sage-tools-demo--run "describe_variable" '((variable . "version-control"))))

(provide 'tool-demos)
;;; tool-demos.el ends here
