;;; run-100-scenarios.el --- Run 100 tool scenarios with logging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; This script runs 100 tool call scenarios to:
;; 1. Create the ~/.emacs.d/sage/projects/ storage directory
;; 2. Log all tool calls with results
;; 3. Verify each tool type is exercised
;;
;; Run with: make test-scenarios
;; Or: emacs --batch -L . -l scripts/run-100-scenarios.el

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load sage modules
(require 'sage-tools)
(require 'sage-project)

;;; Configuration

(defvar scenario-log-file
  (expand-file-name "scenario-log.jsonl" default-directory)
  "File to log all scenario results.")

(defvar scenario-results nil
  "Alist of (tool-name . (successes . failures)).")

(defvar scenario-start-time nil
  "Start time of scenario run.")

;;; Logging

(defun scenario-log (tool-name args result &optional error)
  "Log a tool call to the scenario log."
  (let ((entry `((timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
                 (tool . ,tool-name)
                 (args . ,args)
                 (success . ,(not error))
                 (result_preview . ,(if (stringp result)
                                        (substring result 0 (min 100 (length result)))
                                      (format "%S" result)))
                 ,@(when error `((error . ,error))))))
    (with-temp-buffer
      (insert (json-encode entry) "\n")
      (append-to-file (point-min) (point-max) scenario-log-file))
    ;; Update stats
    (let ((stats (or (alist-get tool-name scenario-results nil nil #'string=)
                     (cons 0 0))))
      (if error
          (setcdr stats (1+ (cdr stats)))
        (setcar stats (1+ (car stats))))
      (setf (alist-get tool-name scenario-results nil nil #'string=) stats))))

(defun scenario-run-tool (tool-name args)
  "Run a tool and log the result."
  (condition-case err
      (let* ((tool-fn (intern (concat "sage--tool-" (replace-regexp-in-string "_" "-" tool-name))))
             (result (if (fboundp tool-fn)
                         (funcall tool-fn args)
                       (format "Tool function %s not found" tool-fn))))
        (scenario-log tool-name args result)
        result)
    (error
     (scenario-log tool-name args nil (error-message-string err))
     nil)))

;;; Scenarios

(defvar scenario-temp-dir nil
  "Temporary directory for scenarios.")

(defun scenario-setup ()
  "Set up scenario environment."
  (setq scenario-temp-dir (make-temp-file "sage-scenarios-" t))
  (setq sage-workspace scenario-temp-dir)
  (setq scenario-results nil)
  (setq scenario-start-time (current-time))

  ;; Initialize project storage
  (sage-project-load)

  ;; Create test files
  (with-temp-file (expand-file-name "test.el" scenario-temp-dir)
    (insert "(defun hello-world ()\n  \"Say hello.\"\n  (message \"Hello, World!\"))\n"))
  (with-temp-file (expand-file-name "data.txt" scenario-temp-dir)
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"))
  (with-temp-file (expand-file-name "todos.org" scenario-temp-dir)
    (insert "* TODO Task 1\n* DONE Task 2\n* TODO Task 3\n"))

  ;; Create subdirectory with files
  (let ((subdir (expand-file-name "subdir" scenario-temp-dir)))
    (make-directory subdir)
    (with-temp-file (expand-file-name "nested.el" subdir)
      (insert "(defvar nested-var 42)\n")))

  (message "Scenario setup complete: %s" scenario-temp-dir))

(defun scenario-teardown ()
  "Clean up scenario environment."
  (when (and scenario-temp-dir (file-exists-p scenario-temp-dir))
    (delete-directory scenario-temp-dir t))
  (setq scenario-temp-dir nil))

;;; 100 Scenarios

(defun run-file-scenarios ()
  "Run file tool scenarios (20 scenarios)."
  (message "Running file scenarios...")

  ;; read_file scenarios (5)
  (scenario-run-tool "read_file" '((path . "test.el")))
  (scenario-run-tool "read_file" '((path . "data.txt")))
  (scenario-run-tool "read_file" '((path . "todos.org")))
  (scenario-run-tool "read_file" '((path . "nonexistent.txt")))
  (scenario-run-tool "read_file" '((path . "subdir/nested.el")))

  ;; write_file scenarios (5)
  (scenario-run-tool "write_file" '((path . "new1.txt") (content . "Content 1")))
  (scenario-run-tool "write_file" '((path . "new2.txt") (content . "Content 2\nLine 2")))
  (scenario-run-tool "write_file" '((path . "new3.el") (content . "(defun new-fn ())")))
  (scenario-run-tool "write_file" '((path . "subdir/new4.txt") (content . "Nested write")))
  (scenario-run-tool "write_file" '((path . "unicode.txt") (content . "日本語テスト")))

  ;; list_files scenarios (5)
  (scenario-run-tool "list_files" '((path . ".")))
  (scenario-run-tool "list_files" '((path . ".") (pattern . "*.el")))
  (scenario-run-tool "list_files" '((path . ".") (pattern . "*.txt")))
  (scenario-run-tool "list_files" '((path . "subdir")))
  (scenario-run-tool "list_files" '((path . ".") (pattern . "*.org")))

  ;; edit_file scenarios (5)
  (scenario-run-tool "edit_file" '((path . "data.txt") (old_text . "Line 2") (new_text . "Modified Line 2")))
  (scenario-run-tool "edit_file" '((path . "data.txt") (old_text . "Line 3") (new_text . "Changed Line 3")))
  (scenario-run-tool "edit_file" '((path . "test.el") (old_text . "hello-world") (new_text . "greet-world")))
  (scenario-run-tool "edit_file" '((path . "data.txt") (old_text . "NOTFOUND") (new_text . "NEW")))
  (scenario-run-tool "edit_file" '((path . "new1.txt") (old_text . "Content 1") (new_text . "Updated Content 1"))))

(defun run-search-scenarios ()
  "Run search tool scenarios (15 scenarios)."
  (message "Running search scenarios...")

  ;; code_search scenarios (8)
  (scenario-run-tool "code_search" '((pattern . "defun")))
  (scenario-run-tool "code_search" '((pattern . "defun") (file_type . "el")))
  (scenario-run-tool "code_search" '((pattern . "defvar")))
  (scenario-run-tool "code_search" '((pattern . "Line")))
  (scenario-run-tool "code_search" '((pattern . "TODO")))
  (scenario-run-tool "code_search" '((pattern . "NOTFOUND_PATTERN")))
  (scenario-run-tool "code_search" '((pattern . "message")))
  (scenario-run-tool "code_search" '((pattern . "hello")))

  ;; glob_files scenarios (7)
  (scenario-run-tool "glob_files" '((pattern . "*.el")))
  (scenario-run-tool "glob_files" '((pattern . "*.txt")))
  (scenario-run-tool "glob_files" '((pattern . "**/*.el")))
  (scenario-run-tool "glob_files" '((pattern . "*.org")))
  (scenario-run-tool "glob_files" '((pattern . "new*.txt")))
  (scenario-run-tool "glob_files" '((pattern . "subdir/*")))
  (scenario-run-tool "glob_files" '((pattern . "*.xyz"))))

(defun run-org-scenarios ()
  "Run org-mode tool scenarios (15 scenarios)."
  (message "Running org-mode scenarios...")

  ;; org_todo_list scenarios (5)
  (scenario-run-tool "org_todo_list" '((file . "todos.org")))
  (scenario-run-tool "org_todo_list" '())
  (scenario-run-tool "org_todo_list" '((file . "nonexistent.org")))

  ;; Create more org files for testing
  (with-temp-file (expand-file-name "project.org" scenario-temp-dir)
    (insert "* TODO [#A] High priority\n* TODO [#B] Medium priority\n* TODO [#C] Low priority\n"))
  (scenario-run-tool "org_todo_list" '((file . "project.org")))

  (with-temp-file (expand-file-name "work.org" scenario-temp-dir)
    (insert "* DONE Completed task\n* IN-PROGRESS Active task\n* TODO Pending task\n"))
  (scenario-run-tool "org_todo_list" '((file . "work.org")))

  ;; org_add_todo scenarios (5)
  (scenario-run-tool "org_add_todo" '((file . "todos.org") (heading . "New Task A") (state . "TODO")))
  (scenario-run-tool "org_add_todo" '((file . "todos.org") (heading . "New Task B") (state . "TODO") (priority . "A")))
  (scenario-run-tool "org_add_todo" '((file . "project.org") (heading . "Project Task") (state . "TODO")))
  (scenario-run-tool "org_add_todo" '((file . "new-todos.org") (heading . "First Task") (state . "TODO")))
  (scenario-run-tool "org_add_todo" '((file . "../escape.org") (heading . "Evil")))  ; Should fail

  ;; org_set_todo_state scenarios (5)
  (scenario-run-tool "org_set_todo_state" '((file . "todos.org") (heading . "Task 1") (state . "DONE")))
  (scenario-run-tool "org_set_todo_state" '((file . "todos.org") (heading . "Task 3") (state . "IN-PROGRESS")))
  (scenario-run-tool "org_set_todo_state" '((file . "project.org") (heading . "High priority") (state . "DONE")))
  (scenario-run-tool "org_set_todo_state" '((file . "todos.org") (heading . "NOTFOUND") (state . "DONE")))
  (scenario-run-tool "org_set_todo_state" '((file . "work.org") (heading . "Pending task") (state . "DONE"))))

(defun run-security-scenarios ()
  "Run security boundary scenarios (15 scenarios)."
  (message "Running security scenarios...")

  ;; Path traversal attempts - all should fail safely
  (scenario-run-tool "read_file" '((path . "../../../etc/passwd")))
  (scenario-run-tool "read_file" '((path . "/etc/passwd")))
  (scenario-run-tool "read_file" '((path . "~/.bashrc")))
  (scenario-run-tool "read_file" '((path . "..\\..\\etc\\passwd")))
  (scenario-run-tool "read_file" '((path . "....//etc/passwd")))

  (scenario-run-tool "write_file" '((path . "../escape.txt") (content . "evil")))
  (scenario-run-tool "write_file" '((path . "/tmp/evil.txt") (content . "evil")))
  (scenario-run-tool "write_file" '((path . "~/.evil") (content . "evil")))

  (scenario-run-tool "edit_file" '((path . "../escape.txt") (old_text . "x") (new_text . "y")))
  (scenario-run-tool "edit_file" '((path . "/etc/passwd") (old_text . "x") (new_text . "y")))

  ;; Command injection attempts in patterns
  (scenario-run-tool "code_search" '((pattern . "; rm -rf /")))
  (scenario-run-tool "code_search" '((pattern . "$(whoami)")))
  (scenario-run-tool "glob_files" '((pattern . "; cat /etc/passwd")))
  (scenario-run-tool "list_files" '((path . "; ls /")))
  (scenario-run-tool "list_files" '((path . "$(pwd)"))))

(defun run-edge-case-scenarios ()
  "Run edge case scenarios (15 scenarios)."
  (message "Running edge case scenarios...")

  ;; Empty content
  (scenario-run-tool "write_file" '((path . "empty.txt") (content . "")))
  (scenario-run-tool "read_file" '((path . "empty.txt")))

  ;; Very long content
  (let ((long-content (make-string 10000 ?x)))
    (scenario-run-tool "write_file" `((path . "long.txt") (content . ,long-content))))
  (scenario-run-tool "read_file" '((path . "long.txt")))

  ;; Special characters
  (scenario-run-tool "write_file" '((path . "special.txt") (content . "Tab:\t Newline:\n Quote:\" Backslash:\\")))
  (scenario-run-tool "read_file" '((path . "special.txt")))

  ;; Nested directory creation
  (scenario-run-tool "write_file" '((path . "deep/nested/dir/file.txt") (content . "deep")))
  (scenario-run-tool "read_file" '((path . "deep/nested/dir/file.txt")))

  ;; Edit edge cases
  (scenario-run-tool "edit_file" '((path . "data.txt") (old_text . "") (new_text . "inserted")))
  (scenario-run-tool "edit_file" '((path . "nonexistent.txt") (old_text . "x") (new_text . "y")))

  ;; Glob edge cases
  (scenario-run-tool "glob_files" '((pattern . "")))
  (scenario-run-tool "glob_files" '((pattern . ".")))
  (scenario-run-tool "glob_files" '((pattern . "..")))

  ;; Search edge cases
  (scenario-run-tool "code_search" '((pattern . "")))
  (scenario-run-tool "code_search" '((pattern . "."))))

(defun run-performance-scenarios ()
  "Run performance scenarios (20 scenarios)."
  (message "Running performance scenarios...")

  ;; Create many files
  (dotimes (i 10)
    (scenario-run-tool "write_file" `((path . ,(format "perf%03d.txt" i))
                                       (content . ,(format "Performance test file %d" i)))))

  ;; Read many files
  (dotimes (i 10)
    (scenario-run-tool "read_file" `((path . ,(format "perf%03d.txt" i))))))

;;; Main

(defun run-all-scenarios ()
  "Run all 100 scenarios."
  (interactive)

  ;; Clear log file
  (when (file-exists-p scenario-log-file)
    (delete-file scenario-log-file))

  (message "\n=== Starting 100 Tool Scenarios ===\n")

  (unwind-protect
      (progn
        (scenario-setup)

        (run-file-scenarios)        ; 20 scenarios
        (run-search-scenarios)      ; 15 scenarios
        (run-org-scenarios)         ; 15 scenarios
        (run-security-scenarios)    ; 15 scenarios
        (run-edge-case-scenarios)   ; 15 scenarios
        (run-performance-scenarios) ; 20 scenarios

        ;; Print summary
        (let ((total-success 0)
              (total-failure 0)
              (elapsed (float-time (time-subtract (current-time) scenario-start-time))))

          (message "\n=== Scenario Results ===\n")
          (message "%-20s %8s %8s" "Tool" "Success" "Failure")
          (message "%s" (make-string 40 ?-))

          (dolist (entry (sort scenario-results (lambda (a b) (string< (car a) (car b)))))
            (let ((name (car entry))
                  (successes (cadr entry))
                  (failures (cddr entry)))
              (message "%-20s %8d %8d" name successes failures)
              (cl-incf total-success successes)
              (cl-incf total-failure failures)))

          (message "%s" (make-string 40 ?-))
          (message "%-20s %8d %8d" "TOTAL" total-success total-failure)
          (message "\nElapsed time: %.2f seconds" elapsed)
          (message "Log file: %s" scenario-log-file)

          ;; Check storage directory
          (let ((storage-dir (expand-file-name "sage/projects" user-emacs-directory)))
            (message "\nStorage directory: %s" storage-dir)
            (message "Storage exists: %s" (if (file-directory-p storage-dir) "YES" "NO")))

          (message "\n=== Scenarios Complete ===\n")))

    (scenario-teardown)))

;; Run if loaded in batch mode
(when noninteractive
  (run-all-scenarios))

(provide 'run-100-scenarios)
;;; run-100-scenarios.el ends here
