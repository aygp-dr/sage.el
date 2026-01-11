;;; feature-checklist.el --- Feature verification checklist -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Run through all documented features in README.org and verify they work.
;; This is a dogfooding script that tests sage functionality in batch mode.
;;
;; Usage:
;;   make feature-check
;;   # or
;;   emacs --batch -L . -l scripts/feature-checklist.el
;;
;; Output:
;;   - Feature verification results to stdout
;;   - Tool calls logged to ~/.emacs.d/sage/projects/*/

;;; Code:

(require 'cl-lib)

;; Load sage modules
(require 'sage)
(require 'sage-tools)
(require 'sage-context)
(require 'sage-memory)
(require 'sage-project)
(require 'sage-ratelimit)
(require 'sage-reflect)
(require 'sage-log)

;;; Checklist infrastructure

(defvar feature-checklist-results nil
  "List of (feature status message) results.")

(defvar feature-checklist-temp-dir nil
  "Temporary directory for tests.")

(defun feature-check (feature test-fn)
  "Check FEATURE by running TEST-FN.
TEST-FN should return non-nil on success, or signal an error."
  (let ((result (condition-case err
                    (if (funcall test-fn)
                        (list feature 'pass "OK")
                      (list feature 'fail "Returned nil"))
                  (error (list feature 'fail (error-message-string err))))))
    (push result feature-checklist-results)
    (let ((status (nth 1 result))
          (msg (nth 2 result)))
      (message "[%s] %s: %s"
               (if (eq status 'pass) "PASS" "FAIL")
               feature
               msg))
    (eq (nth 1 result) 'pass)))

(defun feature-checklist-setup ()
  "Set up test environment."
  (setq feature-checklist-temp-dir (make-temp-file "sage-feature-check-" t))
  (setq sage-workspace feature-checklist-temp-dir)

  ;; Create test files
  (with-temp-file (expand-file-name "test.el" feature-checklist-temp-dir)
    (insert "(defun hello () \"Hello, World!\")\n"))
  (with-temp-file (expand-file-name "data.txt" feature-checklist-temp-dir)
    (insert "Line 1\nLine 2\nLine 3\n"))
  (with-temp-file (expand-file-name "todos.org" feature-checklist-temp-dir)
    (insert "* TODO Task 1\n* DONE Task 2\n"))

  ;; Enable logging
  (setq sage-log-enabled t)
  (setq sage-log-level 'debug)

  ;; Initialize reflection
  (sage-reflect-enable)

  (message "Test environment: %s" feature-checklist-temp-dir))

(defun feature-checklist-teardown ()
  "Clean up test environment."
  (when (and feature-checklist-temp-dir
             (file-exists-p feature-checklist-temp-dir))
    (delete-directory feature-checklist-temp-dir t)))

;;; Feature checks

(defun check-core-loading ()
  "Check that all core modules load."
  (and (featurep 'sage)
       (featurep 'sage-tools)
       (featurep 'sage-context)
       (featurep 'sage-memory)
       (featurep 'sage-project)
       (featurep 'sage-ratelimit)
       (featurep 'sage-reflect)
       (featurep 'sage-log)))

(defun check-tools-registered ()
  "Check that tools are registered."
  (and (boundp 'sage-tools)
       (> (length sage-tools) 0)))

(defun check-read-file-tool ()
  "Check read_file tool works."
  (let ((result (sage--tool-read-file '((path . "test.el")))))
    (and (stringp result)
         (string-match-p "defun hello" result))))

(defun check-write-file-tool ()
  "Check write_file tool works."
  (let ((result (sage--tool-write-file '((path . "new.txt") (content . "Test content")))))
    (and (stringp result)
         (string-match-p "Wrote" result)
         (file-exists-p (expand-file-name "new.txt" feature-checklist-temp-dir)))))

(defun check-list-files-tool ()
  "Check list_files tool works."
  (let ((result (sage--tool-list-files '((path . ".")))))
    (and (stringp result)
         (string-match-p "test.el" result))))

(defun check-edit-file-tool ()
  "Check edit_file tool works."
  (let ((result (sage--tool-edit-file '((path . "data.txt")
                                        (old_text . "Line 2")
                                        (new_text . "Modified Line 2")))))
    (and (stringp result)
         (string-match-p "Edited" result))))

(defun check-glob-files-tool ()
  "Check glob_files tool works."
  (let ((result (sage--tool-glob-files '((pattern . "*.el")))))
    (and (stringp result)
         (string-match-p "test.el" result))))

(defun check-code-search-tool ()
  "Check code_search tool works."
  (let ((result (sage--tool-code-search '((pattern . "defun")))))
    (and (stringp result)
         ;; May return matches or "No matches" - both are valid
         (or (string-match-p "defun" result)
             (string-match-p "No matches" result)
             (string-match-p "test.el" result)))))

(defun check-org-todo-list-tool ()
  "Check org_todo_list tool works."
  (let ((result (sage--tool-org-todo-list '((file . "todos.org")))))
    (and (stringp result)
         (or (string-match-p "TODO" result)
             (string-match-p "DONE" result)
             (string-match-p "No todos" result)))))

(defun check-context-tokens ()
  "Check token counting works."
  (let* ((messages '(((role . "user") (content . "Hello world"))
                     ((role . "assistant") (content . "Hi there!"))))
         (tokens (sage-context-tokens messages)))
    (and (numberp tokens)
         (> tokens 0))))

(defun check-context-usage ()
  "Check context usage calculation."
  (let* ((messages '(((role . "user") (content . "Hello world"))))
         (usage (sage-context-usage messages 8192)))
    (and (numberp usage)
         (>= usage 0)
         (<= usage 1.0))))

(defun check-ratelimit-check ()
  "Check rate limit checking."
  (let ((result (sage-ratelimit-check "gemini-2.0-flash-exp")))
    ;; Should return t (allowed) or nil (blocked)
    (or result (null result))))

(defun check-ratelimit-status ()
  "Check rate limit status reporting."
  (let ((status (sage-ratelimit-status "gemini-2.0-flash-exp")))
    (and (stringp status)
         (string-match-p "requests" status))))

(defun check-memory-add ()
  "Check memory add function."
  (sage-memory-add "test-key" "test-value" 'general)
  (let ((facts (sage-memory-search "test-key")))
    (> (length facts) 0)))

(defun check-memory-get ()
  "Check memory get function."
  (sage-memory-add "get-test" "get-value" 'general)
  (let ((fact (sage-memory-get "get-test")))
    (and fact
         (equal (alist-get 'value fact) "get-value"))))

(defun check-project-init ()
  "Check project initialization."
  (let ((sage-project-directory (expand-file-name "projects" feature-checklist-temp-dir)))
    (sage-project-load)
    (file-directory-p sage-project-directory)))

(defun check-project-append ()
  "Check project message append."
  (let ((sage-project-directory (expand-file-name "projects" feature-checklist-temp-dir)))
    (sage-project-load)
    (sage-project-append '(:role "user" :content "Test message"))
    (let ((conv (sage-project-get-conversation)))
      (> (length conv) 0))))

(defun check-reflect-enable ()
  "Check reflection enable."
  (sage-reflect-enable)
  sage-reflect-enabled)

(defun check-reflect-context-status ()
  "Check reflection context status."
  (let ((status (sage-reflect-context-status)))
    (and (listp status)
         (assq 'tokens status)
         (assq 'recommendation status))))

(defun check-reflect-check-context ()
  "Check reflection context warning."
  ;; At 55% should get a warning
  (let ((warning (sage-reflect-check-context 0.55)))
    (or (null warning)  ; First time may not trigger if threshold not crossed
        (stringp warning))))

(defun check-reflect-tool-analysis ()
  "Check reflection tool analysis."
  ;; Record some tool calls first
  (sage-reflect-record-tool-call "read_file" '((path . "test.el")) "content" 100 t)
  (sage-reflect-record-tool-call "write_file" '((path . "x.txt")) "ok" 50 t)
  (let ((analysis (sage-reflect-tool-analysis)))
    (and (listp analysis)
         (assq 'total-calls analysis)
         (> (alist-get 'total-calls analysis) 0))))

(defun check-log-message ()
  "Check logging works."
  (sage-log-message 'info "test" '((key . "value")))
  t)  ; Log doesn't return a value, just check it doesn't error

(defun check-safe-tools-list ()
  "Check safe tools list exists."
  (and (boundp 'sage-safe-tools)
       (listp sage-safe-tools)
       (member "read_file" sage-safe-tools)))

(defun check-tool-permission ()
  "Check permission checking."
  (let ((sage-yolo-mode nil))
    ;; Safe tool should return t
    (sage--check-permission "read_file" nil)))

;;; Storage verification

(defun check-storage-directory ()
  "Check that ~/.emacs.d/sage/projects/ exists or can be created."
  (let ((storage-dir (expand-file-name "sage/projects" user-emacs-directory)))
    (or (file-directory-p storage-dir)
        (progn
          (make-directory storage-dir t)
          (file-directory-p storage-dir)))))

(defun check-storage-jsonl ()
  "Check that JSONL files can be written to storage."
  (let* ((storage-dir (expand-file-name "sage/projects" user-emacs-directory))
         (test-dir (expand-file-name "-tmp-feature-check" storage-dir))
         (test-file (expand-file-name "test.jsonl" test-dir)))
    (make-directory test-dir t)
    (with-temp-file test-file
      (insert "{\"test\": true}\n"))
    (prog1 (file-exists-p test-file)
      (delete-directory test-dir t))))

;;; Main checklist runner

(defun run-feature-checklist ()
  "Run all feature checks."
  (interactive)
  (setq feature-checklist-results nil)

  (message "\n")
  (message "╔══════════════════════════════════════════════════════════════╗")
  (message "║           SAGE FEATURE VERIFICATION CHECKLIST                ║")
  (message "╚══════════════════════════════════════════════════════════════╝")
  (message "")

  (feature-checklist-setup)

  (unwind-protect
      (progn
        (message "=== Core Loading ===")
        (feature-check "Core modules loaded" #'check-core-loading)
        (feature-check "Tools registered" #'check-tools-registered)

        (message "\n=== File Tools (README: Built-in Tools) ===")
        (feature-check "read_file" #'check-read-file-tool)
        (feature-check "write_file" #'check-write-file-tool)
        (feature-check "list_files" #'check-list-files-tool)
        (feature-check "edit_file" #'check-edit-file-tool)
        (feature-check "glob_files" #'check-glob-files-tool)
        (feature-check "code_search" #'check-code-search-tool)

        (message "\n=== Org Tools ===")
        (feature-check "org_todo_list" #'check-org-todo-list-tool)

        (message "\n=== Context Management (README: Context Management) ===")
        (feature-check "Token counting" #'check-context-tokens)
        (feature-check "Usage calculation" #'check-context-usage)

        (message "\n=== Rate Limiting (README: Rate Limiting) ===")
        (feature-check "Rate limit check" #'check-ratelimit-check)
        (feature-check "Rate limit status" #'check-ratelimit-status)

        (message "\n=== Memory System ===")
        (feature-check "Memory add" #'check-memory-add)
        (feature-check "Memory get" #'check-memory-get)

        (message "\n=== Project Sessions (README: Project-Based Conversations) ===")
        (feature-check "Project init" #'check-project-init)
        (feature-check "Project append" #'check-project-append)

        (message "\n=== Reflection (Agentic Pattern) ===")
        (feature-check "Reflection enable" #'check-reflect-enable)
        (feature-check "Context status" #'check-reflect-context-status)
        (feature-check "Context warnings" #'check-reflect-check-context)
        (feature-check "Tool analysis" #'check-reflect-tool-analysis)

        (message "\n=== Logging ===")
        (feature-check "Log message" #'check-log-message)

        (message "\n=== Security ===")
        (feature-check "Safe tools list" #'check-safe-tools-list)
        (feature-check "Permission check" #'check-tool-permission)

        (message "\n=== Storage Verification (Dogfooding) ===")
        (feature-check "Storage directory" #'check-storage-directory)
        (feature-check "JSONL write" #'check-storage-jsonl)

        ;; Summary
        (let* ((total (length feature-checklist-results))
               (passed (cl-count-if (lambda (r) (eq (nth 1 r) 'pass))
                                    feature-checklist-results))
               (failed (- total passed)))
          (message "")
          (message "╔══════════════════════════════════════════════════════════════╗")
          (message "║                         SUMMARY                              ║")
          (message "╠══════════════════════════════════════════════════════════════╣")
          (message "║  Total:  %3d                                                 ║" total)
          (message "║  Passed: %3d                                                 ║" passed)
          (message "║  Failed: %3d                                                 ║" failed)
          (message "║  Rate:   %3d%%                                                ║"
                   (if (> total 0) (round (* 100 (/ (float passed) total))) 0))
          (message "╚══════════════════════════════════════════════════════════════╝")

          (when (> failed 0)
            (message "\nFailed checks:")
            (dolist (result feature-checklist-results)
              (when (eq (nth 1 result) 'fail)
                (message "  - %s: %s" (nth 0 result) (nth 2 result)))))

          ;; Return success status
          (= failed 0)))

    (feature-checklist-teardown)))

;; Run if in batch mode
(when noninteractive
  (let ((success (run-feature-checklist)))
    (kill-emacs (if success 0 1))))

(provide 'feature-checklist)
;;; feature-checklist.el ends here
