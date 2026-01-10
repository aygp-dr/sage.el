;;; test-tools.el --- Test built-in tools -*- lexical-binding: t; -*-

;; Test script for sage built-in tools

(require 'json)

;; Load sage-tools
(add-to-list 'load-path (file-name-directory (directory-file-name default-directory)))
(require 'sage-tools)

;; Set workspace for testing (use expanded path)
(setq sage-workspace (expand-file-name default-directory))

(defvar test-passed 0)
(defvar test-failed 0)

(defun test-tool (name func args expected-pass)
  "Test a tool and print results.
EXPECTED-PASS indicates if the tool should succeed."
  (message "\n=== Testing: %s ===" name)
  (message "Args: %S" args)
  (condition-case err
      (let ((result (funcall func args)))
        (message "Result (first 300 chars):\n%s"
                 (if (> (length result) 300)
                     (concat (substring result 0 300) "...")
                   result))
        (if expected-pass
            (progn
              (message "Status: PASS")
              (setq test-passed (1+ test-passed)))
          (message "Status: UNEXPECTED PASS")
          (setq test-failed (1+ test-failed)))
        t)
    (error
     (message "Error: %S" err)
     (if expected-pass
         (progn
           (message "Status: FAIL")
           (setq test-failed (1+ test-failed)))
       (progn
         (message "Status: EXPECTED FAIL (OK)")
         (setq test-passed (1+ test-passed))))
     nil)))

(message "")
(message "========================================")
(message "  SAGE BUILT-IN TOOLS TEST")
(message "  Workspace: %s" sage-workspace)
(message "========================================")

;; First, test the safe-path-p function
(message "\n=== Testing: safe-path-p function ===")
(message "README.org safe? %s" (sage-tools--safe-path-p "README.org"))
(message "sage.el safe? %s" (sage-tools--safe-path-p "sage.el"))
(message "../foo safe? %s" (sage-tools--safe-path-p "../foo"))
(message ".env safe? %s" (sage-tools--safe-path-p ".env"))
(message "/etc/passwd safe? %s" (sage-tools--safe-path-p "/etc/passwd"))

;; 1. read_file - Read file contents
(test-tool "read_file"
           #'sage--tool-read-file
           '((path . "README.org"))
           t)

;; 2. list_files - List files in directory
(test-tool "list_files"
           #'sage--tool-list-files
           '((path . "scripts")
             (pattern . "*.el"))
           t)

;; 3. write_file - Write content to file (in workspace)
(test-tool "write_file"
           #'sage--tool-write-file
           '((path . "test-output.txt")
             (content . "Hello from sage write_file tool!"))
           t)

;; Verify write worked
(when (file-exists-p (expand-file-name "test-output.txt" sage-workspace))
  (message "\nVerifying write_file result:")
  (message "Content: %s" (with-temp-buffer
                           (insert-file-contents (expand-file-name "test-output.txt" sage-workspace))
                           (buffer-string)))
  ;; Clean up
  (delete-file (expand-file-name "test-output.txt" sage-workspace))
  (message "Cleaned up test file"))

;; 4. git_status - Get git status
(test-tool "git_status"
           #'sage--tool-git-status
           '()
           t)

;; 5. git_diff - Get git diff
(test-tool "git_diff"
           #'sage--tool-git-diff
           '((staged . nil))
           t)

;; 6. git_log - Get git log
(test-tool "git_log"
           #'sage--tool-git-log
           '((count . 5))
           t)

;; 7. code_search - Search code with ripgrep
(test-tool "code_search"
           #'sage--tool-code-search
           '((pattern . "defun sage")
             (context . 1))
           t)

;; 8. glob_files - Find files matching glob pattern
(test-tool "glob_files"
           #'sage--tool-glob-files
           '((pattern . "*.el"))
           t)

;; Additional test: git_blame
(test-tool "git_blame"
           #'sage--tool-git-blame
           '((path . "sage.el"))
           t)

;; Additional test: git_branch
(test-tool "git_branch"
           #'sage--tool-git-branch
           '()
           t)

(message "")
(message "========================================")
(message "  TEST SUMMARY")
(message "  Passed: %d" test-passed)
(message "  Failed: %d" test-failed)
(message "========================================")

(when (> test-failed 0)
  (kill-emacs 1))
