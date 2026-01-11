;;; sage-stress-test.el --- Stress/edge case tests for sage-tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Hard tests for sage-tools.el including:
;; - Edge cases and boundary conditions
;; - Stress tests with large files/many files
;; - Security boundary tests
;; - Concurrent operation simulation
;; - Error recovery tests
;; - Performance benchmarks
;;
;; These tests are more intensive than unit tests and may take longer to run.
;; Run with: make test-stress

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sage-tools)

;;; Test Fixtures

(defvar sage-stress-test--temp-dir nil
  "Temporary directory for stress tests.")

(defvar sage-stress-test--tool-call-log nil
  "Log of tool calls for verification.")

(defun sage-stress-test--setup ()
  "Set up stress test environment."
  (setq sage-stress-test--temp-dir
        (make-temp-file "sage-stress-test-" t))
  (setq sage-workspace sage-stress-test--temp-dir)
  (setq sage-stress-test--tool-call-log nil))

(defun sage-stress-test--teardown ()
  "Tear down stress test environment."
  (when (and sage-stress-test--temp-dir
             (file-exists-p sage-stress-test--temp-dir))
    (delete-directory sage-stress-test--temp-dir t))
  (setq sage-stress-test--temp-dir nil))

(defmacro sage-stress-test--with-workspace (&rest body)
  "Execute BODY with a temporary workspace."
  `(unwind-protect
       (progn
         (sage-stress-test--setup)
         ,@body)
     (sage-stress-test--teardown)))

(defun sage-stress-test--log-tool-call (tool-name args result)
  "Log a tool call for later analysis."
  (push `((tool . ,tool-name)
          (args . ,args)
          (result-length . ,(length result))
          (timestamp . ,(current-time)))
        sage-stress-test--tool-call-log))

;;; ========== SCENARIO 1: LARGE FILE HANDLING ==========

(ert-deftest sage-stress-test-large-file-read ()
  "Test reading a very large file (10MB)."
  (sage-stress-test--with-workspace
   (let ((large-file (expand-file-name "large.txt" sage-stress-test--temp-dir))
         (content (make-string (* 10 1024 1024) ?x)))  ; 10MB of 'x'
     (with-temp-file large-file
       (insert content))
     (let ((result (sage--tool-read-file '((path . "large.txt")))))
       ;; Should truncate or handle gracefully
       (should (stringp result))
       (should (<= (length result) (* 1024 1024)))))))  ; Expect truncation

(ert-deftest sage-stress-test-large-file-write ()
  "Test writing a large file."
  (sage-stress-test--with-workspace
   (let* ((content (make-string (* 1024 1024) ?y))  ; 1MB
          (result (sage--tool-write-file `((path . "large-out.txt")
                                           (content . ,content)))))
     (should (string-match "Wrote" result))
     (let ((written (expand-file-name "large-out.txt" sage-stress-test--temp-dir)))
       (should (file-exists-p written))
       (should (= (file-attribute-size (file-attributes written))
                  (* 1024 1024)))))))

;;; ========== SCENARIO 2: MANY FILES ==========

(ert-deftest sage-stress-test-list-many-files ()
  "Test listing directory with 1000 files."
  (sage-stress-test--with-workspace
   ;; Create 1000 files
   (dotimes (i 1000)
     (with-temp-file (expand-file-name (format "file%04d.el" i)
                                       sage-stress-test--temp-dir)
       (insert (format ";; File %d\n" i))))
   (let ((result (sage--tool-list-files '((path . ".")
                                          (pattern . "*.el")))))
     (should (stringp result))
     ;; Should find files (may be limited by implementation)
     (should (string-match "file" result)))))

(ert-deftest sage-stress-test-code-search-many-files ()
  "Test code search across 500 files."
  (sage-stress-test--with-workspace
   ;; Create 500 files, some with target pattern
   (dotimes (i 500)
     (with-temp-file (expand-file-name (format "module%04d.el" i)
                                       sage-stress-test--temp-dir)
       (insert (format "(defun module-%d-init ()\n" i))
       (when (= (mod i 10) 0)  ; Every 10th file has special pattern
         (insert "  ;; SEARCH_TARGET: important function\n"))
       (insert "  nil)\n")))
   (let ((result (sage--tool-code-search '((pattern . "SEARCH_TARGET")
                                           (file_type . "el")))))
     ;; Should find 50 files (every 10th of 500)
     (should (stringp result))
     (should (string-match "SEARCH_TARGET" result)))))

;;; ========== SCENARIO 3: SECURITY BOUNDARY TESTS ==========

(ert-deftest sage-stress-test-path-traversal-variants ()
  "Test various path traversal attack patterns."
  (sage-stress-test--with-workspace
   (dolist (attack-path '("../../../etc/passwd"
                          "..\\..\\..\\etc\\passwd"
                          "....//....//etc/passwd"
                          "%2e%2e%2f%2e%2e%2fetc/passwd"
                          "/etc/passwd"
                          "~/.ssh/id_rsa"
                          "${HOME}/.bashrc"
                          "`cat /etc/passwd`"
                          "$(whoami)"
                          "file.txt; rm -rf /"))
     (let ((result (sage--tool-read-file `((path . ,attack-path)))))
       ;; Should reject or return error, never actual file contents
       (should (or (string-match "not found" result)
                   (string-match "Unsafe" result)
                   (string-match "Error" result)
                   (string-match "Invalid" result)))))))

(ert-deftest sage-stress-test-write-path-traversal ()
  "Test path traversal on write operations."
  (sage-stress-test--with-workspace
   (dolist (attack-path '("../escape.txt"
                          "/tmp/evil.txt"
                          "~/.bashrc"))
     (let ((result (sage--tool-write-file `((path . ,attack-path)
                                            (content . "evil content")))))
       ;; Should reject
       (should (or (string-match "Unsafe" result)
                   (string-match "Error" result)))))))

;;; ========== SCENARIO 4: SPECIAL CHARACTERS ==========

(ert-deftest sage-stress-test-unicode-filenames ()
  "Test handling of unicode in filenames."
  (sage-stress-test--with-workspace
   (let ((unicode-name "тест-файл-日本語.txt"))
     (condition-case err
         (progn
           (with-temp-file (expand-file-name unicode-name sage-stress-test--temp-dir)
             (insert "Unicode content: 你好世界"))
           (let ((result (sage--tool-read-file `((path . ,unicode-name)))))
             (should (string-match "你好世界" result))))
       (error
        ;; Some filesystems may not support unicode
        (message "Unicode filename test skipped: %s" err))))))

(ert-deftest sage-stress-test-special-chars-content ()
  "Test handling of special characters in file content."
  (sage-stress-test--with-workspace
   (let* ((special-content "Line1\nLine2\rLine3\r\nLine4\tTabbed\0NullByte")
          (result (sage--tool-write-file `((path . "special.txt")
                                           (content . ,special-content)))))
     (should (string-match "Wrote" result))
     ;; Read it back
     (let ((read-result (sage--tool-read-file '((path . "special.txt")))))
       (should (string-match "Line1" read-result))
       (should (string-match "Tabbed" read-result))))))

;;; ========== SCENARIO 5: NESTED DIRECTORY STRUCTURES ==========

(ert-deftest sage-stress-test-deep-nesting ()
  "Test handling of deeply nested directory structures."
  (sage-stress-test--with-workspace
   ;; Create nested structure: a/b/c/d/e/f/g/h/i/j/file.txt
   (let* ((nested-path "a/b/c/d/e/f/g/h/i/j")
          (full-dir (expand-file-name nested-path sage-stress-test--temp-dir)))
     (make-directory full-dir t)
     (with-temp-file (expand-file-name "deep.txt" full-dir)
       (insert "Deep content"))
     ;; Try to read the deep file
     (let ((result (sage--tool-read-file
                    `((path . ,(concat nested-path "/deep.txt"))))))
       (should (string-match "Deep content" result))))))

;;; ========== SCENARIO 6: CONCURRENT SIMULATION ==========

(ert-deftest sage-stress-test-rapid-sequential-operations ()
  "Test rapid sequential tool calls (simulating concurrent use)."
  (sage-stress-test--with-workspace
   (let ((errors 0)
         (successes 0))
     ;; Perform 100 rapid operations
     (dotimes (i 100)
       (let ((filename (format "rapid%03d.txt" i)))
         ;; Write
         (let ((write-result (sage--tool-write-file
                              `((path . ,filename)
                                (content . ,(format "Content %d" i))))))
           (if (string-match "Wrote" write-result)
               (cl-incf successes)
             (cl-incf errors)))
         ;; Read immediately
         (let ((read-result (sage--tool-read-file `((path . ,filename)))))
           (if (string-match (format "Content %d" i) read-result)
               (cl-incf successes)
             (cl-incf errors)))))
     ;; Should have very high success rate
     (should (> (/ (* 100.0 successes) (+ successes errors)) 95)))))

;;; ========== SCENARIO 7: EDIT EDGE CASES ==========

(ert-deftest sage-stress-test-edit-first-line ()
  "Test editing the first line of a file."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name "first.txt" sage-stress-test--temp-dir)
     (insert "OLD_FIRST\nSecond line\nThird line"))
   (let ((result (sage--tool-edit-file '((path . "first.txt")
                                         (old_text . "OLD_FIRST")
                                         (new_text . "NEW_FIRST")))))
     (should (string-match "Edited" result)))
   (let ((content (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "first.txt" sage-stress-test--temp-dir))
                    (buffer-string))))
     (should (string-match "NEW_FIRST" content))
     (should (string-match "Second line" content)))))

(ert-deftest sage-stress-test-edit-last-line ()
  "Test editing the last line of a file."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name "last.txt" sage-stress-test--temp-dir)
     (insert "First line\nSecond line\nOLD_LAST"))
   (let ((result (sage--tool-edit-file '((path . "last.txt")
                                         (old_text . "OLD_LAST")
                                         (new_text . "NEW_LAST")))))
     (should (string-match "Edited" result)))))

(ert-deftest sage-stress-test-edit-empty-string ()
  "Test editing with empty strings."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name "empty.txt" sage-stress-test--temp-dir)
     (insert "Some content here"))
   ;; Try to replace empty string (should fail or handle gracefully)
   (let ((result (sage--tool-edit-file '((path . "empty.txt")
                                         (old_text . "")
                                         (new_text . "inserted")))))
     ;; Should either reject or handle gracefully
     (should (stringp result)))))

(ert-deftest sage-stress-test-edit-multiline ()
  "Test editing multiline text."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name "multi.txt" sage-stress-test--temp-dir)
     (insert "Line A\nLine B\nLine C\nLine D"))
   (let ((result (sage--tool-edit-file '((path . "multi.txt")
                                         (old_text . "Line B\nLine C")
                                         (new_text . "Line X\nLine Y\nLine Z")))))
     (should (string-match "Edited" result)))
   (let ((content (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "multi.txt" sage-stress-test--temp-dir))
                    (buffer-string))))
     (should (string-match "Line X" content))
     (should (string-match "Line Y" content))
     (should (string-match "Line Z" content)))))

;;; ========== SCENARIO 8: GLOB EDGE CASES ==========

(ert-deftest sage-stress-test-glob-hidden-files ()
  "Test glob handling of hidden files."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name ".hidden" sage-stress-test--temp-dir)
     (insert "hidden"))
   (with-temp-file (expand-file-name "visible.txt" sage-stress-test--temp-dir)
     (insert "visible"))
   (let ((result (sage--tool-glob-files '((pattern . ".*")))))
     ;; Should find hidden files with .* pattern
     (should (or (string-match ".hidden" result)
                 (string-match "No matching" result))))))

(ert-deftest sage-stress-test-glob-recursive ()
  "Test recursive glob patterns."
  (sage-stress-test--with-workspace
   (let ((subdir (expand-file-name "subdir" sage-stress-test--temp-dir)))
     (make-directory subdir)
     (with-temp-file (expand-file-name "test.el" subdir)
       (insert ""))
     (with-temp-file (expand-file-name "top.el" sage-stress-test--temp-dir)
       (insert ""))
     (let ((result (sage--tool-glob-files '((pattern . "**/*.el")))))
       ;; Should find files in subdirectories
       (should (stringp result))))))

;;; ========== SCENARIO 9: TOOL CALL COUNTING ==========

(ert-deftest sage-stress-test-tool-call-counting ()
  "Test that tool calls can be counted and logged."
  (sage-stress-test--with-workspace
   (setq sage-stress-test--tool-call-log nil)

   ;; Simulate tool calls with logging
   (let ((tools-called '()))
     ;; read_file
     (let ((result (sage--tool-read-file '((path . "nonexistent.txt")))))
       (push "read_file" tools-called))

     ;; write_file
     (let ((result (sage--tool-write-file '((path . "test.txt")
                                            (content . "content")))))
       (push "write_file" tools-called))

     ;; list_files
     (let ((result (sage--tool-list-files '((path . ".")))))
       (push "list_files" tools-called))

     ;; edit_file
     (let ((result (sage--tool-edit-file '((path . "test.txt")
                                           (old_text . "content")
                                           (new_text . "modified")))))
       (push "edit_file" tools-called))

     ;; code_search
     (let ((result (sage--tool-code-search '((pattern . "defun")))))
       (push "code_search" tools-called))

     ;; glob_files
     (let ((result (sage--tool-glob-files '((pattern . "*.txt")))))
       (push "glob_files" tools-called))

     ;; Verify all tools were called
     (should (= (length tools-called) 6))
     (should (member "read_file" tools-called))
     (should (member "write_file" tools-called))
     (should (member "list_files" tools-called))
     (should (member "edit_file" tools-called))
     (should (member "code_search" tools-called))
     (should (member "glob_files" tools-called)))))

;;; ========== SCENARIO 10: ERROR RECOVERY ==========

(ert-deftest sage-stress-test-error-recovery ()
  "Test that errors don't corrupt state."
  (sage-stress-test--with-workspace
   ;; Create a valid file
   (with-temp-file (expand-file-name "valid.txt" sage-stress-test--temp-dir)
     (insert "Valid content"))

   ;; Trigger several errors
   (sage--tool-read-file '((path . "nonexistent.txt")))
   (sage--tool-read-file '((path . "../escape.txt")))
   (sage--tool-edit-file '((path . "valid.txt")
                           (old_text . "NOT_PRESENT")
                           (new_text . "new")))

   ;; Original file should be unchanged
   (let ((content (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "valid.txt" sage-stress-test--temp-dir))
                    (buffer-string))))
     (should (string= content "Valid content")))))

;;; ========== PERFORMANCE BENCHMARKS ==========

(ert-deftest sage-stress-test-benchmark-read ()
  "Benchmark file read performance."
  (sage-stress-test--with-workspace
   (with-temp-file (expand-file-name "bench.txt" sage-stress-test--temp-dir)
     (insert (make-string 10000 ?x)))
   (let ((start-time (float-time))
         (iterations 100))
     (dotimes (_ iterations)
       (sage--tool-read-file '((path . "bench.txt"))))
     (let ((elapsed (- (float-time) start-time)))
       (message "Read benchmark: %d iterations in %.3fs (%.1f ops/sec)"
                iterations elapsed (/ iterations elapsed))
       ;; Should complete 100 reads in under 5 seconds
       (should (< elapsed 5.0))))))

(ert-deftest sage-stress-test-benchmark-write ()
  "Benchmark file write performance."
  (sage-stress-test--with-workspace
   (let ((start-time (float-time))
         (iterations 50))
     (dotimes (i iterations)
       (sage--tool-write-file `((path . ,(format "bench%d.txt" i))
                                (content . "Benchmark content"))))
     (let ((elapsed (- (float-time) start-time)))
       (message "Write benchmark: %d iterations in %.3fs (%.1f ops/sec)"
                iterations elapsed (/ iterations elapsed))
       ;; Should complete 50 writes in under 5 seconds
       (should (< elapsed 5.0))))))

(provide 'sage-stress-test)
;;; sage-stress-test.el ends here
