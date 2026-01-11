;;; test-helper.el --- Test helper with coverage support -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Test helper that sets up undercover.el for code coverage.
;; Load this before running tests to enable coverage reporting.
;;
;; Usage:
;;   emacs --batch -L . -l test/test-helper.el -l ert -l test/sage-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Coverage output:
;;   - coverage/lcov.info (LCOV format for Codecov)
;;   - coverage/coverage.txt (text summary)

;;; Code:

(require 'cl-lib)

;; Add project root to load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root))

;;; Undercover setup for code coverage

(defvar test-helper-coverage-enabled nil
  "Whether coverage is enabled for this test run.")

(defvar test-helper-coverage-dir
  (expand-file-name "coverage" (file-name-directory
                                (directory-file-name
                                 (file-name-directory
                                  (or load-file-name buffer-file-name)))))
  "Directory for coverage output files.")

;; Try to load undercover if available
(condition-case err
    (progn
      (require 'undercover)

      ;; Create coverage directory
      (unless (file-directory-p test-helper-coverage-dir)
        (make-directory test-helper-coverage-dir t))

      ;; Configure undercover
      (undercover "sage.el"
                  "sage-context.el"
                  "sage-emacs.el"
                  "sage-log.el"
                  "sage-memory.el"
                  "sage-project.el"
                  "sage-queue.el"
                  "sage-ratelimit.el"
                  "sage-session.el"
                  "sage-tools.el"
                  "sage-tool-factory.el"
                  (:report-format 'lcov)
                  (:send-report nil)
                  (:report-file (expand-file-name "lcov.info" test-helper-coverage-dir)))

      (setq test-helper-coverage-enabled t)
      (message "Coverage enabled - output to %s" test-helper-coverage-dir))
  (error
   (message "undercover.el not available - coverage disabled: %s"
            (error-message-string err))
   (setq test-helper-coverage-enabled nil)))

;;; Test utilities

(defmacro test-helper-with-temp-dir (&rest body)
  "Execute BODY with a temporary directory as default-directory."
  (declare (indent 0) (debug t))
  `(let ((temp-dir (make-temp-file "sage-test-" t)))
     (unwind-protect
         (let ((default-directory temp-dir))
           ,@body)
       (delete-directory temp-dir t))))

(defmacro test-helper-with-mock-response (response &rest body)
  "Execute BODY with a mocked HTTP response."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'url-retrieve-synchronously)
              (lambda (&rest _)
                (let ((buf (generate-new-buffer " *mock-response*")))
                  (with-current-buffer buf
                    (insert "HTTP/1.1 200 OK\n\n")
                    (insert ,response))
                  buf))))
     ,@body))

(defun test-helper-coverage-summary ()
  "Print a summary of coverage results."
  (when test-helper-coverage-enabled
    (let ((lcov-file (expand-file-name "lcov.info" test-helper-coverage-dir)))
      (when (file-exists-p lcov-file)
        (message "\n=== Coverage Summary ===")
        (message "LCOV output: %s" lcov-file)
        ;; Parse LCOV for quick summary
        (with-temp-buffer
          (insert-file-contents lcov-file)
          (let ((total-lines 0)
                (covered-lines 0))
            (while (re-search-forward "^DA:\\([0-9]+\\),\\([0-9]+\\)" nil t)
              (cl-incf total-lines)
              (when (> (string-to-number (match-string 2)) 0)
                (cl-incf covered-lines)))
            (when (> total-lines 0)
              (message "Lines: %d/%d (%.1f%%)"
                       covered-lines total-lines
                       (* 100.0 (/ (float covered-lines) total-lines))))))))))

;; Register coverage summary hook
(add-hook 'kill-emacs-hook #'test-helper-coverage-summary)

(provide 'test-helper)
;;; test-helper.el ends here
