;;; gemini-repl-memory-test.el --- Tests for gemini-repl-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for gemini-repl-memory package.

;;; Code:

(require 'ert)
(require 'gemini-repl-memory)

;;; Test Helpers

(defvar gemini-repl-memory-test-file nil
  "Temporary file for testing.")

(defun gemini-repl-memory-test-setup ()
  "Setup test environment."
  (setq gemini-repl-memory-test-file
        (make-temp-file "gemini-repl-memory-test" nil ".json"))
  (setq gemini-repl-memory-file gemini-repl-memory-test-file
        gemini-repl-memory--facts nil
        gemini-repl-memory--loaded nil
        gemini-repl-memory-auto-save nil))

(defun gemini-repl-memory-test-teardown ()
  "Cleanup test environment."
  (when (and gemini-repl-memory-test-file
             (file-exists-p gemini-repl-memory-test-file))
    (delete-file gemini-repl-memory-test-file))
  (setq gemini-repl-memory--facts nil
        gemini-repl-memory--loaded nil))

;;; Core Functionality Tests

(ert-deftest test-memory-add-fact ()
  "Test adding a fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (let ((fact (gemini-repl-memory-add "test-key" "test-value" 'general)))
        (should fact)
        (should (string= (plist-get fact :key) "test-key"))
        (should (string= (plist-get fact :value) "test-value"))
        (should (eq (plist-get fact :category) 'general))
        (should (plist-get fact :timestamp)))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-get-fact ()
  "Test retrieving a fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "user-name" "Alice" 'preference)
        (let ((fact (gemini-repl-memory-get "user-name")))
          (should fact)
          (should (string= (plist-get fact :value) "Alice"))
          (should (eq (plist-get fact :category) 'preference))))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-get-nonexistent ()
  "Test retrieving non-existent fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (should-not (gemini-repl-memory-get "nonexistent"))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-update-fact ()
  "Test updating an existing fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "language" "Rust" 'technical)
        (gemini-repl-memory-add "language" "Emacs Lisp" 'technical)
        (let ((fact (gemini-repl-memory-get "language")))
          (should (string= (plist-get fact :value) "Emacs Lisp")))
        ;; Should only have one entry for this key
        (should (= (length gemini-repl-memory--facts) 1)))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-remove-fact ()
  "Test removing a fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "temp" "value" 'general)
        (should (gemini-repl-memory-get "temp"))
        (should (gemini-repl-memory-remove "temp"))
        (should-not (gemini-repl-memory-get "temp")))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-remove-nonexistent ()
  "Test removing non-existent fact."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (should-not (gemini-repl-memory-remove "nonexistent"))
    (gemini-repl-memory-test-teardown)))

;;; Category Tests

(ert-deftest test-memory-categories ()
  "Test all valid categories."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "key1" "val1" 'general)
        (gemini-repl-memory-add "key2" "val2" 'preference)
        (gemini-repl-memory-add "key3" "val3" 'project)
        (gemini-repl-memory-add "key4" "val4" 'technical)
        (should (= (length gemini-repl-memory--facts) 4)))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-invalid-category ()
  "Test that invalid category raises error."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (should-error (gemini-repl-memory-add "key" "value" 'invalid))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-default-category ()
  "Test default category is general."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (let ((fact (gemini-repl-memory-add "key" "value")))
        (should (eq (plist-get fact :category) 'general)))
    (gemini-repl-memory-test-teardown)))

;;; List and Filter Tests

(ert-deftest test-memory-list-all ()
  "Test listing all facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "key1" "val1" 'general)
        (gemini-repl-memory-add "key2" "val2" 'preference)
        (let ((facts (gemini-repl-memory-list)))
          (should (= (length facts) 2))))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-list-by-category ()
  "Test listing facts filtered by category."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "key1" "val1" 'general)
        (gemini-repl-memory-add "key2" "val2" 'preference)
        (gemini-repl-memory-add "key3" "val3" 'preference)
        (let ((prefs (gemini-repl-memory-list 'preference)))
          (should (= (length prefs) 2))
          (dolist (fact prefs)
            (should (eq (plist-get fact :category) 'preference)))))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-list-empty ()
  "Test listing when no facts exist."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (should (null (gemini-repl-memory-list)))
    (gemini-repl-memory-test-teardown)))

;;; Context Generation Tests

(ert-deftest test-memory-to-context-empty ()
  "Test context generation with no facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (should (string-empty-p (gemini-repl-memory-to-context)))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-to-context-with-facts ()
  "Test context generation with facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "user-name" "Bob" 'preference)
        (gemini-repl-memory-add "project-name" "gemini-repl" 'project)
        (let ((context (gemini-repl-memory-to-context)))
          (should (string-match-p "user-name: Bob" context))
          (should (string-match-p "project-name: gemini-repl" context))
          (should (string-match-p "# Stored Facts" context))
          (should (string-match-p "## Preference" context))
          (should (string-match-p "## Project" context))))
    (gemini-repl-memory-test-teardown)))

;;; Persistence Tests

(ert-deftest test-memory-save-load ()
  "Test saving and loading facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        ;; Add some facts
        (gemini-repl-memory-add "key1" "value1" 'general)
        (gemini-repl-memory-add "key2" "value2" 'preference)

        ;; Save to file
        (gemini-repl-memory-save)
        (should (file-exists-p gemini-repl-memory-test-file))

        ;; Clear in-memory facts
        (setq gemini-repl-memory--facts nil
              gemini-repl-memory--loaded nil)

        ;; Load from file
        (gemini-repl-memory-load)
        (should (= (length gemini-repl-memory--facts) 2))
        (should (gemini-repl-memory-get "key1"))
        (should (gemini-repl-memory-get "key2")))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-auto-save ()
  "Test auto-save functionality."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (setq gemini-repl-memory-auto-save t)
        (gemini-repl-memory-add "test" "value" 'general)

        ;; File should exist after add
        (should (file-exists-p gemini-repl-memory-test-file))

        ;; Load in new session
        (setq gemini-repl-memory--facts nil
              gemini-repl-memory--loaded nil)
        (gemini-repl-memory-load)
        (should (gemini-repl-memory-get "test")))
    (gemini-repl-memory-test-teardown)))

(ert-deftest test-memory-clear ()
  "Test clearing all facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "key1" "val1" 'general)
        (gemini-repl-memory-add "key2" "val2" 'preference)
        (should (= (length gemini-repl-memory--facts) 2))

        ;; Clear (simulate yes response)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (gemini-repl-memory-clear))

        (should (null gemini-repl-memory--facts)))
    (gemini-repl-memory-test-teardown)))

;;; JSON Format Tests

(ert-deftest test-memory-json-format ()
  "Test JSON format of saved facts."
  (gemini-repl-memory-test-setup)
  (unwind-protect
      (progn
        (gemini-repl-memory-add "test-key" "test-value" 'technical)
        (gemini-repl-memory-save)

        ;; Read and verify JSON structure
        (with-temp-buffer
          (insert-file-contents gemini-repl-memory-test-file)
          (let ((json-data (json-read)))
            (should (vectorp json-data))
            (should (> (length json-data) 0))
            (let ((fact (aref json-data 0)))
              (should (alist-get 'key fact))
              (should (alist-get 'value fact))
              (should (alist-get 'category fact))
              (should (alist-get 'timestamp fact))))))
    (gemini-repl-memory-test-teardown)))

(provide 'gemini-repl-memory-test)
;;; gemini-repl-memory-test.el ends here
