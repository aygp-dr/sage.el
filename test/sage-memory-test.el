;;; sage-memory-test.el --- Tests for sage-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sage-memory package.

;;; Code:

(require 'ert)
(require 'sage-memory)

;;; Test Helpers

(defvar sage-memory-test-file nil
  "Temporary file for testing.")

(defun sage-memory-test-setup ()
  "Setup test environment."
  (setq sage-memory-test-file
        (make-temp-file "sage-memory-test" nil ".json"))
  (setq sage-memory-file sage-memory-test-file
        sage-memory--facts nil
        sage-memory--loaded nil
        sage-memory-auto-load nil
        sage-memory-auto-save nil))

(defun sage-memory-test-teardown ()
  "Cleanup test environment."
  (when (and sage-memory-test-file
             (file-exists-p sage-memory-test-file))
    (delete-file sage-memory-test-file))
  (setq sage-memory--facts nil
        sage-memory--loaded nil))

;;; Core Functionality Tests

(ert-deftest test-memory-add-fact ()
  "Test adding a fact."
  (sage-memory-test-setup)
  (unwind-protect
      (let ((fact (sage-memory-add "test-key" "test-value" 'general)))
        (should fact)
        (should (string= (plist-get fact :key) "test-key"))
        (should (string= (plist-get fact :value) "test-value"))
        (should (eq (plist-get fact :category) 'general))
        (should (plist-get fact :timestamp)))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-get-fact ()
  "Test retrieving a fact."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "user-name" "Alice" 'preference)
        (let ((fact (sage-memory-get "user-name")))
          (should fact)
          (should (string= (plist-get fact :value) "Alice"))
          (should (eq (plist-get fact :category) 'preference))))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-get-nonexistent ()
  "Test retrieving non-existent fact."
  (sage-memory-test-setup)
  (unwind-protect
      (should-not (sage-memory-get "nonexistent"))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-update-fact ()
  "Test updating an existing fact."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "language" "Rust" 'technical)
        (sage-memory-add "language" "Emacs Lisp" 'technical)
        (let ((fact (sage-memory-get "language")))
          (should (string= (plist-get fact :value) "Emacs Lisp")))
        ;; Should only have one entry for this key
        (should (= (length sage-memory--facts) 1)))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-remove-fact ()
  "Test removing a fact."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "temp" "value" 'general)
        (should (sage-memory-get "temp"))
        (should (sage-memory-remove "temp"))
        (should-not (sage-memory-get "temp")))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-remove-nonexistent ()
  "Test removing non-existent fact."
  (sage-memory-test-setup)
  (unwind-protect
      (should-not (sage-memory-remove "nonexistent"))
    (sage-memory-test-teardown)))

;;; Category Tests

(ert-deftest test-memory-categories ()
  "Test all valid categories."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "key1" "val1" 'general)
        (sage-memory-add "key2" "val2" 'preference)
        (sage-memory-add "key3" "val3" 'project)
        (sage-memory-add "key4" "val4" 'technical)
        (should (= (length sage-memory--facts) 4)))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-invalid-category ()
  "Test that invalid category raises error."
  (sage-memory-test-setup)
  (unwind-protect
      (should-error (sage-memory-add "key" "value" 'invalid))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-default-category ()
  "Test default category is general."
  (sage-memory-test-setup)
  (unwind-protect
      (let ((fact (sage-memory-add "key" "value")))
        (should (eq (plist-get fact :category) 'general)))
    (sage-memory-test-teardown)))

;;; List and Filter Tests

(ert-deftest test-memory-list-all ()
  "Test listing all facts."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "key1" "val1" 'general)
        (sage-memory-add "key2" "val2" 'preference)
        (let ((facts (sage-memory-list)))
          (should (= (length facts) 2))))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-list-by-category ()
  "Test listing facts filtered by category."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "key1" "val1" 'general)
        (sage-memory-add "key2" "val2" 'preference)
        (sage-memory-add "key3" "val3" 'preference)
        (let ((prefs (sage-memory-list 'preference)))
          (should (= (length prefs) 2))
          (dolist (fact prefs)
            (should (eq (plist-get fact :category) 'preference)))))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-list-empty ()
  "Test listing when no facts exist."
  (sage-memory-test-setup)
  (unwind-protect
      (should (null (sage-memory-list)))
    (sage-memory-test-teardown)))

;;; Context Generation Tests

(ert-deftest test-memory-to-context-empty ()
  "Test context generation with no facts."
  (sage-memory-test-setup)
  (unwind-protect
      (should (string-empty-p (sage-memory-to-context)))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-to-context-with-facts ()
  "Test context generation with facts."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "user-name" "Bob" 'preference)
        (sage-memory-add "project-name" "sage" 'project)
        (let ((context (sage-memory-to-context)))
          (should (string-match-p "user-name: Bob" context))
          (should (string-match-p "project-name: sage" context))
          (should (string-match-p "# Stored Facts" context))
          (should (string-match-p "## Preference" context))
          (should (string-match-p "## Project" context))))
    (sage-memory-test-teardown)))

;;; Persistence Tests

(ert-deftest test-memory-save-load ()
  "Test saving and loading facts."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        ;; Add some facts
        (sage-memory-add "key1" "value1" 'general)
        (sage-memory-add "key2" "value2" 'preference)

        ;; Save to file
        (sage-memory-save)
        (should (file-exists-p sage-memory-test-file))

        ;; Clear in-memory facts
        (setq sage-memory--facts nil
              sage-memory--loaded nil)

        ;; Load from file
        (sage-memory-load)
        (should (= (length sage-memory--facts) 2))
        (should (sage-memory-get "key1"))
        (should (sage-memory-get "key2")))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-auto-save ()
  "Test auto-save functionality."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (setq sage-memory-auto-save t)
        (sage-memory-add "test" "value" 'general)

        ;; File should exist after add
        (should (file-exists-p sage-memory-test-file))

        ;; Load in new session
        (setq sage-memory--facts nil
              sage-memory--loaded nil)
        (sage-memory-load)
        (should (sage-memory-get "test")))
    (sage-memory-test-teardown)))

(ert-deftest test-memory-clear ()
  "Test clearing all facts."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "key1" "val1" 'general)
        (sage-memory-add "key2" "val2" 'preference)
        (should (= (length sage-memory--facts) 2))

        ;; Clear (simulate yes response)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (sage-memory-clear))

        (should (null sage-memory--facts)))
    (sage-memory-test-teardown)))

;;; JSON Format Tests

(ert-deftest test-memory-json-format ()
  "Test JSON format of saved facts."
  (sage-memory-test-setup)
  (unwind-protect
      (progn
        (sage-memory-add "test-key" "test-value" 'technical)
        (sage-memory-save)

        ;; Read and verify JSON structure
        (with-temp-buffer
          (insert-file-contents sage-memory-test-file)
          (let ((json-data (json-read)))
            (should (vectorp json-data))
            (should (> (length json-data) 0))
            (let ((fact (aref json-data 0)))
              (should (alist-get 'key fact))
              (should (alist-get 'value fact))
              (should (alist-get 'category fact))
              (should (alist-get 'timestamp fact))))))
    (sage-memory-test-teardown)))

(provide 'sage-memory-test)
;;; sage-memory-test.el ends here
