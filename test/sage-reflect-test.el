;;; sage-reflect-test.el --- Tests for sage-reflect -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Tests for reflection and self-awareness functionality.

;;; Code:

(require 'ert)
(require 'sage-reflect)
(require 'sage-context)

;;; Test Data

(defvar sage-reflect-test-messages
  '(((role . "user")
     (content . "Hello, can you help me?"))
    ((role . "assistant")
     (content . "Of course! I'd be happy to help. What do you need?"))
    ((role . "user")
     (content . "I need help writing some Elisp code.")))
  "Sample messages for testing.")

;;; sage-reflect-context-status Tests

(ert-deftest sage-reflect-test-context-status-returns-alist ()
  "Test that sage-reflect-context-status returns proper alist structure."
  (let ((sage-conversation sage-reflect-test-messages)
        (sage-model 'default))
    (let ((status (sage-reflect-context-status)))
      ;; Should return an alist
      (should (listp status))
      ;; Check required keys
      (should (assq 'tokens status))
      (should (assq 'limit status))
      (should (assq 'usage status))
      (should (assq 'usage-pct status))
      (should (assq 'warnings-issued status))
      (should (assq 'recommendation status))
      (should (assq 'session-duration status)))))

(ert-deftest sage-reflect-test-context-status-tokens-are-numbers ()
  "Test that token counts in context status are numbers, not structs."
  (let ((sage-conversation sage-reflect-test-messages)
        (sage-model 'default))
    (let ((status (sage-reflect-context-status)))
      ;; tokens should be a number
      (should (numberp (alist-get 'tokens status)))
      ;; limit should be a number
      (should (numberp (alist-get 'limit status)))
      ;; usage should be a float
      (should (floatp (alist-get 'usage status)))
      ;; usage-pct should be a number
      (should (numberp (alist-get 'usage-pct status))))))

(ert-deftest sage-reflect-test-context-status-no-crash-empty-conversation ()
  "Test that context status works with empty or nil conversation."
  ;; With nil conversation
  (let ((sage-conversation nil)
        (sage-model 'default))
    (should (listp (sage-reflect-context-status))))
  ;; Without sage-conversation bound at all
  (let ((sage-model 'default))
    (when (boundp 'sage-conversation)
      (makunbound 'sage-conversation))
    (should (listp (sage-reflect-context-status)))))

(ert-deftest sage-reflect-test-context-status-usage-ratio-valid ()
  "Test that usage ratio is between 0.0 and 1.0."
  (let ((sage-conversation sage-reflect-test-messages)
        (sage-model 'default))
    (let* ((status (sage-reflect-context-status))
           (usage (alist-get 'usage status)))
      (should (>= usage 0.0))
      ;; For small test messages, usage should be well below 1.0
      (should (< usage 1.0)))))

;;; Tool Call Tracking Tests

(ert-deftest sage-reflect-test-record-tool-call ()
  "Test tool call recording."
  (let ((sage-reflect--tool-calls nil)
        (sage-reflect-track-tool-patterns t))
    (sage-reflect-record-tool-call "test_tool" '((arg . "value")) "result" 100 t)
    (should (= (length sage-reflect--tool-calls) 1))
    (let ((call (car sage-reflect--tool-calls)))
      (should (equal (nth 1 call) "test_tool"))
      (should (equal (nth 4 call) 100))
      (should (equal (nth 5 call) t)))))

(ert-deftest sage-reflect-test-tool-analysis ()
  "Test tool usage analysis."
  (let ((sage-reflect--tool-calls
         `((,(current-time) "read_file" ((path . "/tmp/test")) "content" 50 t)
           (,(current-time) "read_file" ((path . "/tmp/test2")) "content2" 60 t)
           (,(current-time) "write_file" ((path . "/tmp/out")) nil 100 nil))))
    (let ((analysis (sage-reflect-tool-analysis)))
      (should (= (alist-get 'total-calls analysis) 3))
      (should (= (alist-get 'total-errors analysis) 1))
      (should (> (alist-get 'error-rate analysis) 0)))))

;;; Session Summary Tests

(ert-deftest sage-reflect-test-session-summary-structure ()
  "Test that session summary has proper structure."
  (let ((sage-reflect--session-start (current-time))
        (sage-reflect--tool-calls nil)
        (sage-reflect--context-warnings nil)
        (sage-reflect--errors nil)
        (sage-conversation nil)
        (sage-model 'default))
    (let ((summary (sage-reflect-session-summary)))
      (should (assq 'session-duration summary))
      (should (assq 'context summary))
      (should (assq 'tools summary))
      (should (assq 'errors-count summary))
      (should (assq 'warnings-count summary))
      (should (assq 'learnings summary))
      (should (assq 'recommendations summary)))))

;;; Context Warning Tests

(ert-deftest sage-reflect-test-check-context-threshold ()
  "Test context threshold checking."
  (let ((sage-reflect-enabled t)
        (sage-reflect--last-threshold-warned 0.0)
        (sage-reflect--context-warnings nil)
        (sage-reflect-warning-thresholds '(0.50 0.75 0.90)))
    ;; Below threshold - no warning
    (should-not (sage-reflect-check-context 0.40))
    ;; Above first threshold - warning message shows actual usage percentage
    (let ((warning (sage-reflect-check-context 0.55)))
      (should warning)
      (should (string-match-p "55%" warning)))
    ;; Already warned at this level - no new warning
    (should-not (sage-reflect-check-context 0.55))))

;;; Enable/Disable Tests

(ert-deftest sage-reflect-test-enable-disable ()
  "Test enabling and disabling reflection."
  (sage-reflect-enable)
  (should sage-reflect-enabled)
  (should sage-reflect--session-start)
  (should (null sage-reflect--tool-calls))

  (sage-reflect-disable)
  (should-not sage-reflect-enabled))

(provide 'sage-reflect-test)
;;; sage-reflect-test.el ends here
