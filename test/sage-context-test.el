;;; sage-context-test.el --- Tests for sage-context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Tests for token counting and context management.

;;; Code:

(require 'ert)
(require 'sage-context)

;;; Test Data

(defvar sage-context-test-messages
  '(((role . "user")
     (content . "Hello, how are you?"))
    ((role . "assistant")
     (content . "I'm doing well, thank you! How can I help you today?"))
    ((role . "user")
     (content . "Can you help me write a function in Emacs Lisp?"))
    ((role . "assistant")
     (content . "Of course! I'd be happy to help. What kind of function do you need?")))
  "Sample messages for testing.")

;;; Token Counting Tests

(ert-deftest sage-context-test-estimate-tokens ()
  "Test token estimation."
  (should (= (sage-context-estimate-tokens "test") 1))
  (should (= (sage-context-estimate-tokens "hello world") 2))
  (should (= (sage-context-estimate-tokens "") 0))
  (should (= (sage-context-estimate-tokens nil) 0)))

(ert-deftest sage-context-test-tokens-returns-number ()
  "Test that sage-context-tokens returns a number, not a struct."
  (let ((result (sage-context-tokens sage-context-test-messages)))
    ;; Must be an integer
    (should (integerp result))
    ;; Must be positive for non-empty messages
    (should (> result 0))))

(ert-deftest sage-context-test-tokens-empty-messages ()
  "Test sage-context-tokens with empty message list."
  (let ((result (sage-context-tokens nil)))
    (should (integerp result))
    (should (= result 0))))

(ert-deftest sage-context-test-tokens-detailed ()
  "Test detailed message token counting."
  (let* ((stats (sage-context-tokens-detailed sage-context-test-messages))
         (total (alist-get 'total stats))
         (count (alist-get 'count stats))
         (by-role (alist-get 'by-role stats)))
    (should (> total 0))
    (should (= count 4))
    (should (hash-table-p by-role))
    (should (> (gethash "user" by-role) 0))
    (should (> (gethash "assistant" by-role) 0))))

;;; Usage Calculation Tests

(ert-deftest sage-context-test-usage ()
  "Test context usage calculation."
  (let ((usage (sage-context-usage sage-context-test-messages 1000)))
    (should (floatp usage))
    (should (>= usage 0.0))
    (should (<= usage 1.0))))

(ert-deftest sage-context-test-needs-compaction ()
  "Test compaction threshold detection."
  ;; With very small max tokens, should need compaction
  (should (sage-context-needs-compaction-p
           sage-context-test-messages 10))
  ;; With huge max, should not need compaction
  (let ((sage-context-compaction-threshold 0.90))
    (should-not (sage-context-needs-compaction-p
                 '(((role . "user") (content . "hi"))) 100000))))

;;; Compaction Tests

(ert-deftest sage-context-test-sliding-window ()
  "Test sliding window compaction."
  (let* ((sage-context-window-size 2)
         (messages '(((role . "user") (content . "msg1"))
                    ((role . "assistant") (content . "msg2"))
                    ((role . "user") (content . "msg3"))
                    ((role . "assistant") (content . "msg4"))))
         (compacted (sage-context-compact-sliding-window messages)))
    (should (<= (length compacted) 2))
    ;; Should keep most recent messages
    (should (member '((role . "assistant") (content . "msg4")) compacted))))

(ert-deftest sage-context-test-sliding-window-preserves-system ()
  "Test that sliding window preserves system messages."
  (let* ((sage-context-window-size 2)
         (messages '(((role . "system") (content . "You are a helpful assistant"))
                    ((role . "user") (content . "msg1"))
                    ((role . "assistant") (content . "msg2"))
                    ((role . "user") (content . "msg3"))))
         (compacted (sage-context-compact-sliding-window messages)))
    ;; System message should be preserved
    (should (member '((role . "system") (content . "You are a helpful assistant"))
                   compacted))))

;;; Provider Limits Tests

(ert-deftest sage-context-test-get-max-tokens ()
  "Test max token retrieval."
  (should (= (sage-context-get-max-tokens "gemini-1.5-pro") 1000000))
  (should (= (sage-context-get-max-tokens "gpt-4o") 128000))
  (should (= (sage-context-get-max-tokens "llama3.2") 8192))
  (should (= (sage-context-get-max-tokens "unknown-model") 8192)))

;;; Status Tests

(ert-deftest sage-context-test-format-number ()
  "Test number formatting."
  (should (string= (sage-context-format-number 1000) "1,000"))
  (should (string= (sage-context-format-number 1000000) "1,000,000"))
  (should (string= (sage-context-format-number 42) "42")))

;;; Integration Tests

(ert-deftest sage-context-test-track-message ()
  "Test message tracking."
  (sage-context-reset-stats)
  (sage-context-track-message '((role . "user") (content . "test message")))
  (should (> sage-context-total-tokens 0))
  (should (= sage-context-message-count 1))
  (should (> (gethash "user" sage-context-tokens-by-role 0) 0)))

(ert-deftest sage-context-test-reset-stats ()
  "Test statistics reset."
  (sage-context-track-message '((role . "user") (content . "test")))
  (sage-context-reset-stats)
  (should (= sage-context-total-tokens 0))
  (should (= sage-context-message-count 0))
  (should (= sage-context-compaction-count 0)))

(provide 'sage-context-test)
;;; sage-context-test.el ends here
