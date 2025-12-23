;;; gemini-repl-context-test.el --- Tests for gemini-repl-context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Tests for token counting and context management.

;;; Code:

(require 'ert)
(require 'gemini-repl-context)

;;; Test Data

(defvar gemini-repl-context-test-messages
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

(ert-deftest gemini-repl-context-test-estimate-tokens ()
  "Test token estimation."
  (should (= (gemini-repl-context-estimate-tokens "test") 1))
  (should (= (gemini-repl-context-estimate-tokens "hello world") 2))
  (should (= (gemini-repl-context-estimate-tokens "") 0))
  (should (= (gemini-repl-context-estimate-tokens nil) 0)))

(ert-deftest gemini-repl-context-test-tokens ()
  "Test message token counting."
  (let* ((stats (gemini-repl-context-tokens gemini-repl-context-test-messages))
         (total (alist-get 'total stats))
         (count (alist-get 'count stats))
         (by-role (alist-get 'by-role stats)))
    (should (> total 0))
    (should (= count 4))
    (should (hash-table-p by-role))
    (should (> (gethash "user" by-role) 0))
    (should (> (gethash "assistant" by-role) 0))))

;;; Usage Calculation Tests

(ert-deftest gemini-repl-context-test-usage ()
  "Test context usage calculation."
  (let ((usage (gemini-repl-context-usage gemini-repl-context-test-messages 1000)))
    (should (floatp usage))
    (should (>= usage 0.0))
    (should (<= usage 1.0))))

(ert-deftest gemini-repl-context-test-needs-compaction ()
  "Test compaction threshold detection."
  ;; With very small max, should need compaction
  (should (gemini-repl-context-needs-compaction-p
           gemini-repl-context-test-messages))
  ;; With huge max, should not need compaction
  (let ((gemini-repl-context-compaction-threshold 0.90))
    (should-not (gemini-repl-context-needs-compaction-p
                 '(((role . "user") (content . "hi")))))))

;;; Compaction Tests

(ert-deftest gemini-repl-context-test-sliding-window ()
  "Test sliding window compaction."
  (let* ((gemini-repl-context-window-size 2)
         (messages '(((role . "user") (content . "msg1"))
                    ((role . "assistant") (content . "msg2"))
                    ((role . "user") (content . "msg3"))
                    ((role . "assistant") (content . "msg4"))))
         (compacted (gemini-repl-context-compact-sliding-window messages)))
    (should (<= (length compacted) 2))
    ;; Should keep most recent messages
    (should (member '((role . "assistant") (content . "msg4")) compacted))))

(ert-deftest gemini-repl-context-test-sliding-window-preserves-system ()
  "Test that sliding window preserves system messages."
  (let* ((gemini-repl-context-window-size 2)
         (messages '(((role . "system") (content . "You are a helpful assistant"))
                    ((role . "user") (content . "msg1"))
                    ((role . "assistant") (content . "msg2"))
                    ((role . "user") (content . "msg3"))))
         (compacted (gemini-repl-context-compact-sliding-window messages)))
    ;; System message should be preserved
    (should (member '((role . "system") (content . "You are a helpful assistant"))
                   compacted))))

;;; Provider Limits Tests

(ert-deftest gemini-repl-context-test-get-max-tokens ()
  "Test max token retrieval."
  (should (= (gemini-repl-context-get-max-tokens "gemini-1.5-pro") 1000000))
  (should (= (gemini-repl-context-get-max-tokens "gpt-4o") 128000))
  (should (= (gemini-repl-context-get-max-tokens "llama3.2") 8192))
  (should (= (gemini-repl-context-get-max-tokens "unknown-model") 8192)))

;;; Status Tests

(ert-deftest gemini-repl-context-test-format-number ()
  "Test number formatting."
  (should (string= (gemini-repl-context-format-number 1000) "1,000"))
  (should (string= (gemini-repl-context-format-number 1000000) "1,000,000"))
  (should (string= (gemini-repl-context-format-number 42) "42")))

;;; Integration Tests

(ert-deftest gemini-repl-context-test-track-message ()
  "Test message tracking."
  (gemini-repl-context-reset-stats)
  (gemini-repl-context-track-message '((role . "user") (content . "test message")))
  (should (> gemini-repl-context-total-tokens 0))
  (should (= gemini-repl-context-message-count 1))
  (should (> (gethash "user" gemini-repl-context-tokens-by-role 0) 0)))

(ert-deftest gemini-repl-context-test-reset-stats ()
  "Test statistics reset."
  (gemini-repl-context-track-message '((role . "user") (content . "test")))
  (gemini-repl-context-reset-stats)
  (should (= gemini-repl-context-total-tokens 0))
  (should (= gemini-repl-context-message-count 0))
  (should (= gemini-repl-context-compaction-count 0)))

(provide 'gemini-repl-context-test)
;;; gemini-repl-context-test.el ends here
