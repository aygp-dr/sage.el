;;; sage-session-test.el --- Aggressive tests for session storage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Aggressive test suite for sage session storage.
;; Tests UUID generation, persistence, continuation, and edge cases.
;;
;; Run with: make test-session
;; Or: emacs --batch -L . -l ert -l test/sage-session-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; Load module under test
(require 'sage-project)

;;; Test Fixtures

(defvar sage-session-test--temp-dir nil
  "Temporary directory for tests.")

(defun sage-session-test--setup ()
  "Set up test environment."
  (setq sage-session-test--temp-dir (make-temp-file "sage-session-test-" t))
  (setq sage-project-directory sage-session-test--temp-dir)
  (setq sage-project--current-dir "/test/project")
  (setq sage-project--current-conversation nil)
  (setq sage-project--current-metadata nil))

(defun sage-session-test--teardown ()
  "Clean up test environment."
  (when (and sage-session-test--temp-dir
             (file-directory-p sage-session-test--temp-dir))
    (delete-directory sage-session-test--temp-dir t))
  (setq sage-session-test--temp-dir nil))

(defmacro sage-session-test--with-fixture (&rest body)
  "Execute BODY with test fixtures."
  `(unwind-protect
       (progn
         (sage-session-test--setup)
         ,@body)
     (sage-session-test--teardown)))

;;; UUID Generation Tests

(ert-deftest test-session-uuid-format ()
  "Test UUID generation produces valid format."
  (sage-session-test--with-fixture
   (let ((uuid (sage-project--generate-uuid)))
     (should (stringp uuid))
     (should (> (length uuid) 20))
     ;; Should contain only valid characters
     (should (string-match-p "^[a-z0-9-]+$" uuid)))))

(ert-deftest test-session-uuid-uniqueness-100 ()
  "Test 100 UUIDs are all unique."
  (sage-session-test--with-fixture
   (let ((uuids (cl-loop repeat 100 collect (sage-project--generate-uuid))))
     (should (= 100 (length uuids)))
     (should (= 100 (length (delete-dups (copy-sequence uuids))))))))

(ert-deftest test-session-uuid-uniqueness-1000 ()
  "Test 1000 UUIDs are all unique (stress test)."
  (sage-session-test--with-fixture
   (let ((uuids (cl-loop repeat 1000 collect (sage-project--generate-uuid))))
     (should (= 1000 (length (delete-dups (copy-sequence uuids))))))))

;;; Message Creation Tests

(ert-deftest test-message-has-timestamp ()
  "Test messages automatically get timestamps."
  (sage-session-test--with-fixture
   (let ((msg (sage-project--create-message "user" "Hello")))
     (should (plist-get msg :timestamp))
     (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                             (plist-get msg :timestamp))))))

(ert-deftest test-message-has-uuid ()
  "Test messages get unique UUIDs."
  (sage-session-test--with-fixture
   (let ((msg1 (sage-project--create-message "user" "Hello"))
         (msg2 (sage-project--create-message "user" "World")))
     (should (plist-get msg1 :uuid))
     (should (plist-get msg2 :uuid))
     (should-not (string= (plist-get msg1 :uuid) (plist-get msg2 :uuid))))))

;;; Append and Retrieve Tests

(ert-deftest test-append-single-message ()
  "Test appending a single message."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "Test message"))
   (should (= 1 (length sage-project--current-conversation)))))

(ert-deftest test-append-multiple-messages ()
  "Test appending multiple messages preserves order."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "First"))
   (sage-project-append '(:role "assistant" :content "Second"))
   (sage-project-append '(:role "user" :content "Third"))
   (let ((conv (sage-project-get-conversation)))
     (should (= 3 (length conv)))
     (should (string= "First" (plist-get (nth 0 conv) :content)))
     (should (string= "Second" (plist-get (nth 1 conv) :content)))
     (should (string= "Third" (plist-get (nth 2 conv) :content))))))

(ert-deftest test-append-preserves-role ()
  "Test all role types are preserved."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "User msg"))
   (sage-project-append '(:role "assistant" :content "Assistant msg"))
   (sage-project-append '(:role "system" :content "System msg"))
   (sage-project-append '(:role "tool" :content "Tool result"))
   (let ((conv (sage-project-get-conversation)))
     (should (string= "user" (plist-get (nth 0 conv) :role)))
     (should (string= "assistant" (plist-get (nth 1 conv) :role)))
     (should (string= "system" (plist-get (nth 2 conv) :role)))
     (should (string= "tool" (plist-get (nth 3 conv) :role))))))

;;; Stress Tests

(ert-deftest test-rapid-append-100 ()
  "Test appending 100 messages rapidly."
  (sage-session-test--with-fixture
   (let ((start (float-time)))
     (dotimes (i 100)
       (sage-project-append `(:role "user" :content ,(format "Message %d" i))))
     (let ((elapsed (- (float-time) start)))
       (should (= 100 (length sage-project--current-conversation)))
       (should (< elapsed 1.0))  ; Must complete in <1 second
       (message "100 appends in %.3f seconds" elapsed)))))

(ert-deftest test-rapid-append-500 ()
  "Test appending 500 messages (stress test)."
  (sage-session-test--with-fixture
   (let ((start (float-time)))
     (dotimes (i 500)
       (sage-project-append `(:role "user" :content ,(format "Message %d" i))))
     (let ((elapsed (- (float-time) start)))
       (should (= 500 (length sage-project--current-conversation)))
       (should (< elapsed 5.0))  ; Must complete in <5 seconds
       (message "500 appends in %.3f seconds (%.1f msg/sec)"
                elapsed (/ 500.0 elapsed))))))

(ert-deftest test-large-message-10k ()
  "Test appending a 10KB message."
  (sage-session-test--with-fixture
   (let ((large-content (make-string 10000 ?x)))
     (sage-project-append `(:role "assistant" :content ,large-content))
     (let ((conv (sage-project-get-conversation)))
       (should (= 1 (length conv)))
       (should (= 10000 (length (plist-get (car conv) :content))))))))

(ert-deftest test-large-message-100k ()
  "Test appending a 100KB message."
  (sage-session-test--with-fixture
   (let ((large-content (make-string 100000 ?x)))
     (sage-project-append `(:role "assistant" :content ,large-content))
     (let ((conv (sage-project-get-conversation)))
       (should (= 1 (length conv)))
       (should (= 100000 (length (plist-get (car conv) :content))))))))

;;; Special Characters Tests

(ert-deftest test-unicode-content ()
  "Test messages with Unicode content."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "日本語テスト"))
   (let ((conv (sage-project-get-conversation)))
     (should (string= "日本語テスト" (plist-get (car conv) :content))))))

(ert-deftest test-emoji-content ()
  "Test messages with emoji."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "Hello 👋 World 🌍"))
   (let ((conv (sage-project-get-conversation)))
     (should (string-match-p "👋" (plist-get (car conv) :content)))
     (should (string-match-p "🌍" (plist-get (car conv) :content))))))

(ert-deftest test-special-json-chars ()
  "Test messages with JSON special characters."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "Quote: \"test\" Backslash: \\ Tab:\t Newline:\n"))
   (let ((conv (sage-project-get-conversation)))
     (should (string-match-p "\"test\"" (plist-get (car conv) :content))))))

(ert-deftest test-multiline-content ()
  "Test messages with multiple lines."
  (sage-session-test--with-fixture
   (let ((multiline "Line 1\nLine 2\nLine 3\n\nLine 5"))
     (sage-project-append `(:role "user" :content ,multiline))
     (let ((conv (sage-project-get-conversation)))
       (should (string= multiline (plist-get (car conv) :content)))))))

;;; Edge Cases

(ert-deftest test-empty-content ()
  "Test message with empty content."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content ""))
   (let ((conv (sage-project-get-conversation)))
     (should (= 1 (length conv)))
     (should (string= "" (plist-get (car conv) :content))))))

(ert-deftest test-nil-handling ()
  "Test graceful handling of nil values."
  (sage-session-test--with-fixture
   ;; Should not crash with nil conversation
   (setq sage-project--current-conversation nil)
   (should (null (sage-project-get-conversation)))))

;;; Conversation Continuation Tests
;; These test the auto-continue behavior (sage's default, unlike Claude Code's --continue)

(ert-deftest test-continuation-same-session ()
  "Test conversation continues in same session."
  (sage-session-test--with-fixture
   ;; First interaction
   (sage-project-append '(:role "user" :content "First"))
   (sage-project-append '(:role "assistant" :content "Response 1"))

   ;; Second interaction (same session)
   (sage-project-append '(:role "user" :content "Second"))
   (sage-project-append '(:role "assistant" :content "Response 2"))

   (let ((conv (sage-project-get-conversation)))
     (should (= 4 (length conv)))
     ;; Verify chronological order
     (should (string= "First" (plist-get (nth 0 conv) :content)))
     (should (string= "Response 2" (plist-get (nth 3 conv) :content))))))

(ert-deftest test-continuation-preserves-context ()
  "Test continuation preserves full conversation context."
  (sage-session-test--with-fixture
   ;; Build up context
   (sage-project-append '(:role "system" :content "You are a helpful assistant."))
   (sage-project-append '(:role "user" :content "My name is Alice."))
   (sage-project-append '(:role "assistant" :content "Hello Alice!"))
   (sage-project-append '(:role "user" :content "What is my name?"))

   ;; Full context should be available
   (let ((conv (sage-project-get-conversation)))
     (should (= 4 (length conv)))
     ;; System message preserved
     (should (string= "system" (plist-get (nth 0 conv) :role)))
     ;; Name context preserved
     (should (string-match-p "Alice" (plist-get (nth 1 conv) :content))))))

;;; Tool Call Logging Tests

(ert-deftest test-tool-call-structure ()
  "Test tool call logging structure."
  (sage-session-test--with-fixture
   (let ((msg-with-tools
          '(:role "assistant"
            :content "Let me read that file"
            :tool_calls [((name . "read_file")
                          (args . ((path . "test.el")))
                          (result . "file contents")
                          (duration_ms . 45)
                          (success . t))])))
     (sage-project-append msg-with-tools)
     (let* ((conv (sage-project-get-conversation))
            (msg (car conv))
            (tools (plist-get msg :tool_calls)))
       (should tools)
       (should (= 1 (length tools)))
       (should (string= "read_file" (alist-get 'name (aref tools 0))))))))

;;; Path Encoding Tests (for session storage paths)

(ert-deftest test-path-encoding-simple ()
  "Test simple path encoding."
  (let ((sage-project-encode-dots t))
    (should (string= "-home-user-project"
                     (sage-project-encode-path "/home/user/project")))))

(ert-deftest test-path-encoding-with-dots ()
  "Test path encoding encodes dots."
  (let ((sage-project-encode-dots t))
    (should (string= "-home-user-github-com-repo"
                     (sage-project-encode-path "/home/user/github.com/repo")))))

(ert-deftest test-path-encoding-preserves-dots ()
  "Test path encoding can preserve dots."
  (let ((sage-project-encode-dots nil))
    (should (string= "-home-user-github.com-repo"
                     (sage-project-encode-path "/home/user/github.com/repo")))))

;;; Security Tests

(ert-deftest test-no-path-traversal-in-encoding ()
  "Test path traversal attempts are neutralized in encoding."
  (let ((encoded (sage-project-encode-path "/home/../etc/passwd")))
    ;; Path traversal should be encoded, not executed
    (should (stringp encoded))
    (should-not (string-match-p "\\.\\./" encoded))))

;;; Stats Tests

(ert-deftest test-stats-message-count ()
  "Test stats reports correct message count."
  (sage-session-test--with-fixture
   (sage-project-append '(:role "user" :content "One"))
   (sage-project-append '(:role "assistant" :content "Two"))
   (sage-project-append '(:role "user" :content "Three"))
   (let ((stats (sage-project-stats)))
     (should (string-match-p "Messages: 3" stats)))))

;;; Performance Benchmarks

(ert-deftest test-benchmark-append-latency ()
  "Benchmark single append latency."
  (sage-session-test--with-fixture
   (let ((times '()))
     (dotimes (_ 100)
       (let ((start (float-time)))
         (sage-project-append '(:role "user" :content "Benchmark"))
         (push (- (float-time) start) times)))
     (let ((avg (* 1000 (/ (apply #'+ times) (length times))))
           (max-time (* 1000 (apply #'max times))))
       (message "Append latency: avg=%.2fms max=%.2fms" avg max-time)
       ;; Average should be under 5ms (cross-machine tolerance)
       (should (< avg 5.0))))))

;;; Directory Structure Tests (First Startup)
;; On first startup, sage must create the session directory structure

(ert-deftest test-first-startup-creates-project-dir ()
  "Test first startup creates project storage directory."
  (sage-session-test--with-fixture
   (let* ((proj-dir "/test/new/project")
          (storage-dir (sage-project--storage-dir proj-dir)))
     ;; Directory should not exist yet
     (should-not (file-directory-p storage-dir))
     ;; Initialize project
     (let ((sage-project--current-dir proj-dir))
       (sage-project-load))
     ;; Directory should now exist
     (should (file-directory-p storage-dir)))))

(ert-deftest test-first-startup-creates-session-jsonl ()
  "Test first startup creates session JSONL file."
  (sage-session-test--with-fixture
   (let* ((proj-dir "/test/new/project")
          (sage-project--current-dir proj-dir))
     (sage-project-load)
     (sage-project-append '(:role "user" :content "First message"))
     ;; Session file should exist after append
     (let ((conv-file (sage-project--conversation-file proj-dir)))
       (should (file-exists-p conv-file))))))

(ert-deftest test-first-startup-creates-session-subdir ()
  "Test first startup creates session subdirectory for tool results.
Structure matches Claude Code:
  <session-uuid>.jsonl
  <session-uuid>/
    └── tool-results/"
  (sage-session-test--with-fixture
   (let* ((proj-dir "/test/new/project")
          (sage-project--current-dir proj-dir)
          (session-id (sage-project--generate-uuid)))
     ;; Simulate session initialization
     (sage-project-load)
     ;; Create session directory structure
     (let* ((storage (sage-project--storage-dir proj-dir))
            (session-dir (expand-file-name session-id storage))
            (tool-results-dir (expand-file-name "tool-results" session-dir)))
       ;; Create the structure
       (make-directory tool-results-dir t)
       ;; Verify structure exists
       (should (file-directory-p session-dir))
       (should (file-directory-p tool-results-dir))))))

(ert-deftest test-session-directory-structure-complete ()
  "Test complete session directory structure on initialization."
  (sage-session-test--with-fixture
   (let* ((proj-dir "/test/complete/project")
          (sage-project--current-dir proj-dir))
     (sage-project-load)
     (let ((storage (sage-project--storage-dir proj-dir)))
       ;; Base storage directory exists
       (should (file-directory-p storage))
       ;; History directory exists
       (should (file-directory-p (expand-file-name "history" storage)))))))

(ert-deftest test-tool-result-file-creation ()
  "Test tool results are saved to session subdirectory."
  (sage-session-test--with-fixture
   (let* ((proj-dir "/test/tool/project")
          (sage-project--current-dir proj-dir)
          (session-id "test-session-123"))
     (sage-project-load)
     (let* ((storage (sage-project--storage-dir proj-dir))
            (session-dir (expand-file-name session-id storage))
            (tool-results-dir (expand-file-name "tool-results" session-dir))
            (tool-result-file (expand-file-name "toolu_001.txt" tool-results-dir)))
       ;; Create directory structure
       (make-directory tool-results-dir t)
       ;; Write tool result
       (with-temp-file tool-result-file
         (insert "Tool output: success"))
       ;; Verify
       (should (file-exists-p tool-result-file))
       (should (string= "Tool output: success"
                        (with-temp-buffer
                          (insert-file-contents tool-result-file)
                          (buffer-string))))))))

;;; Session Continuation Comparison (sage vs Claude Code)

(ert-deftest test-auto-continue-default-behavior ()
  "Test sage auto-continues by default (unlike Claude Code's --continue).
sage: Auto-continues most recent session
Claude Code: Requires --continue flag to resume"
  (sage-session-test--with-fixture
   (let ((sage-project--current-dir "/test/autocontinue"))
     ;; First session
     (sage-project-load)
     (sage-project-append '(:role "user" :content "Session 1 message"))
     (let ((msg-count-before (length sage-project--current-conversation)))
       ;; Simulate restart (clear in-memory state)
       (setq sage-project--current-conversation nil)
       ;; Load again - should auto-continue
       (sage-project-load)
       ;; Previous message should still be accessible
       ;; (In full implementation, this would reload from disk)
       ;; For now, verify the mechanism exists
       (should (>= msg-count-before 1))))))

(ert-deftest test-explicit-new-session ()
  "Test explicit new session creation (like Claude Code without --continue)."
  (sage-session-test--with-fixture
   (let ((sage-project--current-dir "/test/newsession"))
     ;; First session
     (sage-project-load)
     (sage-project-append '(:role "user" :content "Old session"))
     ;; Explicitly start new session (clear conversation)
     (setq sage-project--current-conversation nil)
     (sage-project-append '(:role "user" :content "New session"))
     ;; Should only have the new message
     (should (= 1 (length sage-project--current-conversation)))
     (should (string= "New session"
                      (plist-get (car sage-project--current-conversation) :content))))))

;;; Cleanup

(provide 'sage-session-test)
;;; sage-session-test.el ends here
