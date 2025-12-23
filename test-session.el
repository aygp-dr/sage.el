;;; test-session.el --- Test session persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test for gemini-repl-session functionality

;;; Code:

(add-to-list 'load-path ".")
(require 'gemini-repl-session)

;; Test 1: Create a session
(message "Test 1: Creating session...")
(let ((session (gemini-repl-session-new "test-session"
                                         "test-model"
                                         'gemini
                                         "/tmp")))
  (message "  Session name: %s" (gemini-repl-session-name session))
  (message "  Session model: %s" (gemini-repl-session-model session))
  (message "  Session provider: %s" (gemini-repl-session-provider session)))

;; Test 2: Add messages
(message "\nTest 2: Adding messages...")
(gemini-repl-session-add-message "user" "Hello, AI!")
(gemini-repl-session-add-message "assistant" "Hello! How can I help you?")
(message "  Messages added: %d"
         (gemini-repl-session-message-count gemini-repl-session--current))

;; Test 3: Save session
(message "\nTest 3: Saving session...")
(let ((file (gemini-repl-session-save)))
  (message "  Saved to: %s" file))

;; Test 4: Load session
(message "\nTest 4: Loading session...")
(let ((loaded (gemini-repl-session-load "test-session")))
  (message "  Loaded session: %s" (gemini-repl-session-name loaded))
  (message "  Message count: %d" (gemini-repl-session-message-count loaded)))

;; Test 5: Export to JSON
(message "\nTest 5: Exporting to JSON...")
(gemini-repl-session-export-json "/tmp/test-session.json")
(message "  Exported to /tmp/test-session.json")

;; Test 6: Export to Markdown
(message "\nTest 6: Exporting to Markdown...")
(gemini-repl-session-export-markdown "/tmp/test-session.md")
(message "  Exported to /tmp/test-session.md")

;; Test 7: Session stats
(message "\nTest 7: Session statistics...")
(gemini-repl-session-stats)

(message "\n=== All tests completed successfully ===")

;;; test-session.el ends here
