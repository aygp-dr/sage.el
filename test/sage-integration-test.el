;;; sage-integration-test.el --- Live integration tests for sage -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests that require a live Ollama instance.
;; Run with: make test-integration
;; Requires: Ollama running on localhost:11434 with llama3.2:1b

;;; Code:

(require 'ert)
(require 'sage)

;;; Test Helpers

(defun sage-test--ollama-available-p ()
  "Check if Ollama is available on localhost."
  (condition-case nil
      (let ((url-request-method "GET"))
        (with-current-buffer (url-retrieve-synchronously
                              "http://localhost:11434/api/tags"
                              t nil 5)
          (goto-char (point-min))
          (search-forward "\n\n" nil t)
          (let ((json (json-read)))
            (and (assoc 'models json) t))))
    (error nil)))

(defun sage-test--skip-unless-ollama ()
  "Skip test if Ollama is not available."
  (unless (sage-test--ollama-available-p)
    (ert-skip "Ollama not available on localhost:11434")))

;;; Live API Tests

(ert-deftest sage-integration-test-ollama-connection ()
  "Test that we can connect to Ollama."
  (sage-test--skip-unless-ollama)
  (should (sage-test--ollama-available-p)))

(ert-deftest sage-integration-test-ollama-simple-prompt ()
  "Test a simple prompt to Ollama."
  (sage-test--skip-unless-ollama)
  (let ((sage-provider 'ollama)
        (sage-model "llama3.2:1b")
        (sage-ollama-host "http://localhost:11434")
        (response nil))
    ;; Format a simple request
    (let* ((messages `(((role . "user")
                        (content . "Reply with only the word 'hello'"))))
           (body (sage--format-request messages nil)))
      (should body)
      (should (stringp (json-encode body))))))

(ert-deftest sage-integration-test-ollama-format-request ()
  "Test request formatting for Ollama."
  (sage-test--skip-unless-ollama)
  (let ((sage-provider 'ollama)
        (sage-model "llama3.2:1b"))
    (let* ((messages `(((role . "user") (content . "test"))))
           (body (sage--format-request messages nil)))
      (should (assoc 'model body))
      (should (string= (alist-get 'model body) "llama3.2:1b"))
      (should (assoc 'messages body)))))

(ert-deftest sage-integration-test-ollama-live-response ()
  "Test getting an actual response from Ollama (slow)."
  :tags '(:slow :live)
  (sage-test--skip-unless-ollama)
  (let ((sage-provider 'ollama)
        (sage-model "llama3.2:1b")
        (sage-ollama-host "http://localhost:11434")
        (sage-conversation nil)
        (result nil))
    ;; This is a synchronous test - may take a few seconds
    (let* ((messages `(((role . "user")
                        (content . "What is 2+2? Reply with just the number."))))
           (body (sage--format-request messages nil))
           (url (concat sage-ollama-host "/api/chat"))
           (url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode body) 'utf-8))
           (response-buffer (url-retrieve-synchronously url t nil 60)))
      (when response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (setq result (buffer-substring-no-properties (point) (point-max))))
          (kill-buffer))))
    (should result)
    (should (stringp result))
    ;; Response should contain "4" somewhere
    (should (string-match-p "4" result))))

(provide 'sage-integration-test)
;;; sage-integration-test.el ends here
