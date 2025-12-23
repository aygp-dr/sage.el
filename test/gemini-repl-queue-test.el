;;; gemini-repl-queue-test.el --- Tests for gemini-repl-queue -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Tests for the gemini-repl-queue file-based queue system.

;;; Code:

(require 'ert)
(require 'gemini-repl-queue)

;;; Test Utilities

(defvar gemini-repl-queue-test-dir nil
  "Temporary directory for tests.")

(defun gemini-repl-queue-test-setup ()
  "Set up test environment."
  (setq gemini-repl-queue-test-dir
        (make-temp-file "gemini-repl-queue-test-" t))
  (setq gemini-repl-queue-directory gemini-repl-queue-test-dir)
  (gemini-repl-queue--ensure-directories))

(defun gemini-repl-queue-test-teardown ()
  "Clean up test environment."
  (when (and gemini-repl-queue-test-dir
             (file-directory-p gemini-repl-queue-test-dir))
    (delete-directory gemini-repl-queue-test-dir t))
  (setq gemini-repl-queue-test-dir nil))

;;; Tests

(ert-deftest gemini-repl-queue-test-directory-creation ()
  "Test that queue directories are created."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (progn
        (should (file-directory-p gemini-repl-queue--input-dir))
        (should (file-directory-p gemini-repl-queue--output-dir))
        (should (file-directory-p gemini-repl-queue--archive-dir)))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-submit-request ()
  "Test submitting a request."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (let ((id (gemini-repl-queue-submit 'ping "test message")))
        (should (stringp id))
        (let ((file (expand-file-name (format "%s.json" id) gemini-repl-queue--input-dir)))
          (should (file-exists-p file))
          (let ((request (gemini-repl-queue--read-json file)))
            (should (string= (alist-get 'id request) id))
            (should (string= (alist-get 'type request) "ping"))
            (should (string= (alist-get 'content request) "test message")))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-respond ()
  "Test writing a response."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (let ((request-id "test-request-123"))
        (gemini-repl-queue-respond request-id 'success "response content")
        (let ((file (expand-file-name (format "%s.json" request-id) gemini-repl-queue--output-dir)))
          (should (file-exists-p file))
          (let ((response (gemini-repl-queue--read-json file)))
            (should (string= (alist-get 'request_id response) request-id))
            (should (string= (alist-get 'status response) "success"))
            (should (string= (alist-get 'content response) "response content")))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-poll ()
  "Test polling for requests."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (progn
        ;; No requests initially
        (should-not (gemini-repl-queue-poll))

        ;; Submit a request
        (let ((id (gemini-repl-queue-submit 'ping "test")))
          (sleep-for 0.1) ; Ensure file is written
          (let ((request (gemini-repl-queue-poll)))
            (should request)
            (should (string= (alist-get 'id request) id)))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-archive ()
  "Test archiving requests."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (let ((id (gemini-repl-queue-submit 'ping "test")))
        (gemini-repl-queue-respond id 'success "pong")
        (sleep-for 0.1)
        (gemini-repl-queue-archive id)

        ;; Input file should be moved
        (should-not (file-exists-p
                     (expand-file-name (format "%s.json" id) gemini-repl-queue--input-dir)))

        ;; Archive file should exist
        (should (file-exists-p
                 (expand-file-name (format "%s.json" id) gemini-repl-queue--archive-dir))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-ping-handler ()
  "Test ping request handler."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (let* ((request `((id . "test-123")
                       (type . "ping")
                       (content . "hello")
                       (created_at . "2024-01-01T00:00:00+0000")))
             (result (gemini-repl-queue--ping-handler request)))
        (should (eq (car result) 'success))
        (should (string-match "pong:" (cdr result))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-json-roundtrip ()
  "Test JSON encoding/decoding."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (let* ((data `((id . "test-123")
                    (type . "ping")
                    (content . "test message")
                    (context . ((key1 . "value1")
                              (key2 . 42)))))
             (file (expand-file-name "test.json" gemini-repl-queue-test-dir)))
        (gemini-repl-queue--write-json file data)
        (let ((read-data (gemini-repl-queue--read-json file)))
          (should (string= (alist-get 'id read-data) "test-123"))
          (should (string= (alist-get 'type read-data) "ping"))
          (should (string= (alist-get 'content read-data) "test message"))))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-id-generation ()
  "Test unique ID generation."
  (let ((id1 (gemini-repl-queue--generate-id))
        (id2 (gemini-repl-queue--generate-id)))
    (should (stringp id1))
    (should (stringp id2))
    (should-not (string= id1 id2))))

(ert-deftest gemini-repl-queue-test-timestamp ()
  "Test timestamp generation."
  (let ((ts (gemini-repl-queue--timestamp)))
    (should (stringp ts))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" ts))))

(ert-deftest gemini-repl-queue-test-list-requests ()
  "Test listing requests."
  (gemini-repl-queue-test-setup)
  (unwind-protect
      (progn
        ;; Submit multiple requests
        (gemini-repl-queue-submit 'ping "request1")
        (sleep-for 0.1)
        (gemini-repl-queue-submit 'ping "request2")
        (sleep-for 0.1)
        (gemini-repl-queue-submit 'ping "request3")
        (sleep-for 0.1)

        (let ((files (gemini-repl-queue--list-requests gemini-repl-queue--input-dir)))
          (should (= (length files) 3))
          ;; Should be sorted by modification time (newest first)
          (should files)))
    (gemini-repl-queue-test-teardown)))

(ert-deftest gemini-repl-queue-test-handler-registration ()
  "Test custom handler registration."
  (let ((handler-called nil)
        (test-handler (lambda (req)
                       (setq handler-called t)
                       (cons 'success "custom response"))))
    (gemini-repl-queue-register-handler 'custom test-handler)

    (let ((request `((id . "test")
                    (type . "custom")
                    (content . "test"))))
      (let ((result (funcall (gethash "custom" gemini-repl-queue--request-handlers) request)))
        (should handler-called)
        (should (eq (car result) 'success))
        (should (string= (cdr result) "custom response"))))))

(provide 'gemini-repl-queue-test)
;;; gemini-repl-queue-test.el ends here
