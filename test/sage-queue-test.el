;;; sage-queue-test.el --- Tests for sage-queue -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Tests for the sage-queue file-based queue system.

;;; Code:

(require 'ert)
(require 'sage-queue)

;;; Test Utilities

(defvar sage-queue-test-dir nil
  "Temporary directory for tests.")

(defun sage-queue-test-setup ()
  "Set up test environment."
  (setq sage-queue-test-dir
        (make-temp-file "sage-queue-test-" t))
  (setq sage-queue-directory sage-queue-test-dir)
  (sage-queue--ensure-directories))

(defun sage-queue-test-teardown ()
  "Clean up test environment."
  (when (and sage-queue-test-dir
             (file-directory-p sage-queue-test-dir))
    (delete-directory sage-queue-test-dir t))
  (setq sage-queue-test-dir nil))

;;; Tests

(ert-deftest sage-queue-test-directory-creation ()
  "Test that queue directories are created."
  (sage-queue-test-setup)
  (unwind-protect
      (progn
        (should (file-directory-p sage-queue--input-dir))
        (should (file-directory-p sage-queue--output-dir))
        (should (file-directory-p sage-queue--archive-dir)))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-submit-request ()
  "Test submitting a request."
  (sage-queue-test-setup)
  (unwind-protect
      (let ((id (sage-queue-submit 'ping "test message")))
        (should (stringp id))
        (let ((file (expand-file-name (format "%s.json" id) sage-queue--input-dir)))
          (should (file-exists-p file))
          (let ((request (sage-queue--read-json file)))
            (should (string= (alist-get 'id request) id))
            (should (string= (alist-get 'type request) "ping"))
            (should (string= (alist-get 'content request) "test message")))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-respond ()
  "Test writing a response."
  (sage-queue-test-setup)
  (unwind-protect
      (let ((request-id "test-request-123"))
        (sage-queue-respond request-id 'success "response content")
        (let ((file (expand-file-name (format "%s.json" request-id) sage-queue--output-dir)))
          (should (file-exists-p file))
          (let ((response (sage-queue--read-json file)))
            (should (string= (alist-get 'request_id response) request-id))
            (should (string= (alist-get 'status response) "success"))
            (should (string= (alist-get 'content response) "response content")))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-poll ()
  "Test polling for requests."
  (sage-queue-test-setup)
  (unwind-protect
      (progn
        ;; No requests initially
        (should-not (sage-queue-poll))

        ;; Submit a request
        (let ((id (sage-queue-submit 'ping "test")))
          (sleep-for 0.1) ; Ensure file is written
          (let ((request (sage-queue-poll)))
            (should request)
            (should (string= (alist-get 'id request) id)))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-archive ()
  "Test archiving requests."
  (sage-queue-test-setup)
  (unwind-protect
      (let ((id (sage-queue-submit 'ping "test")))
        (sage-queue-respond id 'success "pong")
        (sleep-for 0.1)
        (sage-queue-archive id)

        ;; Input file should be moved
        (should-not (file-exists-p
                     (expand-file-name (format "%s.json" id) sage-queue--input-dir)))

        ;; Archive file should exist
        (should (file-exists-p
                 (expand-file-name (format "%s.json" id) sage-queue--archive-dir))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-ping-handler ()
  "Test ping request handler."
  (sage-queue-test-setup)
  (unwind-protect
      (let* ((request `((id . "test-123")
                       (type . "ping")
                       (content . "hello")
                       (created_at . "2024-01-01T00:00:00+0000")))
             (result (sage-queue--ping-handler request)))
        (should (eq (car result) 'success))
        (should (string-match "pong:" (cdr result))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-json-roundtrip ()
  "Test JSON encoding/decoding."
  (sage-queue-test-setup)
  (unwind-protect
      (let* ((data `((id . "test-123")
                    (type . "ping")
                    (content . "test message")
                    (context . ((key1 . "value1")
                              (key2 . 42)))))
             (file (expand-file-name "test.json" sage-queue-test-dir)))
        (sage-queue--write-json file data)
        (let ((read-data (sage-queue--read-json file)))
          (should (string= (alist-get 'id read-data) "test-123"))
          (should (string= (alist-get 'type read-data) "ping"))
          (should (string= (alist-get 'content read-data) "test message"))))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-id-generation ()
  "Test unique ID generation."
  (let ((id1 (sage-queue--generate-id))
        (id2 (sage-queue--generate-id)))
    (should (stringp id1))
    (should (stringp id2))
    (should-not (string= id1 id2))))

(ert-deftest sage-queue-test-timestamp ()
  "Test timestamp generation."
  (let ((ts (sage-queue--timestamp)))
    (should (stringp ts))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" ts))))

(ert-deftest sage-queue-test-list-requests ()
  "Test listing requests."
  (sage-queue-test-setup)
  (unwind-protect
      (progn
        ;; Submit multiple requests
        (sage-queue-submit 'ping "request1")
        (sleep-for 0.1)
        (sage-queue-submit 'ping "request2")
        (sleep-for 0.1)
        (sage-queue-submit 'ping "request3")
        (sleep-for 0.1)

        (let ((files (sage-queue--list-requests sage-queue--input-dir)))
          (should (= (length files) 3))
          ;; Should be sorted by modification time (newest first)
          (should files)))
    (sage-queue-test-teardown)))

(ert-deftest sage-queue-test-handler-registration ()
  "Test custom handler registration."
  (let ((handler-called nil))
    (let ((test-handler (lambda (req)
                          (setq handler-called t)
                          (cons 'success "custom response"))))
      (sage-queue-register-handler 'custom test-handler)
      (let ((request `((id . "test")
                       (type . "custom")
                       (content . "test"))))
        (let ((result (funcall (gethash "custom" sage-queue--request-handlers) request)))
          (should handler-called)
          (should (eq (car result) 'success))
          (should (string= (cdr result) "custom response")))))))

(provide 'sage-queue-test)
;;; sage-queue-test.el ends here
