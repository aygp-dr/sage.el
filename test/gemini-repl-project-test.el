;;; gemini-repl-project-test.el --- Tests for gemini-repl-project -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for gemini-repl-project package.

;;; Code:

(require 'ert)
(require 'gemini-repl-project)

;;; Path Encoding Tests

(ert-deftest test-encode-path-simple ()
  "Test simple path encoding."
  (should (string= (gemini-repl-project-encode-path "/home/user/project")
                   "-home-user-project")))

(ert-deftest test-encode-path-complex ()
  "Test complex path encoding."
  (should (string= (gemini-repl-project-encode-path "/home/user/my-project/src")
                   "-home-user-my-project-src")))

(ert-deftest test-encode-path-root ()
  "Test root path encoding."
  (should (string= (gemini-repl-project-encode-path "/")
                   "-")))

;;; Message Creation Tests

(ert-deftest test-create-message ()
  "Test message creation."
  (let ((msg (gemini-repl-project--create-message "user" "Hello")))
    (should (string= (plist-get msg :role) "user"))
    (should (string= (plist-get msg :content) "Hello"))
    (should (plist-get msg :timestamp))))

;;; Metadata Tests

(ert-deftest test-create-metadata ()
  "Test metadata creation."
  (let ((meta (gemini-repl-project--create-metadata "/tmp/test")))
    (should (string= (plist-get meta :project_dir) "/tmp/test"))
    (should (plist-get meta :created_at))
    (should (plist-get meta :updated_at))
    (should (= (plist-get meta :message_count) 0))))

;;; File Path Tests

(ert-deftest test-storage-dir ()
  "Test storage directory generation."
  (let ((gemini-repl-project-directory "/tmp/test-storage"))
    (should (string-match-p "test-storage.*-home-user-project"
                           (gemini-repl-project--storage-dir "/home/user/project")))))

(ert-deftest test-conversation-file ()
  "Test conversation file path."
  (let ((gemini-repl-project-directory "/tmp/test-storage"))
    (should (string-match-p "conversation\\.jsonl$"
                           (gemini-repl-project--conversation-file "/home/user/project")))))

(ert-deftest test-metadata-file ()
  "Test metadata file path."
  (let ((gemini-repl-project-directory "/tmp/test-storage"))
    (should (string-match-p "metadata\\.json$"
                           (gemini-repl-project--metadata-file "/home/user/project")))))

(ert-deftest test-history-dir ()
  "Test history directory path."
  (let ((gemini-repl-project-directory "/tmp/test-storage"))
    (should (string-match-p "history$"
                           (gemini-repl-project--history-dir "/home/user/project")))))

;;; Integration Tests

(ert-deftest test-project-dir-detection ()
  "Test project directory detection."
  ;; Should return current directory if no project
  (let ((default-directory "/tmp/"))
    (should (file-directory-p (gemini-repl-project-dir)))))

(ert-deftest test-append-message-plist ()
  "Test appending message as plist."
  (let ((gemini-repl-project--current-dir "/tmp/test-project")
        (gemini-repl-project-directory "/tmp/test-storage")
        (gemini-repl-project-auto-save nil)
        (gemini-repl-project--current-conversation nil))
    (let ((msg (gemini-repl-project-append
                '(:role "user" :content "Test message"))))
      (should (string= (plist-get msg :role) "user"))
      (should (string= (plist-get msg :content) "Test message"))
      (should (plist-get msg :timestamp))
      (should (= (length gemini-repl-project--current-conversation) 1)))))

(ert-deftest test-append-message-list ()
  "Test appending message as list."
  (let ((gemini-repl-project--current-dir "/tmp/test-project")
        (gemini-repl-project-directory "/tmp/test-storage")
        (gemini-repl-project-auto-save nil)
        (gemini-repl-project--current-conversation nil))
    (let ((msg (gemini-repl-project-append '("assistant" "Response"))))
      (should (string= (plist-get msg :role) "assistant"))
      (should (string= (plist-get msg :content) "Response"))
      (should (= (length gemini-repl-project--current-conversation) 1)))))

(ert-deftest test-get-set-conversation ()
  "Test getting and setting conversation."
  (let ((gemini-repl-project--current-dir "/tmp/test-project")
        (gemini-repl-project-directory "/tmp/test-storage")
        (gemini-repl-project-auto-save nil)
        (gemini-repl-project--current-conversation nil))
    (let ((msgs (list
                 (gemini-repl-project--create-message "user" "Hello")
                 (gemini-repl-project--create-message "assistant" "Hi"))))
      (gemini-repl-project--set-conversation msgs)
      (let ((retrieved (gemini-repl-project--get-conversation)))
        (should (= (length retrieved) 2))
        (should (string= (plist-get (car retrieved) :content) "Hello"))
        (should (string= (plist-get (cadr retrieved) :content) "Hi"))))))

(ert-deftest test-metadata-get-set ()
  "Test metadata get/set operations."
  (let ((gemini-repl-project--current-dir "/tmp/test-project")
        (gemini-repl-project-directory "/tmp/test-storage")
        (gemini-repl-project--current-metadata nil))
    ;; Set a value
    (gemini-repl-project-metadata :test_key "test_value")
    ;; Get the value
    (should (string= (gemini-repl-project-metadata :test_key) "test_value"))
    ;; Get entire metadata
    (let ((meta (gemini-repl-project-metadata)))
      (should (plist-member meta :test_key)))))

;;; Cleanup

(defun gemini-repl-project-test--cleanup ()
  "Clean up test files and directories."
  (when (file-directory-p "/tmp/test-storage")
    (delete-directory "/tmp/test-storage" t)))

;; Clean up after all tests
(add-hook 'ert-after-test-functions
          (lambda (_test _result)
            (gemini-repl-project-test--cleanup)))

(provide 'gemini-repl-project-test)
;;; gemini-repl-project-test.el ends here
