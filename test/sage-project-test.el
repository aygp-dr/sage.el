;;; sage-project-test.el --- Tests for sage-project -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sage-project package.

;;; Code:

(require 'ert)
(require 'sage-project)

;;; Path Encoding Tests

(ert-deftest test-encode-path-simple ()
  "Test simple path encoding."
  (should (string= (sage-project-encode-path "/home/user/project")
                   "-home-user-project")))

(ert-deftest test-encode-path-complex ()
  "Test complex path encoding."
  (should (string= (sage-project-encode-path "/home/user/my-project/src")
                   "-home-user-my-project-src")))

(ert-deftest test-encode-path-root ()
  "Test root path encoding."
  (should (string= (sage-project-encode-path "/")
                   "-")))

;;; Message Creation Tests

(ert-deftest test-create-message ()
  "Test message creation."
  (let ((msg (sage-project--create-message "user" "Hello")))
    (should (string= (plist-get msg :role) "user"))
    (should (string= (plist-get msg :content) "Hello"))
    (should (plist-get msg :timestamp))))

;;; Metadata Tests

(ert-deftest test-create-metadata ()
  "Test metadata creation."
  (let ((meta (sage-project--create-metadata "/tmp/test")))
    (should (string= (plist-get meta :project_dir) "/tmp/test"))
    (should (plist-get meta :created_at))
    (should (plist-get meta :updated_at))
    (should (= (plist-get meta :message_count) 0))))

;;; File Path Tests

(ert-deftest test-storage-dir ()
  "Test storage directory generation."
  (let ((sage-project-directory "/tmp/test-storage"))
    (should (string-match-p "test-storage.*-home-user-project"
                           (sage-project--storage-dir "/home/user/project")))))

(ert-deftest test-conversation-file ()
  "Test conversation file path."
  (let ((sage-project-directory "/tmp/test-storage"))
    (should (string-match-p "conversation\\.jsonl$"
                           (sage-project--conversation-file "/home/user/project")))))

(ert-deftest test-metadata-file ()
  "Test metadata file path."
  (let ((sage-project-directory "/tmp/test-storage"))
    (should (string-match-p "metadata\\.json$"
                           (sage-project--metadata-file "/home/user/project")))))

(ert-deftest test-history-dir ()
  "Test history directory path."
  (let ((sage-project-directory "/tmp/test-storage"))
    (should (string-match-p "history$"
                           (sage-project--history-dir "/home/user/project")))))

;;; Integration Tests

(ert-deftest test-project-dir-detection ()
  "Test project directory detection."
  ;; Should return current directory if no project
  (let ((default-directory "/tmp/"))
    (should (file-directory-p (sage-project-dir)))))

(ert-deftest test-append-message-plist ()
  "Test appending message as plist."
  (let ((sage-project--current-dir "/tmp/test-project")
        (sage-project-directory "/tmp/test-storage")
        (sage-project-auto-save nil)
        (sage-project--current-conversation nil))
    (let ((msg (sage-project-append
                '(:role "user" :content "Test message"))))
      (should (string= (plist-get msg :role) "user"))
      (should (string= (plist-get msg :content) "Test message"))
      (should (plist-get msg :timestamp))
      (should (= (length sage-project--current-conversation) 1)))))

(ert-deftest test-append-message-list ()
  "Test appending message as list."
  (let ((sage-project--current-dir "/tmp/test-project")
        (sage-project-directory "/tmp/test-storage")
        (sage-project-auto-save nil)
        (sage-project--current-conversation nil))
    (let ((msg (sage-project-append '("assistant" "Response"))))
      (should (string= (plist-get msg :role) "assistant"))
      (should (string= (plist-get msg :content) "Response"))
      (should (= (length sage-project--current-conversation) 1)))))

(ert-deftest test-get-set-conversation ()
  "Test getting and setting conversation."
  (let ((sage-project--current-dir "/tmp/test-project")
        (sage-project-directory "/tmp/test-storage")
        (sage-project-auto-save nil)
        (sage-project--current-conversation nil))
    (let ((msgs (list
                 (sage-project--create-message "user" "Hello")
                 (sage-project--create-message "assistant" "Hi"))))
      (sage-project--set-conversation msgs)
      (let ((retrieved (sage-project--get-conversation)))
        (should (= (length retrieved) 2))
        (should (string= (plist-get (car retrieved) :content) "Hello"))
        (should (string= (plist-get (cadr retrieved) :content) "Hi"))))))

(ert-deftest test-sage-project-get-conversation-public ()
  "Test public sage-project-get-conversation function exists and works."
  (let ((sage-project--current-dir "/tmp/test-project")
        (sage-project-directory "/tmp/test-storage")
        (sage-project-auto-save nil)
        (sage-project--current-conversation nil))
    ;; Verify function exists
    (should (fboundp 'sage-project-get-conversation))
    ;; Add some messages
    (sage-project-append '(:role "user" :content "First"))
    (sage-project-append '(:role "assistant" :content "Second"))
    ;; Verify get-conversation returns messages in chronological order
    (let ((conv (sage-project-get-conversation)))
      (should (= (length conv) 2))
      ;; First message should be "First" (chronological order)
      (should (string= (plist-get (car conv) :content) "First"))
      (should (string= (plist-get (cadr conv) :content) "Second")))))

(ert-deftest test-metadata-get-set ()
  "Test metadata get/set operations."
  (let ((sage-project--current-dir "/tmp/test-project")
        (sage-project-directory "/tmp/test-storage")
        (sage-project--current-metadata nil))
    ;; Set a value
    (sage-project-metadata :test_key "test_value")
    ;; Get the value
    (should (string= (sage-project-metadata :test_key) "test_value"))
    ;; Get entire metadata
    (let ((meta (sage-project-metadata)))
      (should (plist-member meta :test_key)))))

;;; Cleanup

(defun sage-project-test--cleanup ()
  "Clean up test files and directories."
  (when (file-directory-p "/tmp/test-storage")
    (delete-directory "/tmp/test-storage" t)))

;; Clean up after all tests
(add-hook 'ert-after-test-functions
          (lambda (_test _result)
            (sage-project-test--cleanup)))

(provide 'sage-project-test)
;;; sage-project-test.el ends here
