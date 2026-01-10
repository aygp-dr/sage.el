;;; sage-test.el --- Tests for sage -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sage package.

;;; Code:

(require 'ert)
(require 'sage)

;;; Path Safety Tests

(ert-deftest test-safe-path-simple ()
  "Test that simple relative paths are safe."
  (let ((sage-workspace "/tmp/test-workspace"))
    (should (sage--safe-path-p "file.txt"))
    (should (sage--safe-path-p "src/main.el"))
    (should (sage--safe-path-p "a/b/c/d.txt"))))

(ert-deftest test-unsafe-path-traversal ()
  "Test that path traversal is rejected."
  (let ((sage-workspace "/tmp/test-workspace"))
    (should-not (sage--safe-path-p "../file.txt"))
    (should-not (sage--safe-path-p "a/../../../etc/passwd"))
    (should-not (sage--safe-path-p "foo/bar/../../.."))))

(ert-deftest test-unsafe-path-sensitive ()
  "Test that sensitive files are rejected."
  (let ((sage-workspace "/tmp/test-workspace"))
    (should-not (sage--safe-path-p ".env"))
    (should-not (sage--safe-path-p ".env.local"))
    (should-not (sage--safe-path-p ".git/config"))
    (should-not (sage--safe-path-p ".ssh/id_rsa"))))

;;; Provider Tests

(ert-deftest test-get-model-defaults ()
  "Test default model selection."
  (let ((sage-model nil))
    (let ((sage-provider 'gemini))
      (should (string= (sage--get-model) "gemini-2.0-flash-exp")))
    (let ((sage-provider 'ollama))
      (should (string= (sage--get-model) "llama3.2")))
    (let ((sage-provider 'openai))
      (should (string= (sage--get-model) "gpt-4o")))))

(ert-deftest test-get-model-override ()
  "Test model override."
  (let ((sage-model "custom-model")
        (sage-provider 'gemini))
    (should (string= (sage--get-model) "custom-model"))))

;;; Tool Tests

(ert-deftest test-tool-registration ()
  "Test tool registration."
  (let ((sage-tools nil))
    (sage-register-tool
     "test_tool"
     "A test tool"
     '((type . "object"))
     (lambda (_args) "result"))
    (should (= (length sage-tools) 1))
    (should (string= (alist-get 'name (car sage-tools)) "test_tool"))))

(ert-deftest test-safe-tools-list ()
  "Test that safe tools list contains expected tools."
  (should (member "read_file" sage-safe-tools))
  (should (member "git_status" sage-safe-tools))
  (should-not (member "write_file" sage-safe-tools)))

;;; Permission Tests

(ert-deftest test-permission-yolo-mode ()
  "Test that yolo mode allows all tools."
  (let ((sage-yolo-mode t))
    (should (sage--check-permission "write_file" nil))
    (should (sage--check-permission "dangerous_tool" nil))))

(ert-deftest test-permission-safe-tools ()
  "Test that safe tools are auto-allowed."
  (let ((sage-yolo-mode nil)
        (sage-confirm-safe-tools nil))
    (should (sage--check-permission "read_file" nil))
    (should (sage--check-permission "git_status" nil))))

(provide 'sage-test)
;;; sage-test.el ends here
