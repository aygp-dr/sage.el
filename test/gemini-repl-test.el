;;; gemini-repl-test.el --- Tests for gemini-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for gemini-repl package.

;;; Code:

(require 'ert)
(require 'gemini-repl)

;;; Path Safety Tests

(ert-deftest test-safe-path-simple ()
  "Test that simple relative paths are safe."
  (let ((gemini-repl-workspace "/tmp/test-workspace"))
    (should (gemini-repl--safe-path-p "file.txt"))
    (should (gemini-repl--safe-path-p "src/main.el"))
    (should (gemini-repl--safe-path-p "a/b/c/d.txt"))))

(ert-deftest test-unsafe-path-traversal ()
  "Test that path traversal is rejected."
  (let ((gemini-repl-workspace "/tmp/test-workspace"))
    (should-not (gemini-repl--safe-path-p "../file.txt"))
    (should-not (gemini-repl--safe-path-p "a/../../../etc/passwd"))
    (should-not (gemini-repl--safe-path-p "foo/bar/../../.."))))

(ert-deftest test-unsafe-path-sensitive ()
  "Test that sensitive files are rejected."
  (let ((gemini-repl-workspace "/tmp/test-workspace"))
    (should-not (gemini-repl--safe-path-p ".env"))
    (should-not (gemini-repl--safe-path-p ".env.local"))
    (should-not (gemini-repl--safe-path-p ".git/config"))
    (should-not (gemini-repl--safe-path-p ".ssh/id_rsa"))))

;;; Provider Tests

(ert-deftest test-get-model-defaults ()
  "Test default model selection."
  (let ((gemini-repl-model nil))
    (let ((gemini-repl-provider 'gemini))
      (should (string= (gemini-repl--get-model) "gemini-2.0-flash-exp")))
    (let ((gemini-repl-provider 'ollama))
      (should (string= (gemini-repl--get-model) "llama3.2")))
    (let ((gemini-repl-provider 'openai))
      (should (string= (gemini-repl--get-model) "gpt-4o")))))

(ert-deftest test-get-model-override ()
  "Test model override."
  (let ((gemini-repl-model "custom-model")
        (gemini-repl-provider 'gemini))
    (should (string= (gemini-repl--get-model) "custom-model"))))

;;; Tool Tests

(ert-deftest test-tool-registration ()
  "Test tool registration."
  (let ((gemini-repl-tools nil))
    (gemini-repl-register-tool
     "test_tool"
     "A test tool"
     '((type . "object"))
     (lambda (_args) "result"))
    (should (= (length gemini-repl-tools) 1))
    (should (string= (alist-get 'name (car gemini-repl-tools)) "test_tool"))))

(ert-deftest test-safe-tools-list ()
  "Test that safe tools list contains expected tools."
  (should (member "read_file" gemini-repl-safe-tools))
  (should (member "git_status" gemini-repl-safe-tools))
  (should-not (member "write_file" gemini-repl-safe-tools)))

;;; Permission Tests

(ert-deftest test-permission-yolo-mode ()
  "Test that yolo mode allows all tools."
  (let ((gemini-repl-yolo-mode t))
    (should (gemini-repl--check-permission "write_file" nil))
    (should (gemini-repl--check-permission "dangerous_tool" nil))))

(ert-deftest test-permission-safe-tools ()
  "Test that safe tools are auto-allowed."
  (let ((gemini-repl-yolo-mode nil)
        (gemini-repl-confirm-safe-tools nil))
    (should (gemini-repl--check-permission "read_file" nil))
    (should (gemini-repl--check-permission "git_status" nil))))

(provide 'gemini-repl-test)
;;; gemini-repl-test.el ends here
