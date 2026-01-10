;;; gemini-repl-emacs-test.el --- Tests for gemini-repl-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for gemini-repl-emacs package.

;;; Code:

(require 'ert)
(require 'gemini-repl-emacs)

;;; Language Detection Tests

(ert-deftest test-lang-for-mode ()
  "Test language detection for syntax highlighting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (string= (gemini-repl--lang-for-mode) "elisp")))
  (with-temp-buffer
    (python-mode)
    (should (string= (gemini-repl--lang-for-mode) "python"))))

;;; Org Mode Tests

(ert-deftest test-org-ai-block-type ()
  "Test org AI block type customization."
  (let ((gemini-repl-emacs-org-ai-block-type "test"))
    (should (string= gemini-repl-emacs-org-ai-block-type "test"))))

(ert-deftest test-org-insert-response ()
  "Test inserting response as org block."
  (with-temp-buffer
    (org-mode)
    (let ((gemini-repl-emacs-org-ai-block-type "ai"))
      (gemini-repl-org-insert-response "Test response")
      (should (string-match-p "^#\\+begin_ai" (buffer-string)))
      (should (string-match-p "Test response" (buffer-string)))
      (should (string-match-p "#\\+end_ai" (buffer-string))))))

;;; Buffer Integration Tests

(ert-deftest test-insert-as-comment ()
  "Test inserting text as comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq comment-start ";; ")
    (setq comment-end "")
    (gemini-repl--insert-as-comment "Test comment")
    (should (string-match-p ";; Test comment" (buffer-string)))))

;;; Mode Detection Tests

(ert-deftest test-prog-mode-detection ()
  "Test prog-mode derived mode detection."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (derived-mode-p 'prog-mode)))
  (with-temp-buffer
    (text-mode)
    (should-not (derived-mode-p 'prog-mode))))

;;; Minor Mode Tests

(ert-deftest test-minor-mode-activation ()
  "Test that minor mode can be activated."
  (with-temp-buffer
    (gemini-repl-mode 1)
    (should gemini-repl-mode)
    (gemini-repl-mode -1)
    (should-not gemini-repl-mode)))

(ert-deftest test-keybindings-exist ()
  "Test that keybindings are properly defined."
  (with-temp-buffer
    (gemini-repl-mode 1)
    (should (keymapp gemini-repl-mode-map))
    (should (lookup-key gemini-repl-mode-map (kbd "C-c C-g g")))
    (should (lookup-key gemini-repl-mode-map (kbd "C-c C-g b")))
    (should (lookup-key gemini-repl-mode-map (kbd "C-c C-g r")))
    (should (lookup-key gemini-repl-mode-map (kbd "C-c C-g e")))))

;;; Customization Tests

(ert-deftest test-customization-defaults ()
  "Test default values for customizations."
  (should (string= gemini-repl-emacs-org-ai-block-type "ai"))
  (should (null gemini-repl-emacs-insert-as-comment))
  (should (= gemini-repl-emacs-context-lines 5))
  (should gemini-repl-emacs-auto-format-response))

;;; Response Capture Tests

(ert-deftest test-response-capture ()
  "Test that responses are properly captured."
  (let ((gemini-repl-last-response nil))
    (gemini-repl-emacs-capture-response "Test response")
    (should (string= gemini-repl-last-response "Test response"))))

(provide 'gemini-repl-emacs-test)
;;; gemini-repl-emacs-test.el ends here
