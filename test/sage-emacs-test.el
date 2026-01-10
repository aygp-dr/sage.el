;;; sage-emacs-test.el --- Tests for sage-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sage-emacs package.

;;; Code:

(require 'ert)
(require 'sage-emacs)

;;; Language Detection Tests

(ert-deftest test-lang-for-mode ()
  "Test language detection for syntax highlighting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (string= (sage--lang-for-mode) "elisp")))
  (with-temp-buffer
    (python-mode)
    (should (string= (sage--lang-for-mode) "python"))))

;;; Org Mode Tests

(ert-deftest test-org-ai-block-type ()
  "Test org AI block type customization."
  (let ((sage-emacs-org-ai-block-type "test"))
    (should (string= sage-emacs-org-ai-block-type "test"))))

(ert-deftest test-org-insert-response ()
  "Test inserting response as org block."
  (with-temp-buffer
    (org-mode)
    (let ((sage-emacs-org-ai-block-type "ai"))
      (sage-org-insert-response "Test response")
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
    (sage--insert-as-comment "Test comment")
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
    (sage-mode 1)
    (should sage-mode)
    (sage-mode -1)
    (should-not sage-mode)))

(ert-deftest test-keybindings-exist ()
  "Test that keybindings are properly defined."
  (with-temp-buffer
    (sage-mode 1)
    (should (keymapp sage-mode-map))
    (should (lookup-key sage-mode-map (kbd "C-c C-g g")))
    (should (lookup-key sage-mode-map (kbd "C-c C-g b")))
    (should (lookup-key sage-mode-map (kbd "C-c C-g r")))
    (should (lookup-key sage-mode-map (kbd "C-c C-g e")))))

;;; Customization Tests

(ert-deftest test-customization-defaults ()
  "Test default values for customizations."
  (should (string= sage-emacs-org-ai-block-type "ai"))
  (should (null sage-emacs-insert-as-comment))
  (should (= sage-emacs-context-lines 5))
  (should sage-emacs-auto-format-response))

;;; Response Capture Tests

(ert-deftest test-response-capture ()
  "Test that responses are properly captured."
  (let ((sage-last-response nil))
    (sage-emacs-capture-response "Test response")
    (should (string= sage-last-response "Test response"))))

(provide 'sage-emacs-test)
;;; sage-emacs-test.el ends here
