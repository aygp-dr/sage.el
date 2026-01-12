;;; sage-emacs.el --- Emacs-native integrations for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (sage "0.1.0"))
;; Keywords: ai, tools, llm, org, dired
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs-native integrations for sage, providing deep integration
;; with Org mode, buffer operations, Dired, and prog-mode.
;;
;; Features:
;; - Org mode: Send subtrees, source blocks, insert responses
;; - Buffer integration: Send buffer, region, defun
;; - Dired integration: Summarize marked files
;; - Prog mode: Explain errors, suggest fixes
;; - Minor mode with convenient keybindings
;;
;; Usage:
;;   (require 'sage-emacs)
;;   (sage-mode 1)  ; Enable minor mode globally
;;
;; Or with use-package:
;;   (use-package sage-emacs
;;     :after sage
;;     :config
;;     (global-sage-mode 1))

;;; Code:

(require 'sage)
(require 'org nil t)
(require 'dired nil t)

;; Declare functions from org-element
(declare-function org-element-type "org-element" (element))
(declare-function org-element-property "org-element" (property element))

;;; Customization

(defgroup sage-emacs nil
  "Emacs-native integrations for sage."
  :group 'sage
  :prefix "sage-emacs-")

(defcustom sage-emacs-org-ai-block-type "ai"
  "Type of org block to use for AI interactions.
Common values: \"ai\", \"gemini\", \"llm\"."
  :type 'string
  :group 'sage-emacs)

(defcustom sage-emacs-insert-as-comment nil
  "When non-nil, insert AI responses as comments in code buffers."
  :type 'boolean
  :group 'sage-emacs)

(defcustom sage-emacs-context-lines 5
  "Number of lines of context to include around point for error explanations."
  :type 'integer
  :group 'sage-emacs)

(defcustom sage-emacs-auto-format-response t
  "When non-nil, automatically format responses based on major mode."
  :type 'boolean
  :group 'sage-emacs)

;;; Org Mode Integration

(defun sage-org-send-subtree ()
  "Send current org subtree as context to sage."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (_begin (org-element-property :begin element))
           (end (org-element-property :end element))
           (content (buffer-substring-no-properties (point) end))
           (heading (org-element-property :raw-value element)))
      (sage)
      (with-current-buffer sage-buffer-name
        (goto-char (point-max))
        (insert (format "Context from org subtree '%s':\n\n%s\n\nWhat would you like to know about this?"
                        heading content))
        (sage-send-input)))))

(defun sage-org-send-src-block ()
  "Send current org source block to sage."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (unless (eq type 'src-block)
      (user-error "Point is not in a source block"))
    (let* ((lang (org-element-property :language element))
           (code (org-element-property :value element))
           (prompt (read-string "Question about this code: "
                               "Explain this code")))
      (sage)
      (with-current-buffer sage-buffer-name
        (goto-char (point-max))
        (insert (format "%s\n\nLanguage: %s\n\n```%s\n%s```"
                        prompt lang lang code))
        (sage-send-input)))))

(defun sage-org-insert-response (response)
  "Insert RESPONSE as an org block at point.
Creates a #+begin_ai...#+end_ai block with the response."
  (interactive
   (list (or sage-last-response
             (read-string "Response to insert: "))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((block-type sage-emacs-org-ai-block-type))
    (insert (format "#+begin_%s\n%s\n#+end_%s\n"
                    block-type
                    (or response "")
                    block-type))))

(defun sage-org-process-ai-blocks ()
  "Process all #+begin_ai blocks in current buffer.
Sends content to AI and inserts response after the block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (goto-char (point-min))
    (let ((block-re (format "^[ \t]*#\\+begin_%s\\b"
                           (regexp-quote sage-emacs-org-ai-block-type)))
          (case-fold-search t))
      (while (re-search-forward block-re nil t)
        (let* ((element (org-element-at-point))
               (_begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (content (org-element-property :value element)))
          (when content
            (goto-char end)
            (insert "\n#+begin_example\n")
            (insert "[Processing...]\n")
            (insert "#+end_example\n")
            (sage-exec content)))))))

;;; Buffer Integration

(defun sage-send-buffer ()
  "Send entire current buffer to sage."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (mode-name (symbol-name major-mode))
        (filename (or (buffer-file-name) (buffer-name))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "File: %s\nMode: %s\n\n```\n%s\n```\n\nWhat would you like to know?"
                      filename mode-name content))
      (sage-send-input))))

(defun sage-send-region-improved (start end &optional prompt)
  "Send region from START to END to sage with optional PROMPT.
If PROMPT is nil, asks for it interactively."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (mode-name (symbol-name major-mode))
        (question (or prompt
                     (read-string "Question about this code: "
                                 "Explain this code"))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nMode: %s\n\n```\n%s\n```"
                      question mode-name text))
      (sage-send-input))))

(defun sage-send-defun ()
  "Send current function definition to sage."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (unless bounds
      (user-error "No function definition at point"))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (sage-send-region-improved start end "Explain this function"))))

(defun sage-insert-response ()
  "Insert last AI response at point.
Formats the response appropriately based on major mode."
  (interactive)
  (unless sage-last-response
    (user-error "No previous response to insert"))
  (let ((response sage-last-response))
    (cond
     ;; Org mode: insert as block
     ((derived-mode-p 'org-mode)
      (sage-org-insert-response response))
     ;; Code modes: optionally insert as comment
     ((and (derived-mode-p 'prog-mode)
           sage-emacs-insert-as-comment)
      (sage--insert-as-comment response))
     ;; Default: insert as-is
     (t
      (insert response)))))

(defun sage--insert-as-comment (text)
  "Insert TEXT as a comment in current buffer.
Uses the appropriate comment syntax for the major mode."
  (let ((comment-start (or comment-start "# "))
        (comment-end (or comment-end "")))
    (insert comment-start)
    (let ((start (point)))
      (insert text)
      (comment-region start (point)))
    (insert comment-end "\n")))

;;; Dired Integration

(defun sage-dired-summarize ()
  "Summarize marked files in Dired using sage."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in dired-mode"))
  (let ((files (dired-get-marked-files)))
    (unless files
      (user-error "No files marked"))
    (let ((file-list (mapconcat #'identity files "\n"))
          (file-count (length files))
          (total-size 0)
          (contents ""))
      ;; Calculate total size and read text files
      (dolist (file files)
        (when (file-regular-p file)
          (setq total-size (+ total-size (file-attribute-size
                                          (file-attributes file))))
          ;; Read text files (up to 10KB each)
          (when (and (not (file-directory-p file))
                     (< (file-attribute-size (file-attributes file)) 10240))
            (ignore-errors
              (setq contents (concat contents
                                   (format "\n\n=== %s ===\n" file)
                                   (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string))))))))
      (sage)
      (with-current-buffer sage-buffer-name
        (goto-char (point-max))
        (insert (format "Summarize these %d files (total size: %s):\n\n%s\n%s"
                        file-count
                        (file-size-human-readable total-size)
                        file-list
                        (if (> (length contents) 0)
                            (format "\nContents:\n%s" contents)
                          "")))
        (sage-send-input)))))

(defun sage-dired-describe-file ()
  "Describe file at point in Dired."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in dired-mode"))
  (let ((file (dired-get-filename)))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "Describe this file: %s\n\nFile info:\n" file))
      (insert (format "- Size: %s\n"
                      (file-size-human-readable
                       (file-attribute-size (file-attributes file)))))
      (insert (format "- Type: %s\n"
                      (if (file-directory-p file) "directory" "file")))
      (when (file-regular-p file)
        (insert (format "\nContents:\n```\n%s\n```"
                        (with-temp-buffer
                          (insert-file-contents file nil 0 1024)
                          (buffer-string)))))
      (sage-send-input))))

;;; Prog Mode Integration

(defun sage-explain-error ()
  "Explain compilation error at point using context."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (user-error "Not in a programming mode"))
  (let* ((line-num (line-number-at-pos))
         (context-start (max (point-min)
                            (save-excursion
                              (forward-line (- sage-emacs-context-lines))
                              (point))))
         (context-end (min (point-max)
                          (save-excursion
                            (forward-line sage-emacs-context-lines)
                            (point))))
         (context (buffer-substring-no-properties context-start context-end))
         (error-line (thing-at-point 'line t))
         (mode-name (symbol-name major-mode))
         (filename (or (buffer-file-name) (buffer-name))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this error in %s (file: %s, line %d):\n\n"
                      mode-name filename line-num))
      (insert (format "Error line:\n%s\n\n" error-line))
      (insert (format "Context (lines %d-%d):\n```%s\n%s\n```"
                      (- line-num sage-emacs-context-lines)
                      (+ line-num sage-emacs-context-lines)
                      (sage--lang-for-mode)
                      context))
      (sage-send-input))))

(defun sage-suggest-fix ()
  "Suggest a fix for error at point."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (user-error "Not in a programming mode"))
  (let* ((line-num (line-number-at-pos))
         (context-start (max (point-min)
                            (save-excursion
                              (forward-line (- sage-emacs-context-lines))
                              (point))))
         (context-end (min (point-max)
                          (save-excursion
                            (forward-line sage-emacs-context-lines)
                            (point))))
         (context (buffer-substring-no-properties context-start context-end))
         (mode-name (symbol-name major-mode))
         (filename (or (buffer-file-name) (buffer-name))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "Suggest a fix for the code in %s (file: %s, line %d):\n\n"
                      mode-name filename line-num))
      (insert (format "Context:\n```%s\n%s\n```\n\n"
                      (sage--lang-for-mode)
                      context))
      (insert "Please suggest a fix and explain the issue.")
      (sage-send-input))))

(defun sage-explain-at-point ()
  "Explain code or symbol at point."
  (interactive)
  (let ((thing (cond
                ;; Try symbol first
                ((thing-at-point 'symbol)
                 (thing-at-point 'symbol t))
                ;; Then sentence
                ((thing-at-point 'sentence)
                 (thing-at-point 'sentence t))
                ;; Then line
                (t (thing-at-point 'line t)))))
    (unless thing
      (user-error "Nothing at point to explain"))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this in the context of %s:\n\n%s"
                      (symbol-name major-mode)
                      thing))
      (sage-send-input))))

(defun sage--lang-for-mode ()
  "Get syntax highlighting language for current major mode."
  (let ((mode (symbol-name major-mode)))
    (cond
     ((string-match "emacs-lisp" mode) "elisp")
     ((string-match "lisp" mode) "lisp")
     ((string-match "python" mode) "python")
     ((string-match "rust" mode) "rust")
     ((string-match "javascript\\|js" mode) "javascript")
     ((string-match "typescript\\|ts" mode) "typescript")
     ((string-match "c-mode" mode) "c")
     ((string-match "c++" mode) "cpp")
     ((string-match "java" mode) "java")
     ((string-match "ruby" mode) "ruby")
     ((string-match "go" mode) "go")
     ((string-match "shell\\|sh" mode) "bash")
     (t ""))))

;;; Compilation Mode Integration

(defun sage-explain-compilation-error ()
  "Explain error from compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "Not in compilation-mode"))
  (let ((error-msg (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this compilation error:\n\n%s" error-msg))
      (sage-send-input))))

;;; Interactive Query Functions

(defun sage-ask-about-buffer ()
  "Ask a question about the current buffer."
  (interactive)
  (let ((question (read-string "Ask about this buffer: "))
        (content (buffer-substring-no-properties (point-min) (point-max)))
        (filename (or (buffer-file-name) (buffer-name))))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nFile: %s\n\n```\n%s\n```"
                      question filename content))
      (sage-send-input))))

(defun sage-ask-about-project ()
  "Ask a question about the current project."
  (interactive)
  (let ((question (read-string "Ask about this project: "))
        (project-root (or (and (fboundp 'project-root)
                              (project-root (project-current)))
                         default-directory)))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nProject root: %s\n\n"
                      question project-root))
      (insert "Context: Use tools to explore the project structure and files.")
      (sage-send-input))))

;;; Minor Mode

(defvar sage-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Global bindings (C-c C-g prefix)
    (define-key map (kbd "C-c C-g g") #'sage-send-region-improved)
    (define-key map (kbd "C-c C-g b") #'sage-send-buffer)
    (define-key map (kbd "C-c C-g d") #'sage-send-defun)
    (define-key map (kbd "C-c C-g r") #'sage)
    (define-key map (kbd "C-c C-g e") #'sage-explain-at-point)
    (define-key map (kbd "C-c C-g i") #'sage-insert-response)
    (define-key map (kbd "C-c C-g a") #'sage-ask-about-buffer)
    (define-key map (kbd "C-c C-g p") #'sage-ask-about-project)

    ;; Org mode specific
    (define-key map (kbd "C-c C-g o s") #'sage-org-send-subtree)
    (define-key map (kbd "C-c C-g o c") #'sage-org-send-src-block)
    (define-key map (kbd "C-c C-g o i") #'sage-org-insert-response)
    (define-key map (kbd "C-c C-g o p") #'sage-org-process-ai-blocks)

    ;; Prog mode specific
    (define-key map (kbd "C-c C-g x") #'sage-explain-error)
    (define-key map (kbd "C-c C-g f") #'sage-suggest-fix)

    map)
  "Keymap for sage-mode.")

;;;###autoload
(define-minor-mode sage-mode
  "Minor mode for sage Emacs integrations.

Provides convenient keybindings for AI-assisted coding:

\\{sage-mode-map}"
  :lighter " GeminiREPL"
  :keymap sage-mode-map
  :group 'sage-emacs)

;;;###autoload
(define-globalized-minor-mode global-sage-mode
  sage-mode
  (lambda () (sage-mode 1))
  :group 'sage-emacs)

;;; Dired Mode Integration

(defun sage-emacs-dired-setup ()
  "Setup sage bindings in dired-mode."
  (when (derived-mode-p 'dired-mode)
    (local-set-key (kbd "C-c C-g s") #'sage-dired-summarize)
    (local-set-key (kbd "C-c C-g d") #'sage-dired-describe-file)))

(add-hook 'dired-mode-hook #'sage-emacs-dired-setup)

;;; Compilation Mode Integration

(defun sage-emacs-compilation-setup ()
  "Setup sage bindings in compilation-mode."
  (when (derived-mode-p 'compilation-mode)
    (local-set-key (kbd "C-c C-g e") #'sage-explain-compilation-error)))

(add-hook 'compilation-mode-hook #'sage-emacs-compilation-setup)

;;; Response Capture

(defun sage-emacs-capture-response (response)
  "Capture RESPONSE for later insertion.
This is called internally by sage."
  (setq sage-last-response response))

;; Advice to capture responses
(advice-add 'sage--handle-response :after
            (lambda (response)
              (when (eq (alist-get 'type response) 'text)
                (sage-emacs-capture-response
                 (alist-get 'content response)))))

(provide 'sage-emacs)
;;; sage-emacs.el ends here
