;;; gemini-repl-emacs.el --- Emacs-native integrations for gemini-repl -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (gemini-repl "0.1.0"))
;; Keywords: ai, tools, llm, org, dired
;; URL: https://github.com/aygp-dr/gemini-repl-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs-native integrations for gemini-repl, providing deep integration
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
;;   (require 'gemini-repl-emacs)
;;   (gemini-repl-mode 1)  ; Enable minor mode globally
;;
;; Or with use-package:
;;   (use-package gemini-repl-emacs
;;     :after gemini-repl
;;     :config
;;     (global-gemini-repl-mode 1))

;;; Code:

(require 'gemini-repl)
(require 'org nil t)
(require 'dired nil t)

;;; Customization

(defgroup gemini-repl-emacs nil
  "Emacs-native integrations for gemini-repl."
  :group 'gemini-repl
  :prefix "gemini-repl-emacs-")

(defcustom gemini-repl-emacs-org-ai-block-type "ai"
  "Type of org block to use for AI interactions.
Common values: \"ai\", \"gemini\", \"llm\"."
  :type 'string
  :group 'gemini-repl-emacs)

(defcustom gemini-repl-emacs-insert-as-comment nil
  "When non-nil, insert AI responses as comments in code buffers."
  :type 'boolean
  :group 'gemini-repl-emacs)

(defcustom gemini-repl-emacs-context-lines 5
  "Number of lines of context to include around point for error explanations."
  :type 'integer
  :group 'gemini-repl-emacs)

(defcustom gemini-repl-emacs-auto-format-response t
  "When non-nil, automatically format responses based on major mode."
  :type 'boolean
  :group 'gemini-repl-emacs)

;;; Org Mode Integration

(defun gemini-repl-org-send-subtree ()
  "Send current org subtree as context to gemini-repl."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element))
           (content (buffer-substring-no-properties begin end))
           (heading (org-element-property :raw-value element)))
      (gemini-repl)
      (with-current-buffer gemini-repl-buffer-name
        (goto-char (point-max))
        (insert (format "Context from org subtree '%s':\n\n%s\n\nWhat would you like to know about this?"
                        heading content))
        (gemini-repl-send-input)))))

(defun gemini-repl-org-send-src-block ()
  "Send current org source block to gemini-repl."
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
      (gemini-repl)
      (with-current-buffer gemini-repl-buffer-name
        (goto-char (point-max))
        (insert (format "%s\n\nLanguage: %s\n\n```%s\n%s```"
                        prompt lang lang code))
        (gemini-repl-send-input)))))

(defun gemini-repl-org-insert-response (response)
  "Insert RESPONSE as an org block at point.
Creates a #+begin_ai...#+end_ai block with the response."
  (interactive
   (list (or gemini-repl-last-response
             (read-string "Response to insert: "))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((block-type gemini-repl-emacs-org-ai-block-type))
    (insert (format "#+begin_%s\n%s\n#+end_%s\n"
                    block-type
                    (or response "")
                    block-type))))

(defun gemini-repl-org-process-ai-blocks ()
  "Process all #+begin_ai blocks in current buffer.
Sends content to AI and inserts response after the block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (goto-char (point-min))
    (let ((block-re (format "^[ \t]*#\\+begin_%s\\b"
                           (regexp-quote gemini-repl-emacs-org-ai-block-type)))
          (case-fold-search t))
      (while (re-search-forward block-re nil t)
        (let* ((element (org-element-at-point))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element))
               (content (org-element-property :value element)))
          (when content
            (goto-char end)
            (insert "\n#+begin_example\n")
            (insert "[Processing...]\n")
            (insert "#+end_example\n")
            (gemini-repl-exec content)))))))

;;; Buffer Integration

(defun gemini-repl-send-buffer ()
  "Send entire current buffer to gemini-repl."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (mode-name (symbol-name major-mode))
        (filename (or (buffer-file-name) (buffer-name))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "File: %s\nMode: %s\n\n```\n%s\n```\n\nWhat would you like to know?"
                      filename mode-name content))
      (gemini-repl-send-input))))

(defun gemini-repl-send-region-improved (start end &optional prompt)
  "Send region from START to END to gemini-repl with optional PROMPT.
If PROMPT is nil, asks for it interactively."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (mode-name (symbol-name major-mode))
        (question (or prompt
                     (read-string "Question about this code: "
                                 "Explain this code"))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nMode: %s\n\n```\n%s\n```"
                      question mode-name text))
      (gemini-repl-send-input))))

(defun gemini-repl-send-defun ()
  "Send current function definition to gemini-repl."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (unless bounds
      (user-error "No function definition at point"))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (gemini-repl-send-region-improved start end "Explain this function"))))

(defun gemini-repl-insert-response ()
  "Insert last AI response at point.
Formats the response appropriately based on major mode."
  (interactive)
  (unless gemini-repl-last-response
    (user-error "No previous response to insert"))
  (let ((response gemini-repl-last-response))
    (cond
     ;; Org mode: insert as block
     ((derived-mode-p 'org-mode)
      (gemini-repl-org-insert-response response))
     ;; Code modes: optionally insert as comment
     ((and (derived-mode-p 'prog-mode)
           gemini-repl-emacs-insert-as-comment)
      (gemini-repl--insert-as-comment response))
     ;; Default: insert as-is
     (t
      (insert response)))))

(defun gemini-repl--insert-as-comment (text)
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

(defun gemini-repl-dired-summarize ()
  "Summarize marked files in Dired using gemini-repl."
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
      (gemini-repl)
      (with-current-buffer gemini-repl-buffer-name
        (goto-char (point-max))
        (insert (format "Summarize these %d files (total size: %s):\n\n%s\n%s"
                        file-count
                        (file-size-human-readable total-size)
                        file-list
                        (if (> (length contents) 0)
                            (format "\nContents:\n%s" contents)
                          "")))
        (gemini-repl-send-input)))))

(defun gemini-repl-dired-describe-file ()
  "Describe file at point in Dired."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in dired-mode"))
  (let ((file (dired-get-filename)))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
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
      (gemini-repl-send-input))))

;;; Prog Mode Integration

(defun gemini-repl-explain-error ()
  "Explain compilation error at point using context."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (user-error "Not in a programming mode"))
  (let* ((line-num (line-number-at-pos))
         (context-start (max (point-min)
                            (save-excursion
                              (forward-line (- gemini-repl-emacs-context-lines))
                              (point))))
         (context-end (min (point-max)
                          (save-excursion
                            (forward-line gemini-repl-emacs-context-lines)
                            (point))))
         (context (buffer-substring-no-properties context-start context-end))
         (error-line (thing-at-point 'line t))
         (mode-name (symbol-name major-mode))
         (filename (or (buffer-file-name) (buffer-name))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this error in %s (file: %s, line %d):\n\n"
                      mode-name filename line-num))
      (insert (format "Error line:\n%s\n\n" error-line))
      (insert (format "Context (lines %d-%d):\n```%s\n%s\n```"
                      (- line-num gemini-repl-emacs-context-lines)
                      (+ line-num gemini-repl-emacs-context-lines)
                      (gemini-repl--lang-for-mode)
                      context))
      (gemini-repl-send-input))))

(defun gemini-repl-suggest-fix ()
  "Suggest a fix for error at point."
  (interactive)
  (unless (derived-mode-p 'prog-mode)
    (user-error "Not in a programming mode"))
  (let* ((line-num (line-number-at-pos))
         (context-start (max (point-min)
                            (save-excursion
                              (forward-line (- gemini-repl-emacs-context-lines))
                              (point))))
         (context-end (min (point-max)
                          (save-excursion
                            (forward-line gemini-repl-emacs-context-lines)
                            (point))))
         (context (buffer-substring-no-properties context-start context-end))
         (mode-name (symbol-name major-mode))
         (filename (or (buffer-file-name) (buffer-name))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "Suggest a fix for the code in %s (file: %s, line %d):\n\n"
                      mode-name filename line-num))
      (insert (format "Context:\n```%s\n%s\n```\n\n"
                      (gemini-repl--lang-for-mode)
                      context))
      (insert "Please suggest a fix and explain the issue.")
      (gemini-repl-send-input))))

(defun gemini-repl-explain-at-point ()
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
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this in the context of %s:\n\n%s"
                      (symbol-name major-mode)
                      thing))
      (gemini-repl-send-input))))

(defun gemini-repl--lang-for-mode ()
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

(defun gemini-repl-explain-compilation-error ()
  "Explain error from compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "Not in compilation-mode"))
  (let ((error-msg (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "Explain this compilation error:\n\n%s" error-msg))
      (gemini-repl-send-input))))

;;; Interactive Query Functions

(defun gemini-repl-ask-about-buffer ()
  "Ask a question about the current buffer."
  (interactive)
  (let ((question (read-string "Ask about this buffer: "))
        (content (buffer-substring-no-properties (point-min) (point-max)))
        (filename (or (buffer-file-name) (buffer-name))))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nFile: %s\n\n```\n%s\n```"
                      question filename content))
      (gemini-repl-send-input))))

(defun gemini-repl-ask-about-project ()
  "Ask a question about the current project."
  (interactive)
  (let ((question (read-string "Ask about this project: "))
        (project-root (or (and (fboundp 'project-root)
                              (project-root (project-current)))
                         default-directory)))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert (format "%s\n\nProject root: %s\n\n"
                      question project-root))
      (insert "Context: Use tools to explore the project structure and files.")
      (gemini-repl-send-input))))

;;; Minor Mode

(defvar gemini-repl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Global bindings (C-c C-g prefix)
    (define-key map (kbd "C-c C-g g") #'gemini-repl-send-region-improved)
    (define-key map (kbd "C-c C-g b") #'gemini-repl-send-buffer)
    (define-key map (kbd "C-c C-g d") #'gemini-repl-send-defun)
    (define-key map (kbd "C-c C-g r") #'gemini-repl)
    (define-key map (kbd "C-c C-g e") #'gemini-repl-explain-at-point)
    (define-key map (kbd "C-c C-g i") #'gemini-repl-insert-response)
    (define-key map (kbd "C-c C-g a") #'gemini-repl-ask-about-buffer)
    (define-key map (kbd "C-c C-g p") #'gemini-repl-ask-about-project)

    ;; Org mode specific
    (define-key map (kbd "C-c C-g o s") #'gemini-repl-org-send-subtree)
    (define-key map (kbd "C-c C-g o c") #'gemini-repl-org-send-src-block)
    (define-key map (kbd "C-c C-g o i") #'gemini-repl-org-insert-response)
    (define-key map (kbd "C-c C-g o p") #'gemini-repl-org-process-ai-blocks)

    ;; Prog mode specific
    (define-key map (kbd "C-c C-g x") #'gemini-repl-explain-error)
    (define-key map (kbd "C-c C-g f") #'gemini-repl-suggest-fix)

    map)
  "Keymap for gemini-repl-mode.")

;;;###autoload
(define-minor-mode gemini-repl-mode
  "Minor mode for gemini-repl Emacs integrations.

Provides convenient keybindings for AI-assisted coding:

\\{gemini-repl-mode-map}"
  :lighter " GeminiREPL"
  :keymap gemini-repl-mode-map
  :group 'gemini-repl-emacs)

;;;###autoload
(define-globalized-minor-mode global-gemini-repl-mode
  gemini-repl-mode
  (lambda () (gemini-repl-mode 1))
  :group 'gemini-repl-emacs)

;;; Dired Mode Integration

(defun gemini-repl-emacs-dired-setup ()
  "Setup gemini-repl bindings in dired-mode."
  (when (derived-mode-p 'dired-mode)
    (local-set-key (kbd "C-c C-g s") #'gemini-repl-dired-summarize)
    (local-set-key (kbd "C-c C-g d") #'gemini-repl-dired-describe-file)))

(add-hook 'dired-mode-hook #'gemini-repl-emacs-dired-setup)

;;; Compilation Mode Integration

(defun gemini-repl-emacs-compilation-setup ()
  "Setup gemini-repl bindings in compilation-mode."
  (when (derived-mode-p 'compilation-mode)
    (local-set-key (kbd "C-c C-g e") #'gemini-repl-explain-compilation-error)))

(add-hook 'compilation-mode-hook #'gemini-repl-emacs-compilation-setup)

;;; Response Capture

(defun gemini-repl-emacs-capture-response (response)
  "Capture RESPONSE for later insertion.
This is called internally by gemini-repl."
  (setq gemini-repl-last-response response))

;; Advice to capture responses
(advice-add 'gemini-repl--handle-response :after
            (lambda (response)
              (when (eq (alist-get 'type response) 'text)
                (gemini-repl-emacs-capture-response
                 (alist-get 'content response)))))

(provide 'gemini-repl-emacs)
;;; gemini-repl-emacs.el ends here
