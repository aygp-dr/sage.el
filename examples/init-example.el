;;; init-example.el --- Example configuration for sage-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Example configuration showing how to set up sage with
;; Emacs-native integrations.

;;; Code:

;;; Basic Setup

(require 'sage)
(require 'sage-emacs)

;; API Keys
(setq sage-api-key (getenv "GEMINI_API_KEY"))
(setq sage-openai-api-key (getenv "OPENAI_API_KEY"))

;; Provider selection
(setq sage-provider 'gemini)  ; or 'ollama, 'openai

;; Model selection (optional)
;; (setq sage-model "gemini-2.0-flash-exp")

;; Workspace (defaults to project root or current directory)
;; (setq sage-workspace "/path/to/project")

;;; Permission Settings

;; YOLO mode: skip all permission checks (use with caution!)
(setq sage-yolo-mode nil)

;; Confirm even safe (read-only) tools
(setq sage-confirm-safe-tools nil)

;; Maximum tool iterations per request
(setq sage-max-tool-iterations 10)

;;; Emacs Integration Customization

;; Org mode AI block type
(setq sage-emacs-org-ai-block-type "ai")

;; Insert responses as comments in code buffers
(setq sage-emacs-insert-as-comment nil)

;; Context lines for error explanations
(setq sage-emacs-context-lines 5)

;; Auto-format responses based on major mode
(setq sage-emacs-auto-format-response t)

;;; Enable Minor Mode

;; Enable globally for all buffers
(global-sage-mode 1)

;; OR enable selectively for specific modes
;; (add-hook 'emacs-lisp-mode-hook #'sage-mode)
;; (add-hook 'python-mode-hook #'sage-mode)
;; (add-hook 'rust-mode-hook #'sage-mode)
;; (add-hook 'org-mode-hook #'sage-mode)

;;; Custom Tools Example

;; Register a custom tool
(sage-register-tool
 "count_lines"
 "Count lines in a file"
 '((type . "object")
   (properties . ((path . ((type . "string")
                          (description . "File path to count lines in")))))
   (required . ["path"]))
 (lambda (args)
   (let ((path (alist-get 'path args)))
     (if (sage--safe-path-p path)
         (with-temp-buffer
           (insert-file-contents (expand-file-name path (sage--get-workspace)))
           (format "%d lines" (count-lines (point-min) (point-max))))
       (format "Unsafe path: %s" path)))))

;; Another custom tool example
(sage-register-tool
 "current_time"
 "Get the current time"
 '((type . "object")
   (properties . ())
   (required . []))
 (lambda (_args)
   (format-time-string "%Y-%m-%d %H:%M:%S")))

;;; Advanced Configuration

;; Session persistence
;; (setq sage-history-file
;;       (expand-file-name "sage-history" user-emacs-directory))

;; For Ollama users (local inference)
;; (setq sage-provider 'ollama)
;; (setq sage-ollama-host "http://localhost:11434")
;; (setq sage-model "llama3.2")

;;; Keybinding Customization (optional)

;; If you want different keybindings, you can customize the mode map
;; (with-eval-after-load 'sage-emacs
;;   (define-key sage-mode-map (kbd "C-c a r") #'sage)
;;   (define-key sage-mode-map (kbd "C-c a g") #'sage-send-region-improved)
;;   (define-key sage-mode-map (kbd "C-c a b") #'sage-send-buffer))

;;; Org Mode Integration Example

;; Add custom org babel language for AI blocks
;; (with-eval-after-load 'org
;;   (add-to-list 'org-structure-template-alist
;;                '("ai" . "ai")))

;;; Project Integration Example

;; Use project.el to set workspace automatically
;; (defun my-gemini-set-workspace-to-project ()
;;   "Set gemini workspace to current project root."
;;   (when-let ((project (project-current)))
;;     (setq sage-workspace (project-root project))))
;;
;; (add-hook 'sage-mode-hook #'my-gemini-set-workspace-to-project)

;;; Completion Integration (optional)

;; Example: Use with company-mode for code completion
;; (defun my-gemini-complete-at-point ()
;;   "Use Gemini for code completion at point."
;;   (interactive)
;;   (let* ((bounds (bounds-of-thing-at-point 'symbol))
;;          (start (or (car bounds) (point)))
;;          (end (or (cdr bounds) (point)))
;;          (prefix (buffer-substring-no-properties start end))
;;          (context (buffer-substring-no-properties
;;                   (max (point-min) (- (point) 500))
;;                   (min (point-max) (+ (point) 100)))))
;;     ;; This would need async implementation for practical use
;;     (message "Completion for: %s" prefix)))

;;; Advice Examples

;; Log all AI interactions
;; (defvar my-gemini-log-file "~/.emacs.d/gemini-log.org")
;;
;; (defun my-gemini-log-interaction (input)
;;   "Log INPUT to gemini log file."
;;   (with-temp-buffer
;;     (insert (format "* %s\n%s\n\n"
;;                     (format-time-string "%Y-%m-%d %H:%M:%S")
;;                     input))
;;     (append-to-file (point-min) (point-max) my-gemini-log-file)))
;;
;; (advice-add 'sage--process-input :before #'my-gemini-log-interaction)

;;; Usage Examples in Comments

;; Basic REPL:
;;   M-x sage
;;
;; Send current buffer:
;;   C-c C-g b
;;
;; Send region:
;;   Select region, then C-c C-g g
;;
;; Send function:
;;   C-c C-g d
;;
;; Explain code at point:
;;   C-c C-g e
;;
;; In Org mode, send subtree:
;;   C-c C-g o s
;;
;; In Dired, summarize marked files:
;;   Mark files with 'm', then C-c C-g s
;;
;; Explain compilation error:
;;   In compilation buffer, navigate to error, then C-c C-g e
;;
;; Ask about current buffer:
;;   C-c C-g a
;;
;; Ask about project:
;;   C-c C-g p

(provide 'init-example)
;;; init-example.el ends here
