;;; init-example.el --- Example configuration for gemini-repl-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Example configuration showing how to set up gemini-repl with
;; Emacs-native integrations.

;;; Code:

;;; Basic Setup

(require 'gemini-repl)
(require 'gemini-repl-emacs)

;; API Keys
(setq gemini-repl-api-key (getenv "GEMINI_API_KEY"))
(setq gemini-repl-openai-api-key (getenv "OPENAI_API_KEY"))

;; Provider selection
(setq gemini-repl-provider 'gemini)  ; or 'ollama, 'openai

;; Model selection (optional)
;; (setq gemini-repl-model "gemini-2.0-flash-exp")

;; Workspace (defaults to project root or current directory)
;; (setq gemini-repl-workspace "/path/to/project")

;;; Permission Settings

;; YOLO mode: skip all permission checks (use with caution!)
(setq gemini-repl-yolo-mode nil)

;; Confirm even safe (read-only) tools
(setq gemini-repl-confirm-safe-tools nil)

;; Maximum tool iterations per request
(setq gemini-repl-max-tool-iterations 10)

;;; Emacs Integration Customization

;; Org mode AI block type
(setq gemini-repl-emacs-org-ai-block-type "ai")

;; Insert responses as comments in code buffers
(setq gemini-repl-emacs-insert-as-comment nil)

;; Context lines for error explanations
(setq gemini-repl-emacs-context-lines 5)

;; Auto-format responses based on major mode
(setq gemini-repl-emacs-auto-format-response t)

;;; Enable Minor Mode

;; Enable globally for all buffers
(global-gemini-repl-mode 1)

;; OR enable selectively for specific modes
;; (add-hook 'emacs-lisp-mode-hook #'gemini-repl-mode)
;; (add-hook 'python-mode-hook #'gemini-repl-mode)
;; (add-hook 'rust-mode-hook #'gemini-repl-mode)
;; (add-hook 'org-mode-hook #'gemini-repl-mode)

;;; Custom Tools Example

;; Register a custom tool
(gemini-repl-register-tool
 "count_lines"
 "Count lines in a file"
 '((type . "object")
   (properties . ((path . ((type . "string")
                          (description . "File path to count lines in")))))
   (required . ["path"]))
 (lambda (args)
   (let ((path (alist-get 'path args)))
     (if (gemini-repl--safe-path-p path)
         (with-temp-buffer
           (insert-file-contents (expand-file-name path (gemini-repl--get-workspace)))
           (format "%d lines" (count-lines (point-min) (point-max))))
       (format "Unsafe path: %s" path)))))

;; Another custom tool example
(gemini-repl-register-tool
 "current_time"
 "Get the current time"
 '((type . "object")
   (properties . ())
   (required . []))
 (lambda (_args)
   (format-time-string "%Y-%m-%d %H:%M:%S")))

;;; Advanced Configuration

;; Session persistence
;; (setq gemini-repl-history-file
;;       (expand-file-name "gemini-repl-history" user-emacs-directory))

;; For Ollama users (local inference)
;; (setq gemini-repl-provider 'ollama)
;; (setq gemini-repl-ollama-host "http://localhost:11434")
;; (setq gemini-repl-model "llama3.2")

;;; Keybinding Customization (optional)

;; If you want different keybindings, you can customize the mode map
;; (with-eval-after-load 'gemini-repl-emacs
;;   (define-key gemini-repl-mode-map (kbd "C-c a r") #'gemini-repl)
;;   (define-key gemini-repl-mode-map (kbd "C-c a g") #'gemini-repl-send-region-improved)
;;   (define-key gemini-repl-mode-map (kbd "C-c a b") #'gemini-repl-send-buffer))

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
;;     (setq gemini-repl-workspace (project-root project))))
;;
;; (add-hook 'gemini-repl-mode-hook #'my-gemini-set-workspace-to-project)

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
;; (advice-add 'gemini-repl--process-input :before #'my-gemini-log-interaction)

;;; Usage Examples in Comments

;; Basic REPL:
;;   M-x gemini-repl
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
