;;; screencast-demo.el --- Interactive screencast demo -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Step-by-step demonstration of sage tool calling for screencasts.
;; Run with: emacs -Q -L . -l examples/screencast-demo.el
;;
;; Each function demonstrates a specific capability with visible output.
;; Press C-c C-e to evaluate the highlighted block.

;;; Code:

(require 'sage-tools)
(require 'sage-tool-factory)

;; Initialize tools
(sage--init-default-tools)
(sage-tool-factory-init)

(defvar screencast-demo-delay 0.5
  "Delay between demo steps for visibility.")

(defvar screencast-demo-buffer "*Screencast Demo*"
  "Buffer for demo output.")

(defun screencast-demo--show (title result)
  "Display TITLE and RESULT in demo buffer."
  (with-current-buffer (get-buffer-create screencast-demo-buffer)
    (goto-char (point-max))
    (insert (format "\n%s\n%s\n%s\n\n"
                    (make-string 60 ?=)
                    title
                    (make-string 60 ?-)))
    (insert (format "%s\n" result)))
  (display-buffer screencast-demo-buffer)
  (sit-for screencast-demo-delay))

(defun screencast-demo--call (tool-name args)
  "Call TOOL-NAME with ARGS and display result."
  (let* ((tool (cl-find tool-name sage-tools
                        :key (lambda (t) (alist-get 'name t))
                        :test #'string=))
         (fn (alist-get 'execute tool)))
    (if fn
        (condition-case err
            (let ((result (funcall fn args)))
              (screencast-demo--show
               (format "Tool: %s" tool-name)
               result)
              result)
          (error
           (screencast-demo--show
            (format "Tool: %s (ERROR)" tool-name)
            (error-message-string err))))
      (screencast-demo--show
       (format "Tool: %s" tool-name)
       "Tool not found"))))

;;; Demo Sections

;;;###autoload
(defun screencast-demo-intro ()
  "Introduction slide."
  (interactive)
  (with-current-buffer (get-buffer-create screencast-demo-buffer)
    (erase-buffer)
    (insert "
    ____                    _____           _
   / ___|  __ _  __ _  ___ |_   _|__   ___ | |___
   \\___ \\ / _` |/ _` |/ _ \\  | |/ _ \\ / _ \\| / __|
    ___) | (_| | (_| |  __/  | | (_) | (_) | \\__ \\
   |____/ \\__,_|\\__, |\\___|  |_|\\___/ \\___/|_|___/
                |___/

   Self-Extending AI Tools for Emacs

   28 Built-in Tools | Pure Emacs Primitives | No Shell Fallbacks

   Today's Demo:
   1. File Operations
   2. Git Integration (magit)
   3. Org-mode TODO Management
   4. Self-Extending Tool Factory
   5. Custom Tools (Hacker News, Weather, UUID)

")
    (display-buffer screencast-demo-buffer)))

;;;###autoload
(defun screencast-demo-file-tools ()
  "Demonstrate file tools."
  (interactive)
  (screencast-demo--show "FILE TOOLS" "Reading, writing, and searching files")

  ;; Write a test file
  (screencast-demo--call "write_file"
                         '((path . "demo-test.txt")
                           (content . "Line 1: Hello from sage!\nLine 2: Tool calling demo\nLine 3: Pure Emacs power")))

  ;; Read it back
  (screencast-demo--call "read_file" '((path . "demo-test.txt")))

  ;; List elisp files
  (screencast-demo--call "list_files" '((path . ".") (pattern . "*.el")))

  ;; Search for defun
  (screencast-demo--call "code_search" '((pattern . "defun sage") (file_type . "el")))

  ;; Cleanup
  (delete-file (expand-file-name "demo-test.txt" (sage-tools--get-workspace))))

;;;###autoload
(defun screencast-demo-git-tools ()
  "Demonstrate git tools (requires magit)."
  (interactive)
  (screencast-demo--show "GIT TOOLS" "Magit-powered git integration")

  (if (require 'magit nil t)
      (progn
        (screencast-demo--call "git_status" nil)
        (screencast-demo--call "git_branch" nil)
        (screencast-demo--call "git_log" '((count . 5))))
    (screencast-demo--show "GIT TOOLS" "Skipped: magit not available")))

;;;###autoload
(defun screencast-demo-org-tools ()
  "Demonstrate org-mode tools."
  (interactive)
  (screencast-demo--show "ORG-MODE TOOLS" "Native org-mode TODO management")

  ;; Create test org file
  (let ((test-file "demo-todos.org"))
    (with-temp-file (expand-file-name test-file (sage-tools--get-workspace))
      (insert "#+TITLE: Demo TODOs\n\n")
      (insert "* TODO First task\n")
      (insert "* TODO [#A] High priority\n")
      (insert "* DONE Completed task\n"))

    ;; List todos
    (screencast-demo--call "org_todo_list" `((file . ,test-file)))

    ;; Add a new todo
    (screencast-demo--call "org_add_todo"
                           `((file . ,test-file)
                             (heading . "New task from demo")
                             (state . "TODO")
                             (priority . "B")))

    ;; Show updated list
    (screencast-demo--call "org_todo_list" `((file . ,test-file)))

    ;; Cleanup
    (delete-file (expand-file-name test-file (sage-tools--get-workspace)))))

;;;###autoload
(defun screencast-demo-emacs-tools ()
  "Demonstrate Emacs introspection tools."
  (interactive)
  (screencast-demo--show "EMACS TOOLS" "Introspection and evaluation")

  (screencast-demo--call "describe_function" '((function . "mapcar")))
  (screencast-demo--call "describe_variable" '((variable . "version-control")))
  (screencast-demo--call "eval_elisp" '((code . "(+ 1 2 3 4 5)")))
  (screencast-demo--call "eval_elisp" '((code . "(format-time-string \"%Y-%m-%d %H:%M\")")))
  (screencast-demo--call "list_buffers" nil))

;;;###autoload
(defun screencast-demo-factory-tools ()
  "Demonstrate self-extending tool factory."
  (interactive)
  (screencast-demo--show "TOOL FACTORY" "Self-extending tool system")

  ;; List custom tools
  (screencast-demo--call "list_custom_tools" nil)

  ;; Create a new tool
  (screencast-demo--call "create_tool"
                         '((name . "reverse_string")
                           (description . "Reverse a string")
                           (parameters . ((type . "object")
                                          (properties . ((text . ((type . "string")))))
                                          (required . ["text"])))
                           (code . "(reverse (alist-get 'text args))")))

  ;; Use the new tool
  (screencast-demo--call "reverse_string" '((text . "Hello Emacs!")))

  ;; Delete it
  (screencast-demo--call "delete_tool" '((name . "reverse_string"))))

;;;###autoload
(defun screencast-demo-custom-tools ()
  "Demonstrate built-in custom tools."
  (interactive)
  (screencast-demo--show "CUSTOM TOOLS" "Pre-built useful tools")

  ;; UUID
  (screencast-demo--call "generate_uuid" nil)

  ;; Timestamp
  (screencast-demo--call "timestamp" '((format . "iso")))
  (screencast-demo--call "timestamp" '((format . "human")))

  ;; Weather (requires network)
  (when (y-or-n-p "Fetch live weather? ")
    (screencast-demo--call "weather" '((location . "San Francisco"))))

  ;; Hacker News (requires network)
  (when (y-or-n-p "Fetch Hacker News? ")
    (screencast-demo--call "hackernews_summary" '((count . 5)))))

;;;###autoload
(defun screencast-demo-all ()
  "Run complete screencast demo."
  (interactive)
  (screencast-demo-intro)
  (when (y-or-n-p "Continue to File Tools? ")
    (screencast-demo-file-tools))
  (when (y-or-n-p "Continue to Git Tools? ")
    (screencast-demo-git-tools))
  (when (y-or-n-p "Continue to Org Tools? ")
    (screencast-demo-org-tools))
  (when (y-or-n-p "Continue to Emacs Tools? ")
    (screencast-demo-emacs-tools))
  (when (y-or-n-p "Continue to Tool Factory? ")
    (screencast-demo-factory-tools))
  (when (y-or-n-p "Continue to Custom Tools? ")
    (screencast-demo-custom-tools))
  (screencast-demo--show "DEMO COMPLETE"
                         "Thank you for watching!\n\nRepository: github.com/aygp-dr/gemini-repl-010"))

;;;###autoload
(defun screencast-demo-quick ()
  "Run quick non-interactive demo."
  (interactive)
  (screencast-demo-intro)
  (sit-for 2)
  (screencast-demo-file-tools)
  (screencast-demo-git-tools)
  (screencast-demo-org-tools)
  (screencast-demo-emacs-tools)
  (screencast-demo-factory-tools)
  ;; Skip network tools in quick mode
  (screencast-demo--call "generate_uuid" nil)
  (screencast-demo--call "timestamp" '((format . "iso")))
  (screencast-demo--show "DEMO COMPLETE" "Quick demo finished!"))

(provide 'screencast-demo)
;;; screencast-demo.el ends here
