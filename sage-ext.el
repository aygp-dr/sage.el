;;; sage-ext.el --- Extension infrastructure for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, extensions
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; sage-ext provides the extension infrastructure for sage.el, enabling
;; third-party packages to integrate deeply with sage (similar to how
;; MELPA packages extend Emacs core).
;;
;; Extension Points:
;;
;; 1. HOOKS - Standard Emacs hook variables for lifecycle events
;;    - `sage-mode-hook' - when REPL buffer is created
;;    - `sage-before-request-hook' - before API call to LLM
;;    - `sage-after-response-hook' - after response received
;;    - `sage-tool-call-hook' - around tool execution
;;    - `sage-message-added-hook' - when message added to conversation
;;
;; 2. REGISTRIES - Alist-based registries (like auto-mode-alist)
;;    - `sage-slash-commands' - slash command dispatch
;;    - `sage-providers' - LLM provider implementations
;;    - `sage-tools' - tool definitions (already in sage.el)
;;
;; 3. FILTER FUNCTIONS - Transform data at boundaries
;;    - `sage-message-filter-functions' - transform outgoing messages
;;    - `sage-response-filter-functions' - transform incoming responses
;;
;; 4. EXTENSION REGISTRY - Metadata about loaded extensions
;;    - `sage-extensions' - alist of extension name -> plist
;;
;; Usage for extension authors:
;;
;;   (require 'sage-ext)
;;
;;   ;; Register your extension
;;   (sage-register-extension
;;    'my-ext
;;    :version "0.1.0"
;;    :description "My sage extension"
;;    :requires '(sage)
;;    :init #'my-ext-init
;;    :enable #'my-ext-enable
;;    :disable #'my-ext-disable)
;;
;;   ;; Add slash commands
;;   (sage-register-slash-command "mycommand" #'my-ext--cmd-handler)
;;
;;   ;; Hook into lifecycle
;;   (add-hook 'sage-before-request-hook #'my-ext--before-request)

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup sage-ext nil
  "Extension infrastructure for sage."
  :group 'sage
  :prefix "sage-ext-")

(defcustom sage-ext-directory
  (expand-file-name "sage-extensions" user-emacs-directory)
  "Directory for third-party sage extensions."
  :type 'directory
  :group 'sage-ext)

(defcustom sage-ext-auto-enable t
  "When non-nil, automatically enable extensions on registration."
  :type 'boolean
  :group 'sage-ext)

;;; Hook Variables
;;
;; These are the primary extension points for sage.  Extensions add
;; functions to these hooks to participate in the sage lifecycle.

(defvar sage-mode-hook nil
  "Hook run when `sage-mode' is enabled in a buffer.
Use this for buffer-local setup like keybindings or minor modes.")

(defvar sage-before-request-hook nil
  "Hook run before sending a request to the LLM.
Functions receive no arguments.  Use `sage-conversation' to inspect
the current conversation state.")

(defvar sage-after-response-hook nil
  "Hook run after receiving a response from the LLM.
Functions receive one argument: the parsed response content.")

(defvar sage-tool-call-hook nil
  "Hook run around tool execution.
Functions receive three arguments: TOOL-NAME, ARGS, and PHASE.
PHASE is either `before' or `after'.  During `after', a fourth
argument RESULT contains the tool output.")

(defvar sage-message-added-hook nil
  "Hook run when a message is added to the conversation.
Functions receive one argument: the message alist with `role' and `content'.")

(defvar sage-session-start-hook nil
  "Hook run when a new sage session starts.")

(defvar sage-session-end-hook nil
  "Hook run when a sage session ends.")

;;; Filter Function Lists
;;
;; These work like `kill-buffer-query-functions' - each function
;; receives a value and returns a (possibly transformed) value.
;; The chain is run in order, each receiving the previous output.

(defvar sage-message-filter-functions nil
  "List of functions to filter outgoing messages.
Each function receives a message alist and returns a (possibly
modified) message alist.  Functions are called in order, each
receiving the output of the previous function.")

(defvar sage-response-filter-functions nil
  "List of functions to filter incoming responses.
Each function receives the response content string and returns
a (possibly modified) string.")

(defvar sage-tool-result-filter-functions nil
  "List of functions to filter tool results before returning to LLM.
Each function receives (TOOL-NAME RESULT) and returns modified RESULT.")

;;; Registries

(defvar sage-slash-commands nil
  "Alist of (COMMAND . FUNCTION) for slash command dispatch.
COMMAND is a string without the leading slash.
FUNCTION receives the argument string (may be nil).")

(defvar sage-providers nil
  "Alist of (PROVIDER . PLIST) for LLM provider implementations.
PLIST contains:
  :format-fn - function to format request for this provider
  :parse-fn - function to parse response from this provider
  :endpoint-fn - function to return API endpoint
  :auth-fn - function to return auth headers (optional)")

(defvar sage-extensions nil
  "Alist of (NAME . PLIST) for registered extensions.
PLIST contains:
  :version - version string
  :description - human-readable description
  :requires - list of required features
  :init - initialization function
  :enable - enable function
  :disable - disable function
  :enabled - whether currently enabled")

;;; Extension Registration

(cl-defun sage-register-extension (name &key version description requires
                                        init enable disable)
  "Register extension NAME with sage.

NAME is a symbol identifying the extension.

Keyword arguments:
  :version     - Version string (e.g., \"0.1.0\")
  :description - Human-readable description
  :requires    - List of required features (checked on enable)
  :init        - Function called immediately on registration
  :enable      - Function called to activate the extension
  :disable     - Function called to deactivate the extension

Example:
  (sage-register-extension
   \\='my-ext
   :version \"0.1.0\"
   :description \"My sage extension\"
   :requires \\='(sage magit)
   :init #\\='my-ext-init
   :enable #\\='my-ext-enable
   :disable #\\='my-ext-disable)"
  (let ((ext-plist (list :version version
                         :description description
                         :requires requires
                         :init init
                         :enable enable
                         :disable disable
                         :enabled nil)))
    ;; Remove old registration if exists
    (setq sage-extensions (assq-delete-all name sage-extensions))
    ;; Add new registration
    (push (cons name ext-plist) sage-extensions)
    ;; Call init function
    (when init
      (funcall init))
    ;; Auto-enable if configured
    (when sage-ext-auto-enable
      (sage-ext-enable name))
    name))

(defun sage-ext-enable (name)
  "Enable extension NAME.
Checks requirements and calls the extension's enable function."
  (let ((ext (assq name sage-extensions)))
    (unless ext
      (error "Extension not registered: %s" name))
    (let* ((plist (cdr ext))
           (requires (plist-get plist :requires))
           (enable-fn (plist-get plist :enable)))
      ;; Check requirements
      (dolist (req requires)
        (unless (featurep req)
          (error "Extension %s requires %s" name req)))
      ;; Call enable function
      (when enable-fn
        (funcall enable-fn))
      ;; Mark as enabled
      (plist-put plist :enabled t)
      (message "Enabled sage extension: %s" name))))

(defun sage-ext-disable (name)
  "Disable extension NAME.
Calls the extension's disable function."
  (let ((ext (assq name sage-extensions)))
    (unless ext
      (error "Extension not registered: %s" name))
    (let* ((plist (cdr ext))
           (disable-fn (plist-get plist :disable)))
      ;; Call disable function
      (when disable-fn
        (funcall disable-fn))
      ;; Mark as disabled
      (plist-put plist :enabled nil)
      (message "Disabled sage extension: %s" name))))

(defun sage-ext-enabled-p (name)
  "Return non-nil if extension NAME is enabled."
  (let ((ext (assq name sage-extensions)))
    (and ext (plist-get (cdr ext) :enabled))))

(defun sage-ext-list ()
  "Return list of registered extension names."
  (mapcar #'car sage-extensions))

(defun sage-ext-info (name)
  "Return plist of extension NAME, or nil if not registered."
  (cdr (assq name sage-extensions)))

;;; Slash Command Registration

(defun sage-register-slash-command (command function &optional replace)
  "Register COMMAND to call FUNCTION.
COMMAND is a string without the leading slash.
FUNCTION receives the argument string (may be empty string).
If REPLACE is non-nil, replace existing command."
  (let ((existing (assoc command sage-slash-commands)))
    (if existing
        (if replace
            (setcdr existing function)
          (error "Slash command already registered: /%s" command))
      (push (cons command function) sage-slash-commands)))
  command)

(defun sage-unregister-slash-command (command)
  "Unregister slash COMMAND."
  (setq sage-slash-commands
        (assoc-delete-all command sage-slash-commands)))

(defun sage-ext--dispatch-slash-command (input)
  "Dispatch slash command from INPUT string.
Returns t if command was handled, nil otherwise."
  (when (string-prefix-p "/" input)
    (let* ((parts (split-string (substring input 1) " " t))
           (cmd (car parts))
           (args (string-join (cdr parts) " "))
           (handler (cdr (assoc cmd sage-slash-commands))))
      (when handler
        (funcall handler args)
        t))))

;;; Provider Registration

(cl-defun sage-register-provider (name &key format-fn parse-fn endpoint-fn auth-fn)
  "Register LLM provider NAME.

NAME is a symbol identifying the provider.

Keyword arguments:
  :format-fn   - Function to format request body (receives messages, tools)
  :parse-fn    - Function to parse response (receives response)
  :endpoint-fn - Function to return API endpoint URL
  :auth-fn     - Function to return authorization (optional)

Example:
  (sage-register-provider
   \\='anthropic
   :format-fn #\\='my-format-anthropic
   :parse-fn #\\='my-parse-anthropic
   :endpoint-fn (lambda () \"https://api.anthropic.com/v1/messages\"))"
  (let ((provider-plist (list :format-fn format-fn
                              :parse-fn parse-fn
                              :endpoint-fn endpoint-fn
                              :auth-fn auth-fn)))
    (setq sage-providers (assq-delete-all name sage-providers))
    (push (cons name provider-plist) sage-providers))
  name)

(defun sage-ext--get-provider-fn (provider fn-key)
  "Get FN-KEY function for PROVIDER."
  (let ((plist (cdr (assq provider sage-providers))))
    (plist-get plist fn-key)))

;;; Filter Function Utilities

(defun sage-ext--run-filters (functions value &rest extra-args)
  "Run FUNCTIONS as a filter chain on VALUE.
Each function receives VALUE and EXTRA-ARGS, returns new VALUE.
Returns the final filtered value."
  (dolist (fn functions)
    (setq value (apply fn value extra-args)))
  value)

(defun sage-ext-filter-message (message)
  "Run MESSAGE through `sage-message-filter-functions'."
  (sage-ext--run-filters sage-message-filter-functions message))

(defun sage-ext-filter-response (response)
  "Run RESPONSE through `sage-response-filter-functions'."
  (sage-ext--run-filters sage-response-filter-functions response))

(defun sage-ext-filter-tool-result (tool-name result)
  "Run RESULT through `sage-tool-result-filter-functions'.
TOOL-NAME is passed as context."
  (sage-ext--run-filters sage-tool-result-filter-functions result tool-name))

;;; Hook Utilities

(defun sage-ext-run-tool-hook (phase tool-name args &optional result)
  "Run `sage-tool-call-hook' for PHASE with TOOL-NAME, ARGS, RESULT."
  (run-hook-with-args 'sage-tool-call-hook tool-name args phase result))

;;; Interactive Commands

(defun sage-ext-list-extensions ()
  "Display a list of registered sage extensions."
  (interactive)
  (with-current-buffer (get-buffer-create "*sage-extensions*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Sage Extensions\n")
      (insert (make-string 50 ?=) "\n\n")
      (if (null sage-extensions)
          (insert "No extensions registered.\n")
        (dolist (ext sage-extensions)
          (let* ((name (car ext))
                 (plist (cdr ext))
                 (version (plist-get plist :version))
                 (description (plist-get plist :description))
                 (enabled (plist-get plist :enabled)))
            (insert (format "%s %s (%s)\n"
                            (if enabled "[*]" "[ ]")
                            name
                            (or version "unknown")))
            (when description
              (insert (format "    %s\n" description)))
            (insert "\n"))))
      (insert "\n[*] = enabled, [ ] = disabled\n")
      (goto-char (point-min)))
    (special-mode)
    (display-buffer (current-buffer))))

(defun sage-ext-list-slash-commands ()
  "Display a list of registered slash commands."
  (interactive)
  (with-current-buffer (get-buffer-create "*sage-slash-commands*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Sage Slash Commands\n")
      (insert (make-string 50 ?=) "\n\n")
      (if (null sage-slash-commands)
          (insert "No slash commands registered.\n")
        (dolist (cmd (sort (copy-sequence sage-slash-commands)
                           (lambda (a b) (string< (car a) (car b)))))
          (insert (format "/%s\n" (car cmd)))))
      (goto-char (point-min)))
    (special-mode)
    (display-buffer (current-buffer))))

(provide 'sage-ext)

;;; sage-ext.el ends here
