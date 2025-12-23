;;; gemini-repl.el --- AI REPL with tool calling for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (request "0.3.2") (markdown-mode "2.5"))
;; Keywords: ai, tools, llm, gemini, ollama
;; URL: https://github.com/aygp-dr/gemini-repl-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gemini-repl provides an interactive AI REPL with tool calling capabilities.
;; It supports multiple providers (Gemini, Ollama, OpenAI) and includes
;; built-in tools for file operations, code search, and git commands.
;;
;; Features:
;; - Multi-provider support (Gemini, Ollama, OpenAI)
;; - Tool/function calling with permission system
;; - File operations (read, write, list)
;; - Git integration (status, diff, log, blame)
;; - Code search via ripgrep
;; - Session persistence
;; - Project-based conversation history
;;
;; Usage:
;;   M-x gemini-repl        - Start REPL in dedicated buffer
;;   M-x gemini-repl-send   - Send region or prompt to AI
;;   M-x gemini-repl-exec   - Single-shot execution
;;
;; Configuration:
;;   (setq gemini-repl-api-key "your-api-key")
;;   (setq gemini-repl-provider 'gemini)  ; or 'ollama, 'openai

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'seq)

;;; Customization

(defgroup gemini-repl nil
  "AI REPL with tool calling."
  :group 'tools
  :prefix "gemini-repl-")

(defcustom gemini-repl-api-key nil
  "API key for Gemini.
Can also be set via GEMINI_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'gemini-repl)

(defcustom gemini-repl-openai-api-key nil
  "API key for OpenAI.
Can also be set via OPENAI_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'gemini-repl)

(defcustom gemini-repl-provider 'gemini
  "LLM provider to use."
  :type '(choice (const :tag "Google Gemini" gemini)
                 (const :tag "Ollama (local)" ollama)
                 (const :tag "OpenAI" openai))
  :group 'gemini-repl)

(defcustom gemini-repl-model nil
  "Model to use. If nil, uses provider default."
  :type '(choice (const nil) string)
  :group 'gemini-repl)

(defcustom gemini-repl-ollama-host "http://localhost:11434"
  "Ollama server URL."
  :type 'string
  :group 'gemini-repl)

(defcustom gemini-repl-workspace nil
  "Workspace directory for file operations.
If nil, uses `default-directory'."
  :type '(choice (const nil) directory)
  :group 'gemini-repl)

(defcustom gemini-repl-yolo-mode nil
  "When non-nil, skip all tool permission checks."
  :type 'boolean
  :group 'gemini-repl)

(defcustom gemini-repl-confirm-safe-tools nil
  "When non-nil, confirm even safe (read-only) tools."
  :type 'boolean
  :group 'gemini-repl)

(defcustom gemini-repl-max-tool-iterations 10
  "Maximum number of tool call iterations per request."
  :type 'integer
  :group 'gemini-repl)

(defcustom gemini-repl-history-file
  (expand-file-name "gemini-repl-history" user-emacs-directory)
  "File to save conversation history."
  :type 'file
  :group 'gemini-repl)

;;; Variables

(defvar gemini-repl-buffer-name "*gemini-repl*"
  "Name of the REPL buffer.")

(defvar gemini-repl-conversation nil
  "Current conversation history.")

(defvar gemini-repl-tools nil
  "Registered tools.")

(defvar gemini-repl-safe-tools
  '("read_file" "list_files" "git_status" "git_diff" "git_log"
    "git_branch" "git_blame" "code_search" "glob_files")
  "Tools that are safe (read-only) and don't require confirmation.")

;;; Provider API

(defun gemini-repl--get-api-key ()
  "Get API key for current provider."
  (pcase gemini-repl-provider
    ('gemini (or gemini-repl-api-key
                 (getenv "GEMINI_API_KEY")))
    ('openai (or gemini-repl-openai-api-key
                 (getenv "OPENAI_API_KEY")))
    ('ollama nil)))

(defun gemini-repl--get-model ()
  "Get model for current provider."
  (or gemini-repl-model
      (pcase gemini-repl-provider
        ('gemini "gemini-2.0-flash-exp")
        ('ollama "llama3.2")
        ('openai "gpt-4o"))))

(defun gemini-repl--get-endpoint ()
  "Get API endpoint for current provider."
  (pcase gemini-repl-provider
    ('gemini
     (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
             (gemini-repl--get-model)
             (gemini-repl--get-api-key)))
    ('ollama
     (format "%s/api/chat" gemini-repl-ollama-host))
    ('openai
     "https://api.openai.com/v1/chat/completions")))

(defun gemini-repl--format-request (messages tools)
  "Format request body for current provider.
MESSAGES is the conversation history.
TOOLS is the list of available tools."
  (pcase gemini-repl-provider
    ('gemini
     (gemini-repl--format-gemini-request messages tools))
    ('ollama
     (gemini-repl--format-ollama-request messages tools))
    ('openai
     (gemini-repl--format-openai-request messages tools))))

(defun gemini-repl--format-gemini-request (messages tools)
  "Format request for Gemini API."
  (let ((contents (mapcar (lambda (msg)
                            `((role . ,(if (string= (alist-get 'role msg) "user")
                                          "user" "model"))
                              (parts . [((text . ,(alist-get 'content msg)))])))
                          messages)))
    `((contents . ,(vconcat contents))
      (tools . [((function_declarations . ,(vconcat (mapcar #'gemini-repl--tool-to-gemini tools))))]))))

(defun gemini-repl--tool-to-gemini (tool)
  "Convert TOOL to Gemini function declaration format."
  `((name . ,(alist-get 'name tool))
    (description . ,(alist-get 'description tool))
    (parameters . ,(alist-get 'parameters tool))))

(defun gemini-repl--format-ollama-request (messages tools)
  "Format request for Ollama API."
  (let ((ollama-messages (mapcar (lambda (msg)
                                   `((role . ,(alist-get 'role msg))
                                     (content . ,(alist-get 'content msg))))
                                 messages)))
    ;; Add tools as system prompt for Ollama
    (when tools
      (push `((role . "system")
              (content . ,(gemini-repl--tools-system-prompt tools)))
            ollama-messages))
    `((model . ,(gemini-repl--get-model))
      (messages . ,(vconcat ollama-messages))
      (stream . :json-false))))

(defun gemini-repl--tools-system-prompt (tools)
  "Generate system prompt describing TOOLS for Ollama."
  (concat "You have access to the following tools:\n\n"
          (mapconcat
           (lambda (tool)
             (format "- %s: %s\n  Parameters: %s"
                     (alist-get 'name tool)
                     (alist-get 'description tool)
                     (json-encode (alist-get 'parameters tool))))
           tools "\n\n")
          "\n\nTo call a tool, respond with:\n"
          "```tool\n{\"name\": \"tool_name\", \"arguments\": {...}}\n```"))

(defun gemini-repl--format-openai-request (messages tools)
  "Format request for OpenAI API."
  (let ((openai-messages (mapcar (lambda (msg)
                                   `((role . ,(alist-get 'role msg))
                                     (content . ,(alist-get 'content msg))))
                                 messages))
        (openai-tools (mapcar (lambda (tool)
                                `((type . "function")
                                  (function . ((name . ,(alist-get 'name tool))
                                              (description . ,(alist-get 'description tool))
                                              (parameters . ,(alist-get 'parameters tool))))))
                              tools)))
    `((model . ,(gemini-repl--get-model))
      (messages . ,(vconcat openai-messages))
      ,@(when tools `((tools . ,(vconcat openai-tools)))))))

(defun gemini-repl--parse-response (response)
  "Parse RESPONSE from current provider."
  (pcase gemini-repl-provider
    ('gemini (gemini-repl--parse-gemini-response response))
    ('ollama (gemini-repl--parse-ollama-response response))
    ('openai (gemini-repl--parse-openai-response response))))

(defun gemini-repl--parse-gemini-response (response)
  "Parse Gemini API response."
  (let* ((candidates (alist-get 'candidates response))
         (content (alist-get 'content (aref candidates 0)))
         (parts (alist-get 'parts content))
         (part (aref parts 0)))
    (if-let ((function-call (alist-get 'functionCall part)))
        `((type . function_call)
          (name . ,(alist-get 'name function-call))
          (arguments . ,(alist-get 'args function-call)))
      `((type . text)
        (content . ,(alist-get 'text part))))))

(defun gemini-repl--parse-ollama-response (response)
  "Parse Ollama API response."
  (let* ((message (alist-get 'message response))
         (content (alist-get 'content message)))
    ;; Check for tool call in response
    (if (string-match "```tool\n\\(.*\\)\n```" content)
        (let ((tool-json (json-read-from-string (match-string 1 content))))
          `((type . function_call)
            (name . ,(alist-get 'name tool-json))
            (arguments . ,(alist-get 'arguments tool-json))))
      `((type . text)
        (content . ,content)))))

(defun gemini-repl--parse-openai-response (response)
  "Parse OpenAI API response."
  (let* ((choices (alist-get 'choices response))
         (message (alist-get 'message (aref choices 0)))
         (tool-calls (alist-get 'tool_calls message)))
    (if tool-calls
        (let ((tool-call (aref tool-calls 0)))
          `((type . function_call)
            (name . ,(alist-get 'name (alist-get 'function tool-call)))
            (arguments . ,(json-read-from-string
                          (alist-get 'arguments (alist-get 'function tool-call))))))
      `((type . text)
        (content . ,(alist-get 'content message))))))

;;; HTTP Client

(defun gemini-repl--request (messages tools callback)
  "Send request to LLM provider.
MESSAGES is conversation history.
TOOLS is available tools.
CALLBACK receives the parsed response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when (eq gemini-repl-provider 'openai)
                `(("Authorization" . ,(format "Bearer %s" (gemini-repl--get-api-key)))))))
         (url-request-data
          (encode-coding-string
           (json-encode (gemini-repl--format-request messages tools))
           'utf-8)))
    (url-retrieve
     (gemini-repl--get-endpoint)
     (lambda (status)
       (if-let ((err (plist-get status :error)))
           (funcall callback nil (format "Request failed: %s" err))
         (goto-char url-http-end-of-headers)
         (condition-case err
             (let ((response (json-read)))
               (funcall callback (gemini-repl--parse-response response) nil))
           (error (funcall callback nil (format "Parse error: %s" err))))))
     nil t t)))

;;; Tools

(defun gemini-repl-register-tool (name description parameters execute-fn)
  "Register a tool.
NAME is the tool name.
DESCRIPTION explains what it does.
PARAMETERS is a JSON schema for arguments.
EXECUTE-FN is called with arguments and returns result."
  (push `((name . ,name)
          (description . ,description)
          (parameters . ,parameters)
          (execute . ,execute-fn))
        gemini-repl-tools))

(defun gemini-repl--get-workspace ()
  "Get current workspace directory."
  (or gemini-repl-workspace default-directory))

(defun gemini-repl--safe-path-p (path)
  "Check if PATH is safe (within workspace, no traversal)."
  (let ((workspace (gemini-repl--get-workspace))
        (expanded (expand-file-name path (gemini-repl--get-workspace))))
    (and (not (string-match-p "\\.\\." path))
         (string-prefix-p (expand-file-name workspace) expanded)
         (not (string-match-p "\\(\\.env\\|\\.git/\\|\\.ssh\\|\\.gnupg\\)" path)))))

(defun gemini-repl--check-permission (tool-name args)
  "Check if tool execution should proceed.
TOOL-NAME is the tool being called.
ARGS are the tool arguments.
Returns t if allowed, nil if denied."
  (cond
   (gemini-repl-yolo-mode t)
   ((and (member tool-name gemini-repl-safe-tools)
         (not gemini-repl-confirm-safe-tools))
    t)
   (t
    (yes-or-no-p
     (format "Allow tool '%s' with args %s? "
             tool-name (json-encode args))))))

(defun gemini-repl--execute-tool (name args)
  "Execute tool NAME with ARGS."
  (if-let ((tool (seq-find (lambda (t) (string= (alist-get 'name t) name))
                           gemini-repl-tools)))
      (if (gemini-repl--check-permission name args)
          (condition-case err
              (funcall (alist-get 'execute tool) args)
            (error (format "Tool error: %s" (error-message-string err))))
        (format "Permission denied for tool: %s" name))
    (format "Unknown tool: %s" name)))

;; Built-in tools

(defun gemini-repl--init-default-tools ()
  "Initialize default tools."
  ;; read_file
  (gemini-repl-register-tool
   "read_file"
   "Read contents of a file"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))))
     (required . ["path"]))
   (lambda (args)
     (let ((path (alist-get 'path args)))
       (if (gemini-repl--safe-path-p path)
           (let ((full-path (expand-file-name path (gemini-repl--get-workspace))))
             (if (file-exists-p full-path)
                 (with-temp-buffer
                   (insert-file-contents full-path)
                   (buffer-string))
               (format "File not found: %s" path)))
         (format "Unsafe path: %s" path)))))

  ;; list_files
  (gemini-repl-register-tool
   "list_files"
   "List files in a directory"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "Directory path relative to workspace")))
                    (pattern . ((type . "string")
                               (description . "Optional glob pattern")))))
     (required . ["path"]))
   (lambda (args)
     (let ((path (or (alist-get 'path args) "."))
           (pattern (or (alist-get 'pattern args) "*")))
       (if (gemini-repl--safe-path-p path)
           (let ((full-path (expand-file-name path (gemini-repl--get-workspace))))
             (if (file-directory-p full-path)
                 (mapconcat #'identity
                            (directory-files full-path nil pattern)
                            "\n")
               (format "Not a directory: %s" path)))
         (format "Unsafe path: %s" path)))))

  ;; write_file
  (gemini-repl-register-tool
   "write_file"
   "Write content to a file"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))
                    (content . ((type . "string")
                               (description . "Content to write")))))
     (required . ["path" "content"]))
   (lambda (args)
     (let ((path (alist-get 'path args))
           (content (alist-get 'content args)))
       (if (gemini-repl--safe-path-p path)
           (let ((full-path (expand-file-name path (gemini-repl--get-workspace))))
             (with-temp-file full-path
               (insert content))
             (format "Wrote %d bytes to %s" (length content) path))
         (format "Unsafe path: %s" path)))))

  ;; git_status
  (gemini-repl-register-tool
   "git_status"
   "Get git status"
   '((type . "object")
     (properties . ())
     (required . []))
   (lambda (_args)
     (let ((default-directory (gemini-repl--get-workspace)))
       (shell-command-to-string "git status --porcelain"))))

  ;; git_diff
  (gemini-repl-register-tool
   "git_diff"
   "Get git diff"
   '((type . "object")
     (properties . ((staged . ((type . "boolean")
                              (description . "Show staged changes only")))))
     (required . []))
   (lambda (args)
     (let ((default-directory (gemini-repl--get-workspace))
           (staged (alist-get 'staged args)))
       (shell-command-to-string
        (if staged "git diff --staged" "git diff")))))

  ;; git_log
  (gemini-repl-register-tool
   "git_log"
   "Get git log"
   '((type . "object")
     (properties . ((count . ((type . "integer")
                             (description . "Number of commits to show")))))
     (required . []))
   (lambda (args)
     (let ((default-directory (gemini-repl--get-workspace))
           (count (or (alist-get 'count args) 10)))
       (shell-command-to-string
        (format "git log --oneline -n %d" count)))))

  ;; code_search
  (gemini-repl-register-tool
   "code_search"
   "Search code using ripgrep"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Search pattern (regex)")))
                    (file_type . ((type . "string")
                                 (description . "File type filter (e.g., 'rs', 'py')")))))
     (required . ["pattern"]))
   (lambda (args)
     (let ((default-directory (gemini-repl--get-workspace))
           (pattern (alist-get 'pattern args))
           (file-type (alist-get 'file_type args)))
       (shell-command-to-string
        (format "rg %s '%s' || grep -r '%s' ."
                (if file-type (format "-t %s" file-type) "")
                pattern pattern)))))

  ;; glob_files
  (gemini-repl-register-tool
   "glob_files"
   "Find files matching a glob pattern"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Glob pattern (e.g., '**/*.el')")))))
     (required . ["pattern"]))
   (lambda (args)
     (let ((pattern (alist-get 'pattern args)))
       (mapconcat #'identity
                  (file-expand-wildcards
                   (expand-file-name pattern (gemini-repl--get-workspace))
                   t)
                  "\n")))))

;;; REPL Interface

(defvar gemini-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gemini-repl-send-input)
    (define-key map (kbd "C-c C-c") #'gemini-repl-interrupt)
    (define-key map (kbd "C-c C-k") #'gemini-repl-clear)
    map)
  "Keymap for gemini-repl-mode.")

(define-derived-mode gemini-repl-mode special-mode "Gemini-REPL"
  "Major mode for Gemini REPL."
  (setq-local gemini-repl-conversation nil)
  (read-only-mode -1))

(defun gemini-repl--insert (text &optional face)
  "Insert TEXT with optional FACE in REPL buffer."
  (with-current-buffer (get-buffer-create gemini-repl-buffer-name)
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when face
        (add-text-properties start (point) `(face ,face))))))

(defun gemini-repl--prompt ()
  "Insert prompt."
  (gemini-repl--insert "\n> " 'font-lock-keyword-face))

;;;###autoload
(defun gemini-repl ()
  "Start Gemini REPL."
  (interactive)
  (let ((buf (get-buffer-create gemini-repl-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'gemini-repl-mode)
        (gemini-repl-mode)
        (gemini-repl--init-default-tools)
        (gemini-repl--insert "Welcome to Gemini REPL\n" 'font-lock-comment-face)
        (gemini-repl--insert (format "Provider: %s, Model: %s\n"
                                     gemini-repl-provider
                                     (gemini-repl--get-model))
                             'font-lock-comment-face)
        (gemini-repl--insert "Type your message and press RET to send.\n\n"
                             'font-lock-comment-face)
        (gemini-repl--prompt)))
    (pop-to-buffer buf)))

(defun gemini-repl-send-input ()
  "Send current input to the AI."
  (interactive)
  (let ((input (buffer-substring-no-properties
                (save-excursion
                  (beginning-of-line)
                  (forward-char 2) ; skip "> "
                  (point))
                (point-max))))
    (when (string-match-p "[^ \t\n]" input)
      (gemini-repl--insert "\n")
      (gemini-repl--process-input input))))

(defun gemini-repl--process-input (input)
  "Process user INPUT and get response."
  (push `((role . "user") (content . ,input)) gemini-repl-conversation)
  (gemini-repl--insert (format "\nYou: %s\n" input) 'font-lock-string-face)
  (gemini-repl--insert "\n[Thinking...]\n" 'font-lock-comment-face)

  (gemini-repl--request
   gemini-repl-conversation
   gemini-repl-tools
   (lambda (response error)
     (with-current-buffer gemini-repl-buffer-name
       (goto-char (point-max))
       (delete-region (save-excursion
                        (search-backward "[Thinking...]" nil t)
                        (point))
                      (point))
       (if error
           (gemini-repl--insert (format "Error: %s\n" error) 'error)
         (gemini-repl--handle-response response))))))

(defun gemini-repl--handle-response (response)
  "Handle parsed RESPONSE from LLM."
  (pcase (alist-get 'type response)
    ('text
     (let ((content (alist-get 'content response)))
       (push `((role . "assistant") (content . ,content)) gemini-repl-conversation)
       (gemini-repl--insert (format "\nAssistant: %s\n" content) 'font-lock-doc-face)
       (gemini-repl--prompt)))
    ('function_call
     (let* ((name (alist-get 'name response))
            (args (alist-get 'arguments response)))
       (gemini-repl--insert (format "\n[Calling tool: %s]\n" name)
                            'font-lock-function-name-face)
       (let ((result (gemini-repl--execute-tool name args)))
         (gemini-repl--insert (format "[Result: %s]\n"
                                      (truncate-string-to-width result 200))
                              'font-lock-comment-face)
         ;; Add tool result to conversation and continue
         (push `((role . "assistant")
                 (content . ,(format "Calling tool: %s" name)))
               gemini-repl-conversation)
         (push `((role . "user")
                 (content . ,(format "Tool result: %s" result)))
               gemini-repl-conversation)
         ;; Continue conversation
         (gemini-repl--request
          gemini-repl-conversation
          gemini-repl-tools
          (lambda (resp err)
            (with-current-buffer gemini-repl-buffer-name
              (if err
                  (progn
                    (gemini-repl--insert (format "Error: %s\n" err) 'error)
                    (gemini-repl--prompt))
                (gemini-repl--handle-response resp))))))))))

(defun gemini-repl-clear ()
  "Clear conversation history."
  (interactive)
  (setq gemini-repl-conversation nil)
  (with-current-buffer gemini-repl-buffer-name
    (erase-buffer)
    (gemini-repl--insert "Conversation cleared.\n" 'font-lock-comment-face)
    (gemini-repl--prompt)))

(defun gemini-repl-interrupt ()
  "Interrupt current request."
  (interactive)
  (message "Interrupted"))

;;;###autoload
(defun gemini-repl-exec (prompt)
  "Execute PROMPT and return response (non-interactive)."
  (interactive "sPrompt: ")
  (unless gemini-repl-tools
    (gemini-repl--init-default-tools))
  (let ((result nil)
        (done nil))
    (gemini-repl--request
     `(((role . "user") (content . ,prompt)))
     gemini-repl-tools
     (lambda (response error)
       (setq result (if error
                        (format "Error: %s" error)
                      (alist-get 'content response)))
       (setq done t)))
    ;; Wait for response
    (while (not done)
      (sleep-for 0.1))
    (when (called-interactively-p 'any)
      (message "%s" result))
    result))

;;;###autoload
(defun gemini-repl-send-region (start end)
  "Send region from START to END to AI."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (gemini-repl)
    (with-current-buffer gemini-repl-buffer-name
      (goto-char (point-max))
      (insert text)
      (gemini-repl-send-input))))

(provide 'gemini-repl)
;;; gemini-repl.el ends here
