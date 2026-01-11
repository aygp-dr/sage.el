;;; sage.el --- AI REPL with tool calling for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (request "0.3.2") (markdown-mode "2.5"))
;; Keywords: ai, tools, llm, gemini, ollama
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; sage provides an interactive AI REPL with tool calling capabilities.
;; It supports multiple providers (Gemini, Ollama, OpenAI) and includes
;; built-in tools for file operations, code search, and git commands.
;;
;; Features:
;; - Multi-provider support (Gemini, Ollama, OpenAI)
;; - Tool/function calling with permission system
;; - File operations (read, write, list)
;; - Git integration (status, diff, log, blame)
;; - Code search via ripgrep
;; - Session persistence and management
;; - Memory system for facts
;; - Slash commands for REPL control
;; - Emacs integration (region, buffer, org-mode)
;;
;; Usage:
;;   M-x sage        - Start REPL in dedicated buffer
;;   M-x sage-send   - Send region or prompt to AI
;;   M-x sage-exec   - Single-shot execution
;;
;; Slash Commands:
;;   /help                  - Show all commands
;;   /save [name]           - Save conversation
;;   /load <name>           - Load session
;;   /remember <key> <val>  - Remember fact
;;   /region                - Send region to AI
;;   /yank                  - Insert last response
;;
;; Configuration:
;;   (setq sage-api-key "your-api-key")
;;   (setq sage-provider 'gemini)  ; or 'ollama, 'openai

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'seq)
(require 'sage-ratelimit)
(require 'sage-memory)
(require 'sage-session)
(require 'sage-context)

;;; Version

(defconst sage-version "0.1.0"
  "Version of sage.el.")

;;; Customization

(defgroup sage nil
  "AI REPL with tool calling."
  :group 'tools
  :prefix "sage-")

(defcustom sage-api-key nil
  "API key for Gemini.
Can also be set via GEMINI_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'sage)

(defcustom sage-openai-api-key nil
  "API key for OpenAI.
Can also be set via OPENAI_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'sage)

(defcustom sage-provider 'gemini
  "LLM provider to use."
  :type '(choice (const :tag "Google Gemini" gemini)
                 (const :tag "Ollama (local)" ollama)
                 (const :tag "OpenAI" openai))
  :group 'sage)

(defcustom sage-model nil
  "Model to use. If nil, uses provider default."
  :type '(choice (const nil) string)
  :group 'sage)

(defcustom sage-ollama-host "http://localhost:11434"
  "Ollama server URL."
  :type 'string
  :group 'sage)

(defcustom sage-workspace nil
  "Workspace directory for file operations.
If nil, uses `default-directory'."
  :type '(choice (const nil) directory)
  :group 'sage)

(defcustom sage-yolo-mode nil
  "When non-nil, skip all tool permission checks."
  :type 'boolean
  :group 'sage)

(defcustom sage-confirm-safe-tools nil
  "When non-nil, confirm even safe (read-only) tools."
  :type 'boolean
  :group 'sage)

(defcustom sage-max-tool-iterations 10
  "Maximum number of tool call iterations per request."
  :type 'integer
  :group 'sage)

(defcustom sage-history-file
  (expand-file-name "sage-history" user-emacs-directory)
  "File to save conversation history."
  :type 'file
  :group 'sage)

(defcustom sage-use-memory t
  "When non-nil, include memory/facts in conversation context."
  :type 'boolean
  :group 'sage)

;;; Variables

(defvar sage-buffer-name "*sage*"
  "Name of the REPL buffer.")

(defvar sage-conversation nil
  "Current conversation history.")

(defvar sage-tools nil
  "Registered tools.")

(defvar sage-safe-tools
  '("read_file" "list_files" "git_status" "git_diff" "git_log"
    "git_branch" "git_blame" "code_search" "glob_files")
  "Tools that are safe (read-only) and don't require confirmation.")

(defvar sage-sessions (make-hash-table :test 'equal)
  "Hash table of saved sessions.
Keys are session names, values are conversation histories.")

(defvar sage-memory (make-hash-table :test 'equal)
  "Hash table of remembered facts.
Keys are fact names, values are fact content.")

(defvar sage-current-session nil
  "Name of the current session, if any.")

(defvar sage-last-response nil
  "Content of the last AI response for yanking.")

(defvar sage-token-count 0
  "Approximate token count for current session.")

(defvar sage-request-count 0
  "Number of requests in current session.")

;;; Provider API

(defun sage--get-api-key ()
  "Get API key for current provider."
  (pcase sage-provider
    ('gemini (or sage-api-key
                 (getenv "GEMINI_API_KEY")))
    ('openai (or sage-openai-api-key
                 (getenv "OPENAI_API_KEY")))
    ('ollama nil)))

(defun sage--get-model ()
  "Get model for current provider."
  (or sage-model
      (pcase sage-provider
        ('gemini "gemini-2.0-flash-exp")
        ('ollama "llama3.2")
        ('openai "gpt-4o"))))

(defun sage--get-endpoint ()
  "Get API endpoint for current provider."
  (pcase sage-provider
    ('gemini
     (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
             (sage--get-model)))
    ('ollama
     (format "%s/api/chat" sage-ollama-host))
    ('openai
     "https://api.openai.com/v1/chat/completions")))

(defun sage--format-request (messages tools)
  "Format request body for current provider.
MESSAGES is the conversation history.
TOOLS is the list of available tools."
  (pcase sage-provider
    ('gemini
     (sage--format-gemini-request messages tools))
    ('ollama
     (sage--format-ollama-request messages tools))
    ('openai
     (sage--format-openai-request messages tools))))

(defun sage--format-gemini-request (messages tools)
  "Format request for Gemini API."
  (let ((contents (mapcar (lambda (msg)
                            `((role . ,(if (string= (alist-get 'role msg) "user")
                                          "user" "model"))
                              (parts . [((text . ,(alist-get 'content msg)))])))
                          messages))
        (result nil))
    (setq result `((contents . ,(vconcat contents))))
    ;; Only add tools if we have a non-empty list
    (when (and tools (> (length tools) 0))
      (setq result (append result
                           `((tools . [((functionDeclarations . ,(vconcat (mapcar #'sage--tool-to-gemini tools))))])))))
    result))

(defun sage--tool-to-gemini (tool)
  "Convert TOOL to Gemini function declaration format."
  `((name . ,(alist-get 'name tool))
    (description . ,(alist-get 'description tool))
    (parameters . ,(alist-get 'parameters tool))))

(defun sage--format-ollama-request (messages tools)
  "Format request for Ollama API."
  (let ((ollama-messages (mapcar (lambda (msg)
                                   `((role . ,(alist-get 'role msg))
                                     (content . ,(alist-get 'content msg))))
                                 messages)))
    ;; Add tools as system prompt for Ollama
    (when tools
      (push `((role . "system")
              (content . ,(sage--tools-system-prompt tools)))
            ollama-messages))
    `((model . ,(sage--get-model))
      (messages . ,(vconcat ollama-messages))
      (stream . :json-false))))

(defun sage--tools-system-prompt (tools)
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

(defun sage--format-openai-request (messages tools)
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
    `((model . ,(sage--get-model))
      (messages . ,(vconcat openai-messages))
      ,@(when tools `((tools . ,(vconcat openai-tools)))))))

(defun sage--parse-response (response)
  "Parse RESPONSE from current provider."
  (pcase sage-provider
    ('gemini (sage--parse-gemini-response response))
    ('ollama (sage--parse-ollama-response response))
    ('openai (sage--parse-openai-response response))))

(defun sage--parse-gemini-response (response)
  "Parse Gemini API response."
  (let* ((candidates (alist-get 'candidates response))
         (content (alist-get 'content (aref candidates 0)))
         (parts (alist-get 'parts content))
         (part (aref parts 0)))
    (if-let* ((function-call (alist-get 'functionCall part)))
        `((type . function_call)
          (name . ,(alist-get 'name function-call))
          (arguments . ,(alist-get 'args function-call)))
      `((type . text)
        (content . ,(alist-get 'text part))))))

(defun sage--parse-ollama-response (response)
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

(defun sage--parse-openai-response (response)
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

(defun sage--request (messages tools callback)
  "Send request to LLM provider with rate limiting.
MESSAGES is conversation history.
TOOLS is available tools.
CALLBACK receives the parsed response."
  (let ((model (sage--get-model)))
    ;; Apply rate limiting
    (sage-ratelimit-wrap-request
     model
     (lambda ()
       (let* ((url-request-method "POST")
              (url-request-extra-headers
               `(("Content-Type" . "application/json")
                 ,@(pcase sage-provider
                     ('openai `(("Authorization" . ,(format "Bearer %s" (sage--get-api-key)))))
                     ('gemini `(("x-goog-api-key" . ,(sage--get-api-key)))))))
              (url-request-data
               (encode-coding-string
                (json-encode (sage--format-request messages tools))
                'utf-8)))
         (url-retrieve
          (sage--get-endpoint)
          (lambda (status)
            (if-let* ((err (plist-get status :error)))
                (funcall callback nil (format "Request failed: %s" err))
              (goto-char url-http-end-of-headers)
              (condition-case err
                  (let ((response (json-read)))
                    (funcall callback (sage--parse-response response) nil))
                (error (funcall callback nil (format "Parse error: %s" err))))))
          nil t t))))))

;;; Tools

(defun sage-register-tool (name description parameters execute-fn)
  "Register a tool.
NAME is the tool name.
DESCRIPTION explains what it does.
PARAMETERS is a JSON schema for arguments.
EXECUTE-FN is called with arguments and returns result."
  (push `((name . ,name)
          (description . ,description)
          (parameters . ,parameters)
          (execute . ,execute-fn))
        sage-tools))

(defun sage--get-workspace ()
  "Get current workspace directory."
  (or sage-workspace default-directory))

(defun sage--safe-path-p (path)
  "Check if PATH is safe (within workspace, no traversal)."
  (let ((workspace (sage--get-workspace))
        (expanded (expand-file-name path (sage--get-workspace))))
    (and (not (string-match-p "\\.\\." path))
         (string-prefix-p (expand-file-name workspace) expanded)
         (not (string-match-p "\\(\\.env\\|\\.git/\\|\\.ssh\\|\\.gnupg\\)" path)))))

(defun sage--check-permission (tool-name args)
  "Check if tool execution should proceed.
TOOL-NAME is the tool being called.
ARGS are the tool arguments.
Returns t if allowed, nil if denied."
  (cond
   (sage-yolo-mode t)
   ((and (member tool-name sage-safe-tools)
         (not sage-confirm-safe-tools))
    t)
   (t
    (yes-or-no-p
     (format "Allow tool '%s' with args %s? "
             tool-name (json-encode args))))))

(defun sage--execute-tool (name args)
  "Execute tool NAME with ARGS."
  (if-let* ((tool (seq-find (lambda (tl) (string= (alist-get 'name tl) name))
                           sage-tools)))
      (if (sage--check-permission name args)
          (condition-case err
              (funcall (alist-get 'execute tool) args)
            (error (format "Tool error: %s" (error-message-string err))))
        (format "Permission denied for tool: %s" name))
    (format "Unknown tool: %s" name)))

;; Built-in tools

(defun sage--init-default-tools ()
  "Initialize default tools."
  ;; read_file
  (sage-register-tool
   "read_file"
   "Read contents of a file"
   '((type . "object")
     (properties . ((path . ((type . "string")
                             (description . "File path relative to workspace")))))
     (required . ["path"]))
   (lambda (args)
     (let ((path (alist-get 'path args)))
       (if (sage--safe-path-p path)
           (let ((full-path (expand-file-name path (sage--get-workspace))))
             (if (file-exists-p full-path)
                 (with-temp-buffer
                   (insert-file-contents full-path)
                   (buffer-string))
               (format "File not found: %s" path)))
         (format "Unsafe path: %s" path)))))

  ;; list_files
  (sage-register-tool
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
       (if (sage--safe-path-p path)
           (let ((full-path (expand-file-name path (sage--get-workspace))))
             (if (file-directory-p full-path)
                 (mapconcat #'identity
                            (directory-files full-path nil pattern)
                            "\n")
               (format "Not a directory: %s" path)))
         (format "Unsafe path: %s" path)))))

  ;; write_file
  (sage-register-tool
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
       (if (sage--safe-path-p path)
           (let ((full-path (expand-file-name path (sage--get-workspace))))
             (with-temp-file full-path
               (insert content))
             (format "Wrote %d bytes to %s" (length content) path))
         (format "Unsafe path: %s" path)))))

  ;; git_status
  (sage-register-tool
   "git_status"
   "Get git status"
   '((type . "object")
     (properties . ())
     (required . []))
   (lambda (_args)
     (let ((default-directory (sage--get-workspace)))
       (shell-command-to-string "git status --porcelain"))))

  ;; git_diff
  (sage-register-tool
   "git_diff"
   "Get git diff"
   '((type . "object")
     (properties . ((staged . ((type . "boolean")
                              (description . "Show staged changes only")))))
     (required . []))
   (lambda (args)
     (let ((default-directory (sage--get-workspace))
           (staged (alist-get 'staged args)))
       (shell-command-to-string
        (if staged "git diff --staged" "git diff")))))

  ;; git_log
  (sage-register-tool
   "git_log"
   "Get git log"
   '((type . "object")
     (properties . ((count . ((type . "integer")
                             (description . "Number of commits to show")))))
     (required . []))
   (lambda (args)
     (let ((default-directory (sage--get-workspace))
           (count (or (alist-get 'count args) 10)))
       (shell-command-to-string
        (format "git log --oneline -n %d" count)))))

  ;; code_search
  (sage-register-tool
   "code_search"
   "Search code using ripgrep"
   '((type . "object")
     (properties . ((pattern . ((type . "string")
                               (description . "Search pattern (regex)")))
                    (file_type . ((type . "string")
                                 (description . "File type filter (e.g., 'rs', 'py')")))))
     (required . ["pattern"]))
   (lambda (args)
     (let ((default-directory (sage--get-workspace))
           (pattern (alist-get 'pattern args))
           (file-type (alist-get 'file_type args)))
       (shell-command-to-string
        (format "rg %s '%s' || grep -r '%s' ."
                (if file-type (format "-t %s" file-type) "")
                pattern pattern)))))

  ;; glob_files
  (sage-register-tool
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
                   (expand-file-name pattern (sage--get-workspace))
                   t)
                  "\n")))))

;;; Slash Commands

(defun sage--command-help ()
  "Display help for slash commands."
  (concat
   "Available Commands:\n\n"
   "SESSION:\n"
   "  /save [name]          - Save conversation\n"
   "  /load <name>          - Load session\n"
   "  /sessions             - List sessions\n"
   "  /delete <name>        - Delete session\n"
   "  /export [format] [file] - Export (json/markdown)\n"
   "  /reset                - Clear conversation\n\n"
   "MEMORY:\n"
   "  /memory               - List facts\n"
   "  /remember <key> <value> - Add fact\n"
   "  /forget <key>         - Remove fact\n\n"
   "RATE LIMITING:\n"
   "  /ratelimit            - Show rate limit status\n"
   "  /ratelimit-reset      - Reset rate limit history\n\n"
   "INFO:\n"
   "  /help                 - Show commands\n"
   "  /tools                - List tools\n"
   "  /model                - Show model info\n"
   "  /stats                - Show statistics\n"
   "  /tokens               - Token usage\n"
   "  /context              - Context usage info\n"
   "  /version              - Show version info\n\n"
   "EMACS-SPECIFIC:\n"
   "  /region               - Send region to AI\n"
   "  /buffer               - Send buffer to AI\n"
   "  /org                  - Send org subtree to AI\n"
   "  /yank                 - Yank last response\n"))

(defun sage--command-save (&optional name)
  "Save current conversation as NAME."
  (let ((session-name (or name
                          (format-time-string "session-%Y%m%d-%H%M%S"))))
    (puthash session-name (copy-sequence sage-conversation)
             sage-sessions)
    (setq sage-current-session session-name)
    (format "Session saved as '%s'" session-name)))

(defun sage--command-load (name)
  "Load conversation from session NAME."
  (if-let* ((session (gethash name sage-sessions)))
      (progn
        (setq sage-conversation (copy-sequence session))
        (setq sage-current-session name)
        (format "Session '%s' loaded (%d messages)"
                name (length sage-conversation)))
    (format "Session '%s' not found" name)))

(defun sage--command-sessions ()
  "List all saved sessions."
  (if (zerop (hash-table-count sage-sessions))
      "No saved sessions"
    (let ((sessions nil))
      (maphash (lambda (name conv)
                 (push (format "%s%s (%d messages)"
                               name
                               (if (equal name sage-current-session)
                                   " [current]" "")
                               (length conv))
                       sessions))
               sage-sessions)
      (concat "Saved sessions:\n"
              (mapconcat #'identity (nreverse sessions) "\n")))))

(defun sage--command-delete (name)
  "Delete session NAME."
  (if (gethash name sage-sessions)
      (progn
        (remhash name sage-sessions)
        (when (equal name sage-current-session)
          (setq sage-current-session nil))
        (format "Session '%s' deleted" name))
    (format "Session '%s' not found" name)))

(defun sage--command-export (&optional format file)
  "Export current conversation to FILE in FORMAT."
  (let ((export-format (or format "json"))
        (export-file (or file
                         (format-time-string "gemini-export-%Y%m%d-%H%M%S.%s"
                                             (if (equal format "markdown") "md" "json")))))
    (pcase export-format
      ("json"
       (with-temp-file export-file
         (insert (json-encode sage-conversation)))
       (format "Exported to %s (JSON)" export-file))
      ("markdown"
       (with-temp-file export-file
         (insert "# Sage Conversation\n\n")
         (dolist (msg (reverse sage-conversation))
           (insert (format "## %s\n\n%s\n\n"
                           (capitalize (alist-get 'role msg))
                           (alist-get 'content msg)))))
       (format "Exported to %s (Markdown)" export-file))
      (_
       (format "Unknown format: %s (use 'json' or 'markdown')" export-format)))))

(defun sage--command-reset ()
  "Clear conversation history."
  (setq sage-conversation nil)
  (setq sage-token-count 0)
  (setq sage-request-count 0)
  (setq sage-current-session nil)
  "Conversation cleared")

(defun sage--command-memory ()
  "List all remembered facts."
  (if (zerop (hash-table-count sage-memory))
      "No facts in memory"
    (let ((facts nil))
      (maphash (lambda (key value)
                 (push (format "%s: %s" key value) facts))
               sage-memory)
      (concat "Remembered facts:\n"
              (mapconcat #'identity (nreverse facts) "\n")))))

(defun sage--command-remember (key value)
  "Remember VALUE under KEY."
  (puthash key value sage-memory)
  (format "Remembered: %s = %s" key value))

(defun sage--command-forget (key)
  "Forget fact KEY."
  (if (gethash key sage-memory)
      (progn
        (remhash key sage-memory)
        (format "Forgot: %s" key))
    (format "No fact named '%s'" key)))

(defun sage--command-tools ()
  "List all registered tools."
  (if (null sage-tools)
      "No tools registered"
    (concat "Registered tools:\n"
            (mapconcat (lambda (tool)
                         (format "- %s: %s"
                                 (alist-get 'name tool)
                                 (alist-get 'description tool)))
                       (reverse sage-tools)
                       "\n"))))

(defun sage--command-model ()
  "Show current model information."
  (format "Provider: %s\nModel: %s\nEndpoint: %s"
          sage-provider
          (sage--get-model)
          (sage--get-endpoint)))

(defun sage--command-stats ()
  "Show session statistics."
  (format "Session Statistics:\nMessages: %d\nRequests: %d\nApprox. tokens: %d\nSession: %s"
          (length sage-conversation)
          sage-request-count
          sage-token-count
          (or sage-current-session "unsaved")))

(defun sage--command-tokens ()
  "Show token usage information."
  (format "Token Usage:\nCurrent session: ~%d tokens\nMessages: %d\nAvg per message: ~%d tokens"
          sage-token-count
          (length sage-conversation)
          (if (zerop (length sage-conversation))
              0
            (/ sage-token-count (length sage-conversation)))))

(defun sage--command-context ()
  "Show context usage information.
Displays token count, max tokens, usage percentage, message count,
and compaction status."
  (let* ((model (sage--get-model))
         (stats (sage-context-tokens sage-conversation))
         (total-tokens (alist-get 'total stats))
         (msg-count (alist-get 'count stats))
         (max-tokens (sage-context-get-max-tokens model))
         (usage-pct (* 100 (/ (float total-tokens) max-tokens)))
         (needs-compaction (sage-context-needs-compaction-p sage-conversation max-tokens))
         (needs-warning (sage-context-needs-warning-p sage-conversation)))
    (concat
     (format "Context Usage:\n")
     (format "Model: %s\n" model)
     (format "Tokens: ~%s / %s (%.1f%%)\n"
             (sage-context-format-number total-tokens)
             (sage-context-format-number max-tokens)
             usage-pct)
     (format "Messages: %d\n" msg-count)
     (format "Compaction: %s\n"
             (cond
              (needs-compaction
               (format "NEEDED (threshold: %.0f%%)"
                       (* 100 sage-context-compaction-threshold)))
              (needs-warning
               (format "warning (at %.0f%%, threshold: %.0f%%)"
                       usage-pct
                       (* 100 sage-context-warning-threshold)))
              (t "not needed")))
     (format "Auto-compact: %s\n" (if sage-context-auto-compact "enabled" "disabled"))
     (format "Strategy: %s" sage-context-default-strategy))))

(defun sage--command-version ()
  "Show version information."
  (format "sage.el version: %s\nEmacs version: %s\nProvider: %s\nModel: %s"
          sage-version
          emacs-version
          sage-provider
          (sage--get-model)))

(defun sage--command-region ()
  "Send current region to AI."
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (with-current-buffer sage-buffer-name
          (goto-char (point-max))
          (insert text)
          (sage-send-input))
        "Region sent to AI")
    "No active region"))

(defun sage--command-buffer ()
  "Send current buffer to AI."
  (let ((text (with-current-buffer (other-buffer (current-buffer) t)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert text)
      (sage-send-input))
    "Buffer sent to AI"))

(defun sage--command-org ()
  "Send current org subtree to AI."
  (if (derived-mode-p 'org-mode)
      (let ((text (save-excursion
                    (org-back-to-heading t)
                    (let ((start (point)))
                      (org-end-of-subtree t t)
                      (buffer-substring-no-properties start (point))))))
        (with-current-buffer sage-buffer-name
          (goto-char (point-max))
          (insert text)
          (sage-send-input))
        "Org subtree sent to AI")
    "Not in org-mode"))

(defun sage--command-yank ()
  "Yank last AI response."
  (if sage-last-response
      (progn
        (with-current-buffer (other-buffer (current-buffer) t)
          (insert sage-last-response))
        "Last response yanked")
    "No response to yank"))

(defun sage--dispatch-command (input)
  "Dispatch slash command INPUT.
Returns result string if command was handled, nil otherwise."
  (when (string-prefix-p "/" input)
    (let* ((parts (split-string (substring input 1) " " t))
           (cmd (car parts))
           (args (cdr parts)))
      (pcase cmd
        ;; SESSION
        ("save" (sage--command-save (car args)))
        ("load" (if args
                    (sage--command-load (car args))
                  "Usage: /load <name>"))
        ("sessions" (sage--command-sessions))
        ("delete" (if args
                      (sage--command-delete (car args))
                    "Usage: /delete <name>"))
        ("export" (sage--command-export (car args) (cadr args)))
        ("reset" (sage--command-reset))

        ;; MEMORY
        ("memory" (sage--command-memory))
        ("remember" (if (>= (length args) 2)
                        (sage--command-remember
                         (car args)
                         (mapconcat #'identity (cdr args) " "))
                      "Usage: /remember <key> <value>"))
        ("forget" (if args
                      (sage--command-forget (car args))
                    "Usage: /forget <key>"))

        ;; RATE LIMITING
        ("ratelimit" (sage-ratelimit-status (sage--get-model)))
        ("ratelimit-reset" (progn
                             (sage-ratelimit-reset (sage--get-model))
                             (format "Rate limit reset for %s" (sage--get-model))))

        ;; INFO
        ("help" (sage--command-help))
        ("tools" (sage--command-tools))
        ("model" (sage--command-model))
        ("stats" (sage--command-stats))
        ("tokens" (sage--command-tokens))
        ("context" (sage--command-context))
        ("version" (sage--command-version))

        ;; EMACS-SPECIFIC
        ("region" (sage--command-region))
        ("buffer" (sage--command-buffer))
        ("org" (sage--command-org))
        ("yank" (sage--command-yank))

        ;; Unknown command
        (_ (format "Unknown command: /%s\nType /help for available commands" cmd))))))

;;; REPL Interface

(defvar sage-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "RET") #'sage-send-input)
    (define-key map (kbd "C-c C-c") #'sage-interrupt)
    (define-key map (kbd "C-c C-k") #'sage-clear)
    map)
  "Keymap for sage-mode.")

(define-derived-mode sage-mode text-mode "Sage"
  "Major mode for Sage REPL.
Allows text input and sends messages on RET."
  (setq-local sage-conversation nil))

(defun sage--insert (text &optional face)
  "Insert TEXT with optional FACE in REPL buffer."
  (with-current-buffer (get-buffer-create sage-buffer-name)
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when face
        (add-text-properties start (point) `(face ,face))))))

(defun sage--prompt ()
  "Insert prompt."
  (sage--insert "\n> " 'font-lock-keyword-face))

;;;###autoload
(defun sage ()
  "Start Sage."
  (interactive)
  (let ((buf (get-buffer-create sage-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'sage-mode)
        (sage-mode)
        (sage--init-default-tools)
        (sage--insert "Welcome to Sage\n" 'font-lock-comment-face)
        (sage--insert (format "Provider: %s, Model: %s\n"
                                     sage-provider
                                     (sage--get-model))
                             'font-lock-comment-face)
        (sage--insert "Type your message and press RET to send.\n\n"
                             'font-lock-comment-face)
        (sage--prompt)))
    (pop-to-buffer buf)))

(defun sage-send-input ()
  "Send current input to the AI or execute slash command."
  (interactive)
  (let ((input (buffer-substring-no-properties
                (save-excursion
                  (beginning-of-line)
                  (forward-char 2) ; skip "> "
                  (point))
                (point-max))))
    (when (string-match-p "[^ \t\n]" input)
      (sage--insert "\n")
      ;; Check for slash commands first
      (if-let* ((result (sage--dispatch-command input)))
          (progn
            (sage--insert (format "[Command] %s\n\n" input) 'font-lock-keyword-face)
            (sage--insert (format "%s\n" result) 'font-lock-comment-face)
            (sage--prompt))
        ;; Not a command, process as normal AI input
        (sage--process-input input)))))

(defun sage--process-input (input)
  "Process user INPUT and get response."
  ;; Add memory context to first message if enabled and conversation is empty/new
  (when (and sage-use-memory
             (or (null sage-conversation)
                 (= (length sage-conversation) 0)))
    (when-let* ((memory-context (sage-memory-to-context)))
      (unless (string-empty-p memory-context)
        (push `((role . "system") (content . ,memory-context))
              sage-conversation))))

  (push `((role . "user") (content . ,input)) sage-conversation)
  (sage--insert (format "\nYou: %s\n" input) 'font-lock-string-face)
  (sage--insert "\n[Thinking...]\n" 'font-lock-comment-face)

  ;; Update statistics
  (setq sage-request-count (1+ sage-request-count))
  (setq sage-token-count (+ sage-token-count (/ (length input) 4))) ; rough estimate

  (sage--request
   sage-conversation
   sage-tools
   (lambda (response error)
     (with-current-buffer sage-buffer-name
       (goto-char (point-max))
       (delete-region (save-excursion
                        (search-backward "[Thinking...]" nil t)
                        (point))
                      (point))
       (if error
           (sage--insert (format "Error: %s\n" error) 'error)
         (sage--handle-response response))))))

(defun sage--handle-response (response)
  "Handle parsed RESPONSE from LLM."
  (pcase (alist-get 'type response)
    ('text
     (let ((content (alist-get 'content response)))
       (push `((role . "assistant") (content . ,content)) sage-conversation)
       ;; Store last response for yanking
       (setq sage-last-response content)
       ;; Update token count
       (setq sage-token-count (+ sage-token-count (/ (length content) 4)))
       (sage--insert (format "\nAssistant: %s\n" content) 'font-lock-doc-face)
       (sage--prompt)))
    ('function_call
     (let* ((name (alist-get 'name response))
            (args (alist-get 'arguments response)))
       (sage--insert (format "\n[Calling tool: %s]\n" name)
                            'font-lock-function-name-face)
       (let ((result (sage--execute-tool name args)))
         (sage--insert (format "[Result: %s]\n"
                                      (truncate-string-to-width result 200))
                              'font-lock-comment-face)
         ;; Add tool result to conversation and continue
         (push `((role . "assistant")
                 (content . ,(format "Calling tool: %s" name)))
               sage-conversation)
         (push `((role . "user")
                 (content . ,(format "Tool result: %s" result)))
               sage-conversation)
         ;; Continue conversation
         (sage--request
          sage-conversation
          sage-tools
          (lambda (resp err)
            (with-current-buffer sage-buffer-name
              (if err
                  (progn
                    (sage--insert (format "Error: %s\n" err) 'error)
                    (sage--prompt))
                (sage--handle-response resp))))))))))

(defun sage-clear ()
  "Clear conversation history."
  (interactive)
  (setq sage-conversation nil)
  (setq sage-token-count 0)
  (setq sage-request-count 0)
  (setq sage-current-session nil)
  (with-current-buffer sage-buffer-name
    (erase-buffer)
    (sage--insert "Conversation cleared.\n" 'font-lock-comment-face)
    (sage--prompt)))

(defun sage-interrupt ()
  "Interrupt current request."
  (interactive)
  (message "Interrupted"))

;;;###autoload
(defun sage-exec (prompt)
  "Execute PROMPT and return response (non-interactive)."
  (interactive "sPrompt: ")
  (unless sage-tools
    (sage--init-default-tools))
  (let ((result nil)
        (done nil))
    (sage--request
     `(((role . "user") (content . ,prompt)))
     sage-tools
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
(defun sage-send-region (start end)
  "Send region from START to END to AI."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (sage)
    (with-current-buffer sage-buffer-name
      (goto-char (point-max))
      (insert text)
      (sage-send-input))))

(provide 'sage)
;;; sage.el ends here
