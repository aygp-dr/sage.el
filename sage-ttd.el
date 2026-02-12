;;; sage-ttd.el --- Time-travel debugging for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (sage "0.1.0"))
;; Keywords: ai, tools, debugging, time-travel
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; sage-ttd provides time-travel debugging for sage conversations.
;;
;; This is a MOCK EXTENSION demonstrating the sage extension architecture.
;; It shows how third-party packages integrate with sage using:
;; - Tool registration
;; - Slash command registration
;; - Hook integration
;; - Filter functions
;; - Keymap extension
;;
;; Features (stubbed):
;; - Checkpoint conversation state at meaningful moments
;; - Branch conversations to explore alternatives
;; - Restore to previous checkpoints
;; - Visualize conversation timeline
;; - Replay tool calls with modified parameters
;;
;; Usage:
;;   (require 'sage-ttd)
;;
;;   ;; In sage REPL:
;;   /checkpoint save-point-1    ; Create named checkpoint
;;   /restore save-point-1       ; Restore to checkpoint
;;   /timeline                   ; View conversation tree
;;   /branch experiment          ; Create new branch
;;
;;   ;; Or interactively:
;;   M-x sage-ttd-checkpoint     ; Create checkpoint
;;   M-x sage-ttd-restore        ; Restore with completion
;;   M-x sage-ttd-timeline       ; Visualize history

;;; Code:

(require 'cl-lib)
(require 'sage-ext)

;; Soft dependency on sage - we use with-eval-after-load for integration
(declare-function sage-conversation "sage" ())
(declare-function sage-register-tool "sage" (name description parameters execute-fn))

;;; Customization

(defgroup sage-ttd nil
  "Time-travel debugging for sage."
  :group 'sage
  :prefix "sage-ttd-")

(defcustom sage-ttd-auto-checkpoint t
  "When non-nil, automatically checkpoint before tool calls."
  :type 'boolean
  :group 'sage-ttd)

(defcustom sage-ttd-auto-checkpoint-interval nil
  "When non-nil, auto-checkpoint every N messages."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Message count"))
  :group 'sage-ttd)

(defcustom sage-ttd-max-checkpoints 50
  "Maximum number of checkpoints to retain per branch."
  :type 'integer
  :group 'sage-ttd)

(defcustom sage-ttd-storage-directory
  (expand-file-name "sage/ttd" user-emacs-directory)
  "Directory for TTD checkpoint storage."
  :type 'directory
  :group 'sage-ttd)

;;; Data Structures

(cl-defstruct (sage-ttd-checkpoint
               (:constructor sage-ttd-checkpoint--create)
               (:copier nil))
  "A conversation checkpoint."
  (id nil :type string
      :documentation "Unique identifier (content hash)")
  (name nil :type string
        :documentation "Human-readable name")
  (timestamp nil :type string
             :documentation "ISO 8601 creation time")
  (parent-id nil :type string
             :documentation "Parent checkpoint ID")
  (branch nil :type string
          :documentation "Branch name")
  (conversation nil :type list
                :documentation "Snapshot of sage-conversation")
  (tool-history nil :type list
                :documentation "Tool calls up to this point")
  (metadata nil :type list
            :documentation "Additional state"))

(cl-defstruct (sage-ttd-branch
               (:constructor sage-ttd-branch--create)
               (:copier nil))
  "A conversation branch."
  (name nil :type string
        :documentation "Branch name")
  (head-id nil :type string
           :documentation "ID of branch tip checkpoint")
  (created-at nil :type string
              :documentation "When branch was created")
  (description nil :type string
               :documentation "Why this branch exists"))

;;; State Variables

(defvar sage-ttd--checkpoints (make-hash-table :test 'equal)
  "Hash table of checkpoint-id -> sage-ttd-checkpoint.")

(defvar sage-ttd--branches nil
  "Alist of branch-name -> sage-ttd-branch.")

(defvar sage-ttd--current-branch "main"
  "Name of the current branch.")

(defvar sage-ttd--current-checkpoint-id nil
  "ID of the current checkpoint (branch head).")

(defvar sage-ttd--tool-history nil
  "List of tool calls in current session for replay.")

(defvar sage-ttd--message-count 0
  "Count of messages since last checkpoint.")

;;; Internal Helpers

(defun sage-ttd--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun sage-ttd--generate-id (content)
  "Generate a content-addressed ID for CONTENT."
  (secure-hash 'sha256 (format "%s" content)))

(defun sage-ttd--ensure-directory ()
  "Ensure TTD storage directory exists."
  (unless (file-directory-p sage-ttd-storage-directory)
    (make-directory sage-ttd-storage-directory t)))

(defun sage-ttd--init-main-branch ()
  "Initialize the main branch if it doesn't exist."
  (unless (assoc "main" sage-ttd--branches)
    (push (cons "main"
                (sage-ttd-branch--create
                 :name "main"
                 :head-id nil
                 :created-at (sage-ttd--timestamp)
                 :description "Default branch"))
          sage-ttd--branches)))

;;; Checkpoint Operations (Stubs)

(defun sage-ttd--create-checkpoint (name)
  "Create a checkpoint with NAME.
Returns the checkpoint ID."
  ;; STUB: In full implementation, would snapshot sage-conversation
  (let* ((conversation (when (boundp 'sage-conversation) sage-conversation))
         (content (list conversation sage-ttd--tool-history (current-time)))
         (id (sage-ttd--generate-id content))
         (checkpoint (sage-ttd-checkpoint--create
                      :id id
                      :name name
                      :timestamp (sage-ttd--timestamp)
                      :parent-id sage-ttd--current-checkpoint-id
                      :branch sage-ttd--current-branch
                      :conversation conversation
                      :tool-history (copy-sequence sage-ttd--tool-history)
                      :metadata nil)))
    ;; Store checkpoint
    (puthash id checkpoint sage-ttd--checkpoints)
    ;; Update branch head
    (let ((branch (cdr (assoc sage-ttd--current-branch sage-ttd--branches))))
      (when branch
        (setf (sage-ttd-branch-head-id branch) id)))
    ;; Update current
    (setq sage-ttd--current-checkpoint-id id)
    (setq sage-ttd--message-count 0)
    (message "TTD: Created checkpoint '%s' (id: %s...)" name (substring id 0 8))
    id))

(defun sage-ttd--restore-checkpoint (id)
  "Restore conversation to checkpoint ID.
Returns t on success, nil on failure."
  ;; STUB: In full implementation, would restore sage-conversation
  (let ((checkpoint (gethash id sage-ttd--checkpoints)))
    (if (null checkpoint)
        (progn
          (message "TTD: Checkpoint not found: %s" id)
          nil)
      ;; Would restore: (setq sage-conversation (sage-ttd-checkpoint-conversation checkpoint))
      (setq sage-ttd--current-checkpoint-id id)
      (setq sage-ttd--tool-history (sage-ttd-checkpoint-tool-history checkpoint))
      (setq sage-ttd--current-branch (sage-ttd-checkpoint-branch checkpoint))
      (message "TTD: Restored to '%s' on branch '%s'"
               (sage-ttd-checkpoint-name checkpoint)
               sage-ttd--current-branch)
      t)))

(defun sage-ttd--list-checkpoints (&optional branch)
  "List checkpoints, optionally filtered by BRANCH."
  (let ((result nil))
    (maphash (lambda (_id cp)
               (when (or (null branch)
                         (equal branch (sage-ttd-checkpoint-branch cp)))
                 (push cp result)))
             sage-ttd--checkpoints)
    (sort result (lambda (a b)
                   (string< (sage-ttd-checkpoint-timestamp b)
                            (sage-ttd-checkpoint-timestamp a))))))

(defun sage-ttd--create-branch (name &optional description)
  "Create a new branch NAME from current checkpoint."
  (if (assoc name sage-ttd--branches)
      (progn
        (message "TTD: Branch '%s' already exists" name)
        nil)
    (let ((branch (sage-ttd-branch--create
                   :name name
                   :head-id sage-ttd--current-checkpoint-id
                   :created-at (sage-ttd--timestamp)
                   :description description)))
      (push (cons name branch) sage-ttd--branches)
      (setq sage-ttd--current-branch name)
      (message "TTD: Created and switched to branch '%s'" name)
      name)))

(defun sage-ttd--switch-branch (name)
  "Switch to branch NAME."
  (let ((branch (cdr (assoc name sage-ttd--branches))))
    (if (null branch)
        (progn
          (message "TTD: Branch not found: %s" name)
          nil)
      (setq sage-ttd--current-branch name)
      (when (sage-ttd-branch-head-id branch)
        (sage-ttd--restore-checkpoint (sage-ttd-branch-head-id branch)))
      (message "TTD: Switched to branch '%s'" name)
      t)))

;;; Tool Definitions

(defun sage-ttd--tool-checkpoint (args)
  "Tool: Create a conversation checkpoint.
ARGS should contain `name' (optional)."
  (let ((name (or (alist-get 'name args)
                  (format "checkpoint-%s" (sage-ttd--timestamp)))))
    (sage-ttd--create-checkpoint name)
    (format "Created checkpoint: %s" name)))

(defun sage-ttd--tool-restore (args)
  "Tool: Restore to a checkpoint.
ARGS should contain `name' or `id'."
  (let ((name (alist-get 'name args))
        (id (alist-get 'id args)))
    (if id
        (if (sage-ttd--restore-checkpoint id)
            (format "Restored to checkpoint %s" id)
          (format "Failed to restore: checkpoint not found"))
      (if name
          (let ((cp (cl-find-if (lambda (c)
                                  (equal name (sage-ttd-checkpoint-name c)))
                                (sage-ttd--list-checkpoints))))
            (if cp
                (progn
                  (sage-ttd--restore-checkpoint (sage-ttd-checkpoint-id cp))
                  (format "Restored to checkpoint: %s" name))
              (format "Checkpoint not found: %s" name)))
        "Error: Must provide name or id"))))

(defun sage-ttd--tool-list-checkpoints (_args)
  "Tool: List available checkpoints."
  (let ((checkpoints (sage-ttd--list-checkpoints)))
    (if (null checkpoints)
        "No checkpoints available."
      (mapconcat
       (lambda (cp)
         (format "- %s (%s) [%s]"
                 (sage-ttd-checkpoint-name cp)
                 (substring (sage-ttd-checkpoint-id cp) 0 8)
                 (sage-ttd-checkpoint-branch cp)))
       checkpoints
       "\n"))))

(defun sage-ttd--tool-branch (args)
  "Tool: Create or switch branch.
ARGS should contain `name' and optionally `description'."
  (let ((name (alist-get 'name args))
        (description (alist-get 'description args)))
    (if (assoc name sage-ttd--branches)
        (progn
          (sage-ttd--switch-branch name)
          (format "Switched to existing branch: %s" name))
      (sage-ttd--create-branch name description)
      (format "Created and switched to branch: %s" name))))

(defun sage-ttd--tool-timeline (_args)
  "Tool: Show conversation timeline."
  (let ((checkpoints (sage-ttd--list-checkpoints))
        (branches sage-ttd--branches))
    (format "Current branch: %s\n\nBranches:\n%s\n\nRecent checkpoints:\n%s"
            sage-ttd--current-branch
            (mapconcat (lambda (b)
                         (format "- %s%s"
                                 (car b)
                                 (if (equal (car b) sage-ttd--current-branch)
                                     " *" "")))
                       branches "\n")
            (mapconcat
             (lambda (cp)
               (format "  [%s] %s (%s)"
                       (substring (sage-ttd-checkpoint-id cp) 0 8)
                       (sage-ttd-checkpoint-name cp)
                       (sage-ttd-checkpoint-timestamp cp)))
             (seq-take checkpoints 10)
             "\n"))))

;;; Slash Command Handlers

(defun sage-ttd--cmd-checkpoint (args)
  "Slash command handler for /checkpoint."
  (let ((name (if (string-empty-p args)
                  (format "cp-%s" (format-time-string "%H%M%S"))
                args)))
    (sage-ttd--create-checkpoint name)))

(defun sage-ttd--cmd-restore (args)
  "Slash command handler for /restore."
  (if (string-empty-p args)
      (message "Usage: /restore <checkpoint-name>")
    (let ((cp (cl-find-if (lambda (c)
                            (equal args (sage-ttd-checkpoint-name c)))
                          (sage-ttd--list-checkpoints))))
      (if cp
          (sage-ttd--restore-checkpoint (sage-ttd-checkpoint-id cp))
        (message "Checkpoint not found: %s" args)))))

(defun sage-ttd--cmd-timeline (_args)
  "Slash command handler for /timeline."
  (sage-ttd-timeline))

(defun sage-ttd--cmd-branch (args)
  "Slash command handler for /branch."
  (if (string-empty-p args)
      (message "Current branch: %s\nBranches: %s"
               sage-ttd--current-branch
               (mapconcat #'car sage-ttd--branches ", "))
    (if (assoc args sage-ttd--branches)
        (sage-ttd--switch-branch args)
      (sage-ttd--create-branch args))))

;;; Hook Functions

(defun sage-ttd--auto-checkpoint-hook ()
  "Hook function for auto-checkpointing before requests."
  (when sage-ttd-auto-checkpoint
    (sage-ttd--create-checkpoint
     (format "auto-%s" (format-time-string "%H%M%S")))))

(defun sage-ttd--record-tool-call (tool-name args phase &optional result)
  "Record tool call for replay capability.
TOOL-NAME, ARGS, PHASE, and RESULT are passed from the hook."
  (when (eq phase 'after)
    (push (list :tool tool-name
                :args args
                :result result
                :timestamp (sage-ttd--timestamp))
          sage-ttd--tool-history)))

(defun sage-ttd--message-added-hook (_message)
  "Track message count for interval-based checkpointing."
  (cl-incf sage-ttd--message-count)
  (when (and sage-ttd-auto-checkpoint-interval
             (>= sage-ttd--message-count sage-ttd-auto-checkpoint-interval))
    (sage-ttd--create-checkpoint
     (format "auto-interval-%d" sage-ttd--message-count))))

;;; Filter Function (Example)

(defun sage-ttd--inject-context (message)
  "Example filter: inject TTD context into messages.
MESSAGE is the outgoing message alist."
  ;; Could inject checkpoint info, branch name, etc.
  ;; For now, just pass through
  message)

;;; Interactive Commands

;;;###autoload
(defun sage-ttd-checkpoint (name)
  "Create a checkpoint with NAME interactively."
  (interactive "sCheckpoint name: ")
  (sage-ttd--create-checkpoint name))

;;;###autoload
(defun sage-ttd-restore (name)
  "Restore to checkpoint NAME with completion."
  (interactive
   (list (completing-read "Restore checkpoint: "
                          (mapcar #'sage-ttd-checkpoint-name
                                  (sage-ttd--list-checkpoints))
                          nil t)))
  (let ((cp (cl-find-if (lambda (c)
                          (equal name (sage-ttd-checkpoint-name c)))
                        (sage-ttd--list-checkpoints))))
    (when cp
      (sage-ttd--restore-checkpoint (sage-ttd-checkpoint-id cp)))))

;;;###autoload
(defun sage-ttd-timeline ()
  "Display the conversation timeline in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*sage-ttd-timeline*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Sage TTD Timeline\n")
      (insert (make-string 50 ?=) "\n\n")
      (insert (format "Current branch: %s\n" sage-ttd--current-branch))
      (insert (format "Current checkpoint: %s\n\n"
                      (or sage-ttd--current-checkpoint-id "none")))

      ;; Branches
      (insert "Branches:\n")
      (dolist (b sage-ttd--branches)
        (insert (format "  %s %s\n"
                        (if (equal (car b) sage-ttd--current-branch) "*" " ")
                        (car b))))
      (insert "\n")

      ;; Checkpoints (simple list for now)
      (insert "Checkpoints:\n")
      (let ((checkpoints (sage-ttd--list-checkpoints)))
        (if (null checkpoints)
            (insert "  (none)\n")
          (dolist (cp checkpoints)
            (insert (format "  %s [%s] %s (%s)\n"
                            (if (equal (sage-ttd-checkpoint-id cp)
                                       sage-ttd--current-checkpoint-id)
                                ">" " ")
                            (substring (sage-ttd-checkpoint-id cp) 0 8)
                            (sage-ttd-checkpoint-name cp)
                            (sage-ttd-checkpoint-branch cp))))))

      ;; Tool history
      (insert "\nRecent tool calls:\n")
      (if (null sage-ttd--tool-history)
          (insert "  (none)\n")
        (dolist (call (seq-take sage-ttd--tool-history 5))
          (insert (format "  - %s\n" (plist-get call :tool)))))

      (goto-char (point-min)))
    (special-mode)
    (display-buffer (current-buffer))))

;;;###autoload
(defun sage-ttd-branch (name)
  "Create or switch to branch NAME."
  (interactive
   (list (completing-read "Branch: "
                          (mapcar #'car sage-ttd--branches)
                          nil nil)))
  (if (assoc name sage-ttd--branches)
      (sage-ttd--switch-branch name)
    (when (y-or-n-p (format "Create new branch '%s'? " name))
      (sage-ttd--create-branch name))))

;;; Extension Lifecycle

(defun sage-ttd-init ()
  "Initialize sage-ttd extension."
  (sage-ttd--ensure-directory)
  (sage-ttd--init-main-branch))

(defun sage-ttd-enable ()
  "Enable sage-ttd extension."
  ;; Register tools
  (when (fboundp 'sage-register-tool)
    (sage-register-tool
     "ttd_checkpoint"
     "Create a conversation checkpoint for time-travel debugging"
     '((type . "object")
       (properties . ((name . ((type . "string")
                               (description . "Checkpoint name (optional)")))))
       (required . []))
     #'sage-ttd--tool-checkpoint)

    (sage-register-tool
     "ttd_restore"
     "Restore conversation to a previous checkpoint"
     '((type . "object")
       (properties . ((name . ((type . "string")
                               (description . "Checkpoint name")))
                      (id . ((type . "string")
                             (description . "Checkpoint ID (alternative to name)")))))
       (required . []))
     #'sage-ttd--tool-restore)

    (sage-register-tool
     "ttd_list_checkpoints"
     "List available conversation checkpoints"
     '((type . "object")
       (properties . ())
       (required . []))
     #'sage-ttd--tool-list-checkpoints)

    (sage-register-tool
     "ttd_branch"
     "Create or switch conversation branch"
     '((type . "object")
       (properties . ((name . ((type . "string")
                               (description . "Branch name")))
                      (desc . ((type . "string")
                               (description . "Branch description (for new branches)")))))
       (required . ["name"]))
     #'sage-ttd--tool-branch)

    (sage-register-tool
     "ttd_timeline"
     "Show conversation timeline with branches and checkpoints"
     '((type . "object")
       (properties . ())
       (required . []))
     #'sage-ttd--tool-timeline))

  ;; Register slash commands
  (sage-register-slash-command "checkpoint" #'sage-ttd--cmd-checkpoint)
  (sage-register-slash-command "restore" #'sage-ttd--cmd-restore)
  (sage-register-slash-command "timeline" #'sage-ttd--cmd-timeline)
  (sage-register-slash-command "branch" #'sage-ttd--cmd-branch)

  ;; Add hooks
  (add-hook 'sage-before-request-hook #'sage-ttd--auto-checkpoint-hook)
  (add-hook 'sage-tool-call-hook #'sage-ttd--record-tool-call)
  (add-hook 'sage-message-added-hook #'sage-ttd--message-added-hook)

  ;; Add filter (example)
  (add-to-list 'sage-message-filter-functions #'sage-ttd--inject-context)

  ;; Add keybindings
  (with-eval-after-load 'sage
    (when (boundp 'sage-mode-map)
      (define-key sage-mode-map (kbd "C-c t c") #'sage-ttd-checkpoint)
      (define-key sage-mode-map (kbd "C-c t r") #'sage-ttd-restore)
      (define-key sage-mode-map (kbd "C-c t t") #'sage-ttd-timeline)
      (define-key sage-mode-map (kbd "C-c t b") #'sage-ttd-branch))))

(defun sage-ttd-disable ()
  "Disable sage-ttd extension."
  ;; Remove tools (would need sage-unregister-tool)

  ;; Remove slash commands
  (sage-unregister-slash-command "checkpoint")
  (sage-unregister-slash-command "restore")
  (sage-unregister-slash-command "timeline")
  (sage-unregister-slash-command "branch")

  ;; Remove hooks
  (remove-hook 'sage-before-request-hook #'sage-ttd--auto-checkpoint-hook)
  (remove-hook 'sage-tool-call-hook #'sage-ttd--record-tool-call)
  (remove-hook 'sage-message-added-hook #'sage-ttd--message-added-hook)

  ;; Remove filter
  (setq sage-message-filter-functions
        (delq #'sage-ttd--inject-context sage-message-filter-functions)))

;;; Extension Registration

(sage-register-extension
 'ttd
 :version "0.1.0"
 :description "Time-travel debugging: checkpoints, branches, replay"
 :requires '(sage-ext)
 :init #'sage-ttd-init
 :enable #'sage-ttd-enable
 :disable #'sage-ttd-disable)

(provide 'sage-ttd)

;;; sage-ttd.el ends here
