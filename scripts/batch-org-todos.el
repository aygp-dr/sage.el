;;; batch-org-todos.el --- Batch org-mode todo management via sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Demonstrates batch interaction with sage for org-mode todo management.
;; Shows how to drive AI-assisted operations like:
;; - Export internal todos to org format
;; - Improve task headers
;; - Add/adjust priorities
;; - Generate task summaries
;;
;; This is the Emacs equivalent of Claude Code's interactive todo management,
;; but designed for batch/automated workflows.
;;
;; Usage:
;;   make batch-todos
;;   emacs --batch -L . -l scripts/batch-org-todos.el
;;
;; Context: https://news.ycombinator.com/item?id=46564116
;; (Discussion of org-mode as a superior markup language)

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load sage modules
(require 'sage-tools)
(require 'sage-project)

;;; Internal Todo Structure
;; This mirrors what Claude Code uses internally for task tracking

(defvar sage-internal-todos nil
  "Internal todo list (like Claude Code's TodoWrite).
Each item is a plist with :content :status :activeForm :priority.")

(defun sage-todo-create (content status &optional active-form priority)
  "Create a todo item with CONTENT, STATUS, ACTIVE-FORM, and PRIORITY."
  `(:content ,content
    :status ,status
    :activeForm ,(or active-form content)
    :priority ,(or priority "B")
    :created ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))

(defun sage-todo-add (content &optional priority)
  "Add a new todo with CONTENT and optional PRIORITY."
  (push (sage-todo-create content "pending" nil priority) sage-internal-todos))

(defun sage-todo-set-status (index status)
  "Set STATUS of todo at INDEX."
  (when-let* ((todo (nth index sage-internal-todos)))
    (setf (nth index sage-internal-todos)
          (plist-put todo :status status))))

;;; Export to Org Format

(defun sage-todos-to-org (&optional todos)
  "Export TODOS (or `sage-internal-todos') to org-mode format.
Returns a string of org content."
  (let ((items (or todos sage-internal-todos))
        (lines '()))
    (push "#+TITLE: Sage Task List" lines)
    (push (format "#+DATE: %s" (format-time-string "%Y-%m-%d")) lines)
    (push "#+OPTIONS: toc:nil" lines)
    (push "" lines)

    ;; Group by status
    (let ((pending (seq-filter (lambda (t) (string= (plist-get t :status) "pending")) items))
          (in-progress (seq-filter (lambda (t) (string= (plist-get t :status) "in_progress")) items))
          (completed (seq-filter (lambda (t) (string= (plist-get t :status) "completed")) items)))

      ;; In Progress
      (when in-progress
        (push "* In Progress" lines)
        (dolist (todo in-progress)
          (push (sage-todo--to-org-item todo "IN-PROGRESS") lines)))

      ;; Pending
      (when pending
        (push "* Pending" lines)
        (dolist (todo pending)
          (push (sage-todo--to-org-item todo "TODO") lines)))

      ;; Completed
      (when completed
        (push "* Completed" lines)
        (dolist (todo completed)
          (push (sage-todo--to-org-item todo "DONE") lines))))

    (mapconcat #'identity (nreverse lines) "\n")))

(defun sage-todo--to-org-item (todo state)
  "Convert TODO to org heading with STATE."
  (let* ((content (plist-get todo :content))
         (priority (plist-get todo :priority))
         (created (plist-get todo :created))
         (priority-str (if priority (format " [#%s]" priority) "")))
    (format "** %s%s %s\n:PROPERTIES:\n:CREATED: %s\n:END:"
            state
            priority-str
            content
            (or created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))

(defun sage-todos-export-to-file (filename &optional todos)
  "Export TODOS to FILENAME in org format."
  (let ((org-content (sage-todos-to-org todos)))
    (with-temp-file filename
      (insert org-content))
    (message "Exported %d todos to %s" (length (or todos sage-internal-todos)) filename)
    filename))

;;; AI-Assisted Operations (Templates)
;; These generate prompts for sage to process

(defun sage-todo-improve-headers-prompt (todos)
  "Generate prompt to improve todo headers in TODOS."
  (format "Review these task descriptions and suggest improvements.
Make them more specific, actionable, and clear.

Current tasks:
%s

For each task, suggest:
1. An improved title (imperative, specific)
2. An active form for progress display
3. A priority (A=urgent, B=normal, C=low)

Return as org-mode TODO items."
          (mapconcat (lambda (t) (format "- %s" (plist-get t :content)))
                     todos "\n")))

(defun sage-todo-add-priorities-prompt (todos)
  "Generate prompt to suggest priorities for TODOS."
  (format "Analyze these tasks and suggest priorities.

Tasks:
%s

Assign priorities based on:
- [#A] Critical/urgent tasks
- [#B] Important but not urgent
- [#C] Nice to have, low priority

Return as org-mode format:
* TODO [#X] Task description"
          (mapconcat (lambda (t) (format "- %s" (plist-get t :content)))
                     todos "\n")))

(defun sage-todo-summarize-prompt (todos)
  "Generate prompt to summarize TODOS."
  (format "Summarize these tasks into a brief status report.

Tasks:
%s

Provide:
1. One-line summary
2. Key blockers or risks
3. Suggested next action"
          (mapconcat (lambda (t)
                       (format "- [%s] %s"
                               (plist-get t :status)
                               (plist-get t :content)))
                     todos "\n")))

;;; Batch Demonstration

(defun sage-batch-demo ()
  "Run batch demonstration of org todo export."
  (message "\n=== Sage Batch Org Todo Demo ===\n")

  ;; Create sample todos (like Claude Code's internal tracking)
  (setq sage-internal-todos nil)

  ;; Add sample tasks
  (sage-todo-add "Integrate sage.el with sage-project.el for persistence" "A")
  (sage-todo-add "Add structured tool call logging to JSONL" "A")
  (sage-todo-add "Create batch interaction examples" "B")
  (sage-todo-add "Write org export tests" "B")
  (sage-todo-add "Document storage presets" "C")
  (sage-todo-add "Add rate limit dashboard" "C")

  ;; Set some statuses
  (sage-todo-set-status 2 "in_progress")
  (sage-todo-set-status 3 "completed")

  ;; Display current state
  (message "Current internal todos:")
  (dolist (todo sage-internal-todos)
    (message "  [%s] %s (priority: %s)"
             (plist-get todo :status)
             (plist-get todo :content)
             (plist-get todo :priority)))

  ;; Export to org format
  (message "\n--- Exported Org Format ---\n")
  (let ((org-content (sage-todos-to-org)))
    (message "%s" org-content)

    ;; Also write to file
    (let ((output-file (expand-file-name "sage-todos.org" default-directory)))
      (sage-todos-export-to-file output-file)
      (message "\nOrg file written: %s" output-file)))

  ;; Show AI prompts that could be sent to sage
  (message "\n--- AI Prompt Templates ---\n")

  (message "1. Improve Headers Prompt:")
  (message "---")
  (message "%s" (sage-todo-improve-headers-prompt sage-internal-todos))

  (message "\n2. Add Priorities Prompt:")
  (message "---")
  (message "%s" (sage-todo-add-priorities-prompt sage-internal-todos))

  (message "\n3. Summarize Prompt:")
  (message "---")
  (message "%s" (sage-todo-summarize-prompt sage-internal-todos))

  (message "\n=== Demo Complete ===\n"))

;;; Test Functions

(defun sage-test-org-export ()
  "Test org export functionality."
  (setq sage-internal-todos nil)

  ;; Create test data
  (sage-todo-add "Test task one" "B")
  (sage-todo-add "Test task two" "A")
  (sage-todo-set-status 0 "completed")
  (sage-todo-set-status 1 "in_progress")

  (let ((org-content (sage-todos-to-org)))
    ;; Verify structure
    (cl-assert (string-match-p "\\* In Progress" org-content))
    (cl-assert (string-match-p "\\* Completed" org-content))
    ;; Check for IN-PROGRESS state and [#A] priority (may be on separate lines)
    (cl-assert (string-match-p "IN-PROGRESS" org-content))
    (cl-assert (string-match-p "\\[#A\\]" org-content))
    (cl-assert (string-match-p "DONE" org-content))
    (cl-assert (string-match-p "\\[#B\\]" org-content))
    (message "Org export test PASSED")))

(defun sage-test-todo-operations ()
  "Test todo CRUD operations."
  (setq sage-internal-todos nil)

  ;; Test add
  (sage-todo-add "Task A" "A")
  (cl-assert (= (length sage-internal-todos) 1))
  (cl-assert (string= (plist-get (car sage-internal-todos) :content) "Task A"))
  (cl-assert (string= (plist-get (car sage-internal-todos) :priority) "A"))

  ;; Test status change
  (sage-todo-set-status 0 "completed")
  (cl-assert (string= (plist-get (car sage-internal-todos) :status) "completed"))

  (message "Todo operations test PASSED"))

(defun sage-run-tests ()
  "Run all tests."
  (message "\n=== Running Tests ===\n")
  (sage-test-todo-operations)
  (sage-test-org-export)
  (message "\n=== All Tests Passed ===\n"))

;; Run if loaded in batch mode
(when noninteractive
  (sage-run-tests)
  (sage-batch-demo))

(provide 'batch-org-todos)
;;; batch-org-todos.el ends here
