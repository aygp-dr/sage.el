;;; sage-magit.el --- Sage integration with Magit -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "3.0"))
;; Keywords: ai, git, tools
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; AI-enhanced Git workflows through Magit integration.
;;
;; Features:
;; - AI-generated commit messages from staged changes
;; - Explain merge conflicts
;; - Code review before commit
;; - PR description generation
;;
;; Usage:
;;   (require 'sage-magit)
;;   M-x sage-magit-generate-commit-message
;;   M-x sage-magit-explain-conflict
;;   M-x sage-magit-review-staged

;;; Code:

(require 'sage-tools)

;; Soft dependency on magit - functions will error if magit not available
(declare-function magit-git-string "magit-git")
(declare-function magit-git-lines "magit-git")
(declare-function magit-get-upstream-branch "magit-git")

(defun sage-magit--ensure-magit ()
  "Ensure magit is available, error if not."
  (unless (require 'magit nil t)
    (user-error "sage-magit requires magit. Install with M-x package-install RET magit RET")))

;;; Customization

(defgroup sage-magit nil
  "Sage integration with Magit."
  :group 'sage
  :prefix "sage-magit-")

(defcustom sage-magit-commit-style 'conventional
  "Style for generated commit messages.
Options:
  - `conventional': Conventional commits (feat:, fix:, etc.)
  - `simple': Simple descriptive messages
  - `detailed': Longer explanatory messages"
  :type '(choice (const :tag "Conventional Commits" conventional)
                 (const :tag "Simple" simple)
                 (const :tag "Detailed" detailed))
  :group 'sage-magit)

(defcustom sage-magit-include-scope t
  "Include scope in conventional commits (e.g., feat(scope): message)."
  :type 'boolean
  :group 'sage-magit)

;;; Helper Functions

(defun sage-magit--get-staged-diff ()
  "Get the staged diff as a string."
  (sage-magit--ensure-magit)
  (magit-git-string "diff" "--cached" "--no-color"))

(defun sage-magit--get-unstaged-diff ()
  "Get the unstaged diff as a string."
  (magit-git-string "diff" "--no-color"))

(defun sage-magit--get-conflict-files ()
  "Get list of files with merge conflicts."
  (magit-git-lines "diff" "--name-only" "--diff-filter=U"))

(defun sage-magit--get-conflict-diff (file)
  "Get conflict markers for FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun sage-magit--get-recent-commits (&optional count)
  "Get recent COUNT commit messages for style reference."
  (let ((n (or count 5)))
    (magit-git-lines "log" (format "-%d" n) "--format=%s")))

;;; Interactive Commands

;;;###autoload
(defun sage-magit-generate-commit-message ()
  "Generate a commit message from staged changes using AI."
  (interactive)
  (let ((diff (sage-magit--get-staged-diff)))
    (unless diff
      (user-error "No staged changes"))
    (when (> (length diff) 50000)
      (setq diff (substring diff 0 50000))
      (message "Diff truncated to 50000 characters"))
    (let* ((style-prompt (pcase sage-magit-commit-style
                           ('conventional "Use conventional commit format (feat:, fix:, docs:, refactor:, test:, chore:)")
                           ('simple "Write a simple, clear commit message")
                           ('detailed "Write a detailed commit message with body explaining the changes")))
           (recent (sage-magit--get-recent-commits 3))
           (prompt (format "Generate a git commit message for the following diff.
%s
%s

Recent commits for style reference:
%s

Diff:
```
%s
```

Return ONLY the commit message, no explanation."
                           style-prompt
                           (if sage-magit-include-scope
                               "Include scope if clear from the changes (e.g., feat(auth): ...)"
                             "Do not include scope")
                           (mapconcat #'identity recent "\n")
                           diff))
           (result (sage--tool-ask-model `((prompt . ,prompt)))))
      (kill-new result)
      (message "Commit message copied to kill ring:\n%s" result)
      result)))

;;;###autoload
(defun sage-magit-explain-conflict ()
  "Explain merge conflicts in the current repository."
  (interactive)
  (let ((conflict-files (sage-magit--get-conflict-files)))
    (unless conflict-files
      (user-error "No merge conflicts found"))
    (let* ((file (if (= (length conflict-files) 1)
                     (car conflict-files)
                   (completing-read "File with conflict: " conflict-files nil t)))
           (content (sage-magit--get-conflict-diff file))
           (prompt (format "Explain this merge conflict and suggest how to resolve it:

File: %s

```
%s
```

Explain:
1. What each side of the conflict represents
2. Why the conflict occurred
3. How to resolve it (keep ours, theirs, or merge)"
                           file
                           (if (> (length content) 10000)
                               (substring content 0 10000)
                             content)))
           (result (sage--tool-ask-model `((prompt . ,prompt)))))
      (with-current-buffer (get-buffer-create "*sage-conflict-explanation*")
        (erase-buffer)
        (insert "Conflict Explanation for: " file "\n")
        (insert (make-string 60 ?=) "\n\n")
        (insert result)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun sage-magit-review-staged ()
  "AI code review of staged changes before commit."
  (interactive)
  (let ((diff (sage-magit--get-staged-diff)))
    (unless diff
      (user-error "No staged changes"))
    (when (> (length diff) 50000)
      (setq diff (substring diff 0 50000)))
    (let* ((prompt (format "Review these staged changes for a git commit.
Look for:
1. Potential bugs or errors
2. Security issues
3. Code style problems
4. Missing error handling
5. Opportunities for improvement

Be concise. If everything looks good, say so.

Diff:
```
%s
```"
                           diff))
           (result (sage--tool-ask-model `((prompt . ,prompt)))))
      (with-current-buffer (get-buffer-create "*sage-review*")
        (erase-buffer)
        (insert "Code Review - Staged Changes\n")
        (insert (make-string 60 ?=) "\n\n")
        (insert result)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun sage-magit-generate-pr-description ()
  "Generate a PR description from commits on current branch."
  (interactive)
  (let* ((base (or (magit-get-upstream-branch) "main"))
         (commits (magit-git-lines "log" (format "%s..HEAD" base) "--format=%s%n%b---"))
         (diff-stat (magit-git-string "diff" "--stat" base))
         (prompt (format "Generate a GitHub Pull Request description.

Commits:
%s

Files changed:
%s

Write a PR description with:
## Summary
(1-3 bullet points)

## Changes
(List key changes)

## Test Plan
(How to verify)"
                         (mapconcat #'identity commits "\n")
                         diff-stat))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (kill-new result)
    (message "PR description copied to kill ring")
    (with-current-buffer (get-buffer-create "*sage-pr-description*")
      (erase-buffer)
      (insert result)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Tool Registration

(when (fboundp 'sage-tools--register)
  (sage-tools--register
   "generate_commit_message"
   "Generate a git commit message from staged changes"
   '((type . "object")
     (properties . ())
     (required . []))
   (lambda (_args)
     (sage-magit-generate-commit-message)))

  (sage-tools--register
   "explain_merge_conflict"
   "Explain merge conflicts and suggest resolution"
   '((type . "object")
     (properties . ((file . ((type . "string")
                             (description . "File with conflict (optional)")))))
     (required . []))
   (lambda (args)
     (let ((file (alist-get 'file args)))
       (if file
           (sage-magit--get-conflict-diff file)
         (sage-magit-explain-conflict)))))

  (sage-tools--register
   "review_staged_changes"
   "AI code review of staged changes before commit"
   '((type . "object")
     (properties . ())
     (required . []))
   (lambda (_args)
     (sage-magit-review-staged))))

(provide 'sage-magit)
;;; sage-magit.el ends here
