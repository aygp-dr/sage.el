;;; sage-org.el --- Sage integration with Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.0"))
;; Keywords: ai, org, tools
;; URL: https://github.com/aygp-dr/sage-010

;; This file is not part of GNU Emacs.

;;; Commentary:

;; AI-enhanced Org Mode workflows.
;;
;; Features:
;; - Summarize org documents or subtrees
;; - Expand outlines into full content
;; - Extract action items from meeting notes
;; - Generate org structure from description
;;
;; Usage:
;;   (require 'sage-org)
;;   M-x sage-org-summarize-buffer
;;   M-x sage-org-expand-outline
;;   M-x sage-org-extract-todos

;;; Code:

(require 'org)
(require 'sage-tools)

;;; Customization

(defgroup sage-org nil
  "Sage integration with Org Mode."
  :group 'sage
  :prefix "sage-org-")

(defcustom sage-org-summary-length 'medium
  "Length of generated summaries.
Options: `short' (1-2 sentences), `medium' (paragraph), `long' (detailed)."
  :type '(choice (const :tag "Short" short)
                 (const :tag "Medium" medium)
                 (const :tag "Long" long))
  :group 'sage-org)

;;; Helper Functions

(defun sage-org--get-subtree-content ()
  "Get content of current org subtree."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point)))
      (org-end-of-subtree t t)
      (buffer-substring-no-properties start (point)))))

(defun sage-org--get-buffer-content ()
  "Get content of current org buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun sage-org--length-instruction ()
  "Return instruction based on summary length setting."
  (pcase sage-org-summary-length
    ('short "Keep it to 1-2 sentences.")
    ('medium "Write a concise paragraph.")
    ('long "Provide a detailed summary with key points.")))

;;; Interactive Commands

;;;###autoload
(defun sage-org-summarize-subtree ()
  "Summarize the current org subtree using AI."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let* ((content (sage-org--get-subtree-content))
         (prompt (format "Summarize this org-mode content. %s

```org
%s
```"
                         (sage-org--length-instruction)
                         (if (> (length content) 20000)
                             (substring content 0 20000)
                           content)))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (message "Summary: %s" result)
    result))

;;;###autoload
(defun sage-org-summarize-buffer ()
  "Summarize the entire org buffer using AI."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let* ((content (sage-org--get-buffer-content))
         (prompt (format "Summarize this org-mode document. %s

```org
%s
```"
                         (sage-org--length-instruction)
                         (if (> (length content) 30000)
                             (substring content 0 30000)
                           content)))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (with-current-buffer (get-buffer-create "*sage-summary*")
      (erase-buffer)
      (insert "Document Summary\n")
      (insert (make-string 60 ?=) "\n\n")
      (insert result)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun sage-org-expand-outline ()
  "Expand the current outline into full content."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let* ((content (sage-org--get-subtree-content))
         (prompt (format "Expand this org-mode outline into full content.
Keep the org-mode structure (headings, lists, etc.).
Add substantive content under each heading.

Outline:
```org
%s
```

Return the expanded org-mode content:"
                         content))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (with-current-buffer (get-buffer-create "*sage-expanded*")
      (erase-buffer)
      (insert result)
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun sage-org-extract-todos ()
  "Extract action items from the current buffer or subtree."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let* ((content (if (org-at-heading-p)
                      (sage-org--get-subtree-content)
                    (sage-org--get-buffer-content)))
         (prompt (format "Extract action items and TODOs from this content.
Return them as org-mode TODO items with appropriate priorities.

Content:
```org
%s
```

Return only the TODO items in org format, like:
* TODO [#A] High priority task
* TODO [#B] Medium priority task"
                         (if (> (length content) 20000)
                             (substring content 0 20000)
                           content)))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (with-current-buffer (get-buffer-create "*sage-todos*")
      (erase-buffer)
      (insert "Extracted TODOs\n")
      (insert (make-string 60 ?=) "\n\n")
      (insert result)
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun sage-org-generate-structure (topic)
  "Generate an org-mode structure for TOPIC."
  (interactive "sTopic: ")
  (let* ((prompt (format "Generate an org-mode document structure for: %s

Include:
- Main heading
- Logical subheadings
- Brief placeholder text or TODOs
- Relevant tags

Return valid org-mode syntax:"
                         topic))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (with-current-buffer (get-buffer-create "*sage-structure*")
      (erase-buffer)
      (insert result)
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun sage-org-meeting-notes ()
  "Generate meeting notes template and extract action items."
  (interactive)
  (let* ((title (read-string "Meeting title: "))
         (attendees (read-string "Attendees (comma-separated): "))
         (notes (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Paste raw notes (or leave empty): ")))
         (prompt (format "Create organized meeting notes.

Meeting: %s
Attendees: %s
Raw notes: %s

Generate org-mode meeting notes with:
* Meeting Info (date, attendees)
* Agenda/Topics Discussed
* Key Decisions
* Action Items (as TODO items with assignees if mentioned)
* Next Steps"
                         title attendees notes))
         (result (sage--tool-ask-model `((prompt . ,prompt)))))
    (with-current-buffer (get-buffer-create (format "*Meeting: %s*" title))
      (erase-buffer)
      (insert result)
      (org-mode)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

;;; Tool Registration

(when (fboundp 'sage-tools--register)
  (sage-tools--register
   "org_summarize"
   "Summarize an org-mode document or section"
   '((type . "object")
     (properties . ((content . ((type . "string")
                                (description . "Org content to summarize")))))
     (required . ["content"]))
   (lambda (args)
     (let ((content (alist-get 'content args)))
       (sage--tool-ask-model
        `((prompt . ,(format "Summarize this org content concisely:\n\n%s" content)))))))

  (sage-tools--register
   "org_expand_outline"
   "Expand an org outline into full content"
   '((type . "object")
     (properties . ((outline . ((type . "string")
                                (description . "Org outline to expand")))))
     (required . ["outline"]))
   (lambda (args)
     (let ((outline (alist-get 'outline args)))
       (sage--tool-ask-model
        `((prompt . ,(format "Expand this outline into full org content:\n\n%s" outline)))))))

  (sage-tools--register
   "org_extract_todos"
   "Extract action items from text as org TODOs"
   '((type . "object")
     (properties . ((text . ((type . "string")
                             (description . "Text to extract TODOs from")))))
     (required . ["text"]))
   (lambda (args)
     (let ((text (alist-get 'text args)))
       (sage--tool-ask-model
        `((prompt . ,(format "Extract action items as org TODO items:\n\n%s" text))))))))

(provide 'sage-org)
;;; sage-org.el ends here
