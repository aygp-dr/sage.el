;;; sage-memory.el --- Persistent memory/facts system for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, memory
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; sage-memory provides a persistent memory/facts system for the
;; sage package.  It allows storing and retrieving facts across
;; sessions, categorizing them, and including them in conversation context.
;;
;; Features:
;; - Persistent JSON storage of facts
;; - Categorization (general, preference, project, technical)
;; - Automatic timestamping
;; - Context generation for LLM prompts
;; - Query and filtering by category
;;
;; Usage:
;;   (sage-memory-add "user-name" "Jason" 'preference)
;;   (sage-memory-get "user-name")
;;   (sage-memory-list 'project)
;;   (sage-memory-to-context)
;;   (sage-memory-remove "user-name")
;;   (sage-memory-clear)

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup sage-memory nil
  "Persistent memory/facts system for sage."
  :group 'sage
  :prefix "sage-memory-")

(defcustom sage-memory-file
  (expand-file-name "sage/memory/facts.json" user-emacs-directory)
  "File to store persistent facts."
  :type 'file
  :group 'sage-memory)

(defcustom sage-memory-auto-load t
  "When non-nil, automatically load facts on first access."
  :type 'boolean
  :group 'sage-memory)

(defcustom sage-memory-auto-save t
  "When non-nil, automatically save facts after modifications."
  :type 'boolean
  :group 'sage-memory)

;;; Variables

(defvar sage-memory--facts nil
  "In-memory storage of facts.
Alist of (KEY . FACT) where FACT is a plist with:
  :key       - unique identifier
  :value     - fact value
  :category  - one of general, preference, project, technical
  :timestamp - ISO 8601 timestamp")

(defvar sage-memory--loaded nil
  "Whether facts have been loaded from disk.")

(defvar sage-memory-categories
  '(general preference project technical)
  "Valid fact categories.")

;;; Core Functions

(defun sage-memory--ensure-loaded ()
  "Ensure facts are loaded from disk."
  (when (and sage-memory-auto-load
             (not sage-memory--loaded))
    (sage-memory-load)))

(defun sage-memory--ensure-directory ()
  "Ensure memory directory exists."
  (let ((dir (file-name-directory sage-memory-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun sage-memory--validate-category (category)
  "Validate CATEGORY, return symbol or signal error."
  (let ((cat-symbol (if (stringp category)
                        (intern category)
                      category)))
    (unless (memq cat-symbol sage-memory-categories)
      (error "Invalid category: %s. Must be one of: %s"
             category sage-memory-categories))
    cat-symbol))

(defun sage-memory--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun sage-memory--create-fact (key value category)
  "Create a fact plist.
KEY is the unique identifier.
VALUE is the fact value.
CATEGORY is the fact category."
  (list :key key
        :value value
        :category (sage-memory--validate-category category)
        :timestamp (sage-memory--timestamp)))

;;; Public API

;;;###autoload
(defun sage-memory-add (key value &optional category)
  "Add or update a fact.
KEY is a unique identifier (string).
VALUE is the fact value (string).
CATEGORY is one of general, preference, project, technical.
Defaults to general."
  (interactive
   (let* ((key (read-string "Fact key: "))
          (value (read-string "Fact value: "))
          (category (intern (completing-read
                            "Category: "
                            sage-memory-categories
                            nil t nil nil "general"))))
     (list key value category)))
  (sage-memory--ensure-loaded)
  (let* ((category (or category 'general))
         (fact (sage-memory--create-fact key value category)))
    ;; Remove existing fact with same key if present
    (setq sage-memory--facts
          (assoc-delete-all key sage-memory--facts))
    ;; Add new fact
    (push (cons key fact) sage-memory--facts)
    ;; Auto-save if enabled
    (when sage-memory-auto-save
      (sage-memory-save))
    fact))

;;;###autoload
(defun sage-memory-get (key)
  "Get fact by KEY.
Returns the fact plist or nil if not found."
  (interactive "sFact key: ")
  (sage-memory--ensure-loaded)
  (let ((fact (alist-get key sage-memory--facts nil nil #'string=)))
    (when (called-interactively-p 'any)
      (if fact
          (message "%s: %s [%s]"
                   key
                   (plist-get fact :value)
                   (plist-get fact :category))
        (message "Fact not found: %s" key)))
    fact))

;;;###autoload
(defun sage-memory-remove (key)
  "Remove fact by KEY.
Returns t if fact was removed, nil if not found."
  (interactive
   (list (completing-read "Remove fact: "
                         (mapcar #'car sage-memory--facts)
                         nil t)))
  (sage-memory--ensure-loaded)
  (let ((found (assoc key sage-memory--facts)))
    (when found
      (setq sage-memory--facts
            (assoc-delete-all key sage-memory--facts))
      (when sage-memory-auto-save
        (sage-memory-save))
      (when (called-interactively-p 'any)
        (message "Removed fact: %s" key))
      t)))

;;;###autoload
(defun sage-memory-list (&optional category)
  "List all facts, optionally filtered by CATEGORY.
Returns list of fact plists."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Category (empty for all): "
                                   sage-memory-categories
                                   nil t)))))
  (sage-memory--ensure-loaded)
  (let ((facts (if category
                   (cl-remove-if-not
                    (lambda (fact)
                      (eq (plist-get (cdr fact) :category)
                          (sage-memory--validate-category category)))
                    sage-memory--facts)
                 sage-memory--facts)))
    (when (called-interactively-p 'any)
      (if facts
          (let ((buf (get-buffer-create "*Sage Memory*")))
            (with-current-buffer buf
              (erase-buffer)
              (insert (format "Sage Memory Facts%s\n\n"
                            (if category
                                (format " [%s]" category)
                              "")))
              (dolist (fact-entry facts)
                (let* ((fact (cdr fact-entry))
                       (key (plist-get fact :key))
                       (value (plist-get fact :value))
                       (cat (plist-get fact :category))
                       (time (plist-get fact :timestamp)))
                  (insert (format "%-20s: %s\n" key value))
                  (insert (format "  Category: %s, Updated: %s\n\n" cat time))))
              (goto-char (point-min))
              (special-mode))
            (pop-to-buffer buf))
        (message "No facts found%s"
                (if category (format " in category: %s" category) ""))))
    (mapcar #'cdr facts)))

;;;###autoload
(defun sage-memory-clear ()
  "Clear all facts from memory.
Prompts for confirmation before clearing."
  (interactive)
  (when (yes-or-no-p "Clear all facts? This cannot be undone. ")
    (setq sage-memory--facts nil)
    (when sage-memory-auto-save
      (sage-memory-save))
    (message "All facts cleared")))

;;;###autoload
(defun sage-memory-to-context ()
  "Generate system prompt context from facts.
Returns a formatted string suitable for including in LLM context."
  (sage-memory--ensure-loaded)
  (if (null sage-memory--facts)
      ""
    (let ((categories (make-hash-table)))
      ;; Group facts by category
      (dolist (fact-entry sage-memory--facts)
        (let* ((fact (cdr fact-entry))
               (category (plist-get fact :category)))
          (push fact (gethash category categories))))

      ;; Build context string
      (with-temp-buffer
        (insert "# Stored Facts\n\n")
        (insert "The following facts have been learned about the user and their environment:\n\n")

        (dolist (category sage-memory-categories)
          (when-let* ((facts (gethash category categories)))
            (insert (format "## %s\n\n"
                          (capitalize (symbol-name category))))
            (dolist (fact (nreverse facts))
              (insert (format "- %s: %s\n"
                            (plist-get fact :key)
                            (plist-get fact :value))))
            (insert "\n")))

        (buffer-string)))))

;;; Persistence

(defun sage-memory-save ()
  "Save facts to disk."
  (interactive)
  (sage-memory--ensure-directory)
  (let ((facts-data (mapcar (lambda (fact-entry)
                              (let ((fact (cdr fact-entry)))
                                `((key . ,(plist-get fact :key))
                                  (value . ,(plist-get fact :value))
                                  (category . ,(symbol-name (plist-get fact :category)))
                                  (timestamp . ,(plist-get fact :timestamp)))))
                            sage-memory--facts)))
    (with-temp-file sage-memory-file
      (insert (json-encode facts-data)))
    (when (called-interactively-p 'any)
      (message "Facts saved to %s" sage-memory-file))))

(defun sage-memory-load ()
  "Load facts from disk."
  (interactive)
  (if (file-exists-p sage-memory-file)
      (condition-case err
          (progn
            (with-temp-buffer
              (insert-file-contents sage-memory-file)
              (let ((facts-data (json-read)))
                (setq sage-memory--facts
                      (mapcar (lambda (fact-obj)
                                (let ((key (alist-get 'key fact-obj))
                                      (value (alist-get 'value fact-obj))
                                      (category (intern (alist-get 'category fact-obj)))
                                      (timestamp (alist-get 'timestamp fact-obj)))
                                  (cons key
                                        (list :key key
                                              :value value
                                              :category category
                                              :timestamp timestamp))))
                              facts-data))))
            (setq sage-memory--loaded t)
            (when (called-interactively-p 'any)
              (message "Loaded %d facts from %s"
                      (length sage-memory--facts)
                      sage-memory-file)))
        (error
         (message "Error loading facts: %s" (error-message-string err))
         (setq sage-memory--facts nil
               sage-memory--loaded nil)))
    (setq sage-memory--facts nil
          sage-memory--loaded t)
    (when (called-interactively-p 'any)
      (message "No facts file found, starting fresh"))))

;;; Integration with sage

(defun sage-memory--inject-context ()
  "Inject memory context into conversation.
This function can be called before sending messages to LLM."
  (let ((context (sage-memory-to-context)))
    (when (and context (not (string-empty-p context)))
      context)))

(provide 'sage-memory)
;;; sage-memory.el ends here
