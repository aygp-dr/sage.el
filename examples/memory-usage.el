;;; memory-usage.el --- Examples of using sage-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates how to use the sage-memory system.
;; Load this file in an interactive Emacs session to try the examples.

;;; Code:

(require 'sage-memory)

;;; Example 1: Basic fact management

(defun memory-example-basic ()
  "Demonstrate basic fact operations."
  (interactive)

  ;; Add some facts about yourself
  (sage-memory-add "name" "Alice" 'general)
  (sage-memory-add "location" "San Francisco" 'general)

  ;; Add preferences
  (sage-memory-add "editor" "Emacs" 'preference)
  (sage-memory-add "theme" "dark" 'preference)
  (sage-memory-add "font" "Iosevka" 'preference)

  ;; Retrieve a fact
  (let ((editor-fact (sage-memory-get "editor")))
    (message "Editor: %s" (plist-get editor-fact :value)))

  ;; List all preferences
  (let ((prefs (sage-memory-list 'preference)))
    (message "Found %d preferences" (length prefs))))

;;; Example 2: Project-specific facts

(defun memory-example-project ()
  "Demonstrate project-specific facts."
  (interactive)

  ;; Add project information
  (sage-memory-add "current-project" "sage" 'project)
  (sage-memory-add "project-language" "Emacs Lisp" 'project)
  (sage-memory-add "project-goal" "AI REPL with tool calling" 'project)
  (sage-memory-add "repo-url" "https://github.com/aygp-dr/sage-010" 'project)

  ;; When switching projects, update relevant facts
  (sage-memory-add "current-project" "new-project" 'project)

  ;; Query project facts
  (let ((project-facts (sage-memory-list 'project)))
    (dolist (fact project-facts)
      (message "%s: %s"
               (plist-get fact :key)
               (plist-get fact :value)))))

;;; Example 3: Technical environment

(defun memory-example-technical ()
  "Demonstrate technical environment facts."
  (interactive)

  ;; System information
  (sage-memory-add "os" "FreeBSD" 'technical)
  (sage-memory-add "shell" "zsh" 'technical)
  (sage-memory-add "make-command" "gmake" 'technical)

  ;; Programming languages
  (sage-memory-add "lang-primary" "Emacs Lisp" 'technical)
  (sage-memory-add "lang-secondary" "Rust" 'technical)

  ;; Tools and utilities
  (sage-memory-add "version-control" "git" 'technical)
  (sage-memory-add "package-manager" "pkg" 'technical))

;;; Example 4: Context generation

(defun memory-example-context ()
  "Demonstrate context generation."
  (interactive)

  ;; Add various facts
  (sage-memory-add "name" "Bob" 'general)
  (sage-memory-add "role" "Software Engineer" 'general)
  (sage-memory-add "editor" "Emacs" 'preference)
  (sage-memory-add "current-project" "sage" 'project)
  (sage-memory-add "os" "FreeBSD" 'technical)

  ;; Generate context for AI
  (let ((context (sage-memory-to-context)))
    ;; Display in a buffer
    (with-current-buffer (get-buffer-create "*Memory Context*")
      (erase-buffer)
      (insert context)
      (goto-char (point-min))
      (markdown-mode)
      (pop-to-buffer (current-buffer)))))

;;; Example 5: Batch operations

(defun memory-example-batch ()
  "Demonstrate batch operations on facts."
  (interactive)

  ;; Add multiple related facts
  (let ((tech-stack '(("backend-lang" "Rust" technical)
                      ("frontend-lang" "TypeScript" technical)
                      ("database" "PostgreSQL" technical)
                      ("cache" "Redis" technical))))
    (dolist (fact tech-stack)
      (apply #'sage-memory-add fact)))

  ;; Export technical facts to a variable
  (let ((tech-facts (sage-memory-list 'technical)))
    (message "Technology stack has %d components" (length tech-facts))

    ;; Process each fact
    (dolist (fact tech-facts)
      (message "- %s: %s"
               (plist-get fact :key)
               (plist-get fact :value)))))

;;; Example 6: Updating facts

(defun memory-example-update ()
  "Demonstrate updating existing facts."
  (interactive)

  ;; Initial state
  (sage-memory-add "skill-level-elisp" "intermediate" 'technical)
  (message "Initial: %s"
           (plist-get (sage-memory-get "skill-level-elisp") :value))

  ;; Update after learning
  (sage-memory-add "skill-level-elisp" "advanced" 'technical)
  (message "Updated: %s"
           (plist-get (sage-memory-get "skill-level-elisp") :value))

  ;; Note: Only one entry exists, the update replaces the old value
  (message "Total facts: %d" (length sage-memory--facts)))

;;; Example 7: Conditional fact usage

(defun memory-example-conditional ()
  "Demonstrate conditional fact usage based on context."
  (interactive)

  ;; Setup different project contexts
  (sage-memory-add "work-project" "enterprise-app" 'project)
  (sage-memory-add "personal-project" "sage" 'project)
  (sage-memory-add "current-context" "work" 'general)

  ;; Use facts conditionally
  (let* ((context-fact (sage-memory-get "current-context"))
         (context (plist-get context-fact :value))
         (project-key (if (string= context "work")
                          "work-project"
                        "personal-project"))
         (project-fact (sage-memory-get project-key)))
    (message "Current context: %s, Project: %s"
             context
             (plist-get project-fact :value))))

;;; Example 8: Fact cleanup

(defun memory-example-cleanup ()
  "Demonstrate removing outdated facts."
  (interactive)

  ;; Add temporary facts
  (sage-memory-add "temp-note" "Remember to do X" 'general)
  (sage-memory-add "session-id" "abc123" 'general)

  ;; Later, clean up
  (sage-memory-remove "temp-note")
  (sage-memory-remove "session-id")

  (message "Temporary facts removed"))

;;; Example 9: Integration with sage

(defun memory-example-integration ()
  "Demonstrate integration with sage."
  (interactive)

  ;; Setup your profile before starting REPL
  (sage-memory-add "name" "Charlie" 'general)
  (sage-memory-add "expertise" "Emacs Lisp, Rust" 'technical)
  (sage-memory-add "workflow" "TDD, EDD" 'preference)
  (sage-memory-add "communication-style" "concise" 'preference)

  ;; Enable memory in conversations
  (setq sage-use-memory t)

  ;; Now when you start sage, the AI will know these facts
  ;; (sage)

  (message "Memory configured for sage. Start REPL to use."))

;;; Example 10: Category-based workflows

(defun memory-example-categories ()
  "Demonstrate working with different categories."
  (interactive)

  ;; Personal facts (general)
  (sage-memory-add "timezone" "PST" 'general)
  (sage-memory-add "working-hours" "9-5" 'general)

  ;; Preferences
  (sage-memory-add "code-style" "functional" 'preference)
  (sage-memory-add "commit-style" "conventional" 'preference)

  ;; Current project
  (sage-memory-add "project-phase" "Phase 2" 'project)
  (sage-memory-add "project-deadline" "2024-12-31" 'project)

  ;; Technical details
  (sage-memory-add "emacs-version" "31.0.50" 'technical)
  (sage-memory-add "build-system" "gmake" 'technical)

  ;; Show category summaries
  (dolist (category '(general preference project technical))
    (let ((facts (sage-memory-list category)))
      (message "%s: %d facts" category (length facts)))))

;;; Running all examples

(defun memory-run-all-examples ()
  "Run all memory examples in sequence."
  (interactive)
  (message "Running memory system examples...")
  (memory-example-basic)
  (sit-for 0.5)
  (memory-example-project)
  (sit-for 0.5)
  (memory-example-technical)
  (sit-for 0.5)
  (memory-example-update)
  (sit-for 0.5)
  (memory-example-batch)
  (sit-for 0.5)
  (message "Examples complete! Use M-x sage-memory-list to see all facts."))

(provide 'memory-usage)
;;; memory-usage.el ends here
