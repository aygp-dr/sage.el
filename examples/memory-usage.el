;;; memory-usage.el --- Examples of using gemini-repl-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates how to use the gemini-repl-memory system.
;; Load this file in an interactive Emacs session to try the examples.

;;; Code:

(require 'gemini-repl-memory)

;;; Example 1: Basic fact management

(defun memory-example-basic ()
  "Demonstrate basic fact operations."
  (interactive)

  ;; Add some facts about yourself
  (gemini-repl-memory-add "name" "Alice" 'general)
  (gemini-repl-memory-add "location" "San Francisco" 'general)

  ;; Add preferences
  (gemini-repl-memory-add "editor" "Emacs" 'preference)
  (gemini-repl-memory-add "theme" "dark" 'preference)
  (gemini-repl-memory-add "font" "Iosevka" 'preference)

  ;; Retrieve a fact
  (let ((editor-fact (gemini-repl-memory-get "editor")))
    (message "Editor: %s" (plist-get editor-fact :value)))

  ;; List all preferences
  (let ((prefs (gemini-repl-memory-list 'preference)))
    (message "Found %d preferences" (length prefs))))

;;; Example 2: Project-specific facts

(defun memory-example-project ()
  "Demonstrate project-specific facts."
  (interactive)

  ;; Add project information
  (gemini-repl-memory-add "current-project" "gemini-repl" 'project)
  (gemini-repl-memory-add "project-language" "Emacs Lisp" 'project)
  (gemini-repl-memory-add "project-goal" "AI REPL with tool calling" 'project)
  (gemini-repl-memory-add "repo-url" "https://github.com/aygp-dr/gemini-repl-010" 'project)

  ;; When switching projects, update relevant facts
  (gemini-repl-memory-add "current-project" "new-project" 'project)

  ;; Query project facts
  (let ((project-facts (gemini-repl-memory-list 'project)))
    (dolist (fact project-facts)
      (message "%s: %s"
               (plist-get fact :key)
               (plist-get fact :value)))))

;;; Example 3: Technical environment

(defun memory-example-technical ()
  "Demonstrate technical environment facts."
  (interactive)

  ;; System information
  (gemini-repl-memory-add "os" "FreeBSD" 'technical)
  (gemini-repl-memory-add "shell" "zsh" 'technical)
  (gemini-repl-memory-add "make-command" "gmake" 'technical)

  ;; Programming languages
  (gemini-repl-memory-add "lang-primary" "Emacs Lisp" 'technical)
  (gemini-repl-memory-add "lang-secondary" "Rust" 'technical)

  ;; Tools and utilities
  (gemini-repl-memory-add "version-control" "git" 'technical)
  (gemini-repl-memory-add "package-manager" "pkg" 'technical))

;;; Example 4: Context generation

(defun memory-example-context ()
  "Demonstrate context generation."
  (interactive)

  ;; Add various facts
  (gemini-repl-memory-add "name" "Bob" 'general)
  (gemini-repl-memory-add "role" "Software Engineer" 'general)
  (gemini-repl-memory-add "editor" "Emacs" 'preference)
  (gemini-repl-memory-add "current-project" "gemini-repl" 'project)
  (gemini-repl-memory-add "os" "FreeBSD" 'technical)

  ;; Generate context for AI
  (let ((context (gemini-repl-memory-to-context)))
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
      (apply #'gemini-repl-memory-add fact)))

  ;; Export technical facts to a variable
  (let ((tech-facts (gemini-repl-memory-list 'technical)))
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
  (gemini-repl-memory-add "skill-level-elisp" "intermediate" 'technical)
  (message "Initial: %s"
           (plist-get (gemini-repl-memory-get "skill-level-elisp") :value))

  ;; Update after learning
  (gemini-repl-memory-add "skill-level-elisp" "advanced" 'technical)
  (message "Updated: %s"
           (plist-get (gemini-repl-memory-get "skill-level-elisp") :value))

  ;; Note: Only one entry exists, the update replaces the old value
  (message "Total facts: %d" (length gemini-repl-memory--facts)))

;;; Example 7: Conditional fact usage

(defun memory-example-conditional ()
  "Demonstrate conditional fact usage based on context."
  (interactive)

  ;; Setup different project contexts
  (gemini-repl-memory-add "work-project" "enterprise-app" 'project)
  (gemini-repl-memory-add "personal-project" "gemini-repl" 'project)
  (gemini-repl-memory-add "current-context" "work" 'general)

  ;; Use facts conditionally
  (let* ((context-fact (gemini-repl-memory-get "current-context"))
         (context (plist-get context-fact :value))
         (project-key (if (string= context "work")
                          "work-project"
                        "personal-project"))
         (project-fact (gemini-repl-memory-get project-key)))
    (message "Current context: %s, Project: %s"
             context
             (plist-get project-fact :value))))

;;; Example 8: Fact cleanup

(defun memory-example-cleanup ()
  "Demonstrate removing outdated facts."
  (interactive)

  ;; Add temporary facts
  (gemini-repl-memory-add "temp-note" "Remember to do X" 'general)
  (gemini-repl-memory-add "session-id" "abc123" 'general)

  ;; Later, clean up
  (gemini-repl-memory-remove "temp-note")
  (gemini-repl-memory-remove "session-id")

  (message "Temporary facts removed"))

;;; Example 9: Integration with gemini-repl

(defun memory-example-integration ()
  "Demonstrate integration with gemini-repl."
  (interactive)

  ;; Setup your profile before starting REPL
  (gemini-repl-memory-add "name" "Charlie" 'general)
  (gemini-repl-memory-add "expertise" "Emacs Lisp, Rust" 'technical)
  (gemini-repl-memory-add "workflow" "TDD, EDD" 'preference)
  (gemini-repl-memory-add "communication-style" "concise" 'preference)

  ;; Enable memory in conversations
  (setq gemini-repl-use-memory t)

  ;; Now when you start gemini-repl, the AI will know these facts
  ;; (gemini-repl)

  (message "Memory configured for gemini-repl. Start REPL to use."))

;;; Example 10: Category-based workflows

(defun memory-example-categories ()
  "Demonstrate working with different categories."
  (interactive)

  ;; Personal facts (general)
  (gemini-repl-memory-add "timezone" "PST" 'general)
  (gemini-repl-memory-add "working-hours" "9-5" 'general)

  ;; Preferences
  (gemini-repl-memory-add "code-style" "functional" 'preference)
  (gemini-repl-memory-add "commit-style" "conventional" 'preference)

  ;; Current project
  (gemini-repl-memory-add "project-phase" "Phase 2" 'project)
  (gemini-repl-memory-add "project-deadline" "2024-12-31" 'project)

  ;; Technical details
  (gemini-repl-memory-add "emacs-version" "31.0.50" 'technical)
  (gemini-repl-memory-add "build-system" "gmake" 'technical)

  ;; Show category summaries
  (dolist (category '(general preference project technical))
    (let ((facts (gemini-repl-memory-list category)))
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
  (message "Examples complete! Use M-x gemini-repl-memory-list to see all facts."))

(provide 'memory-usage)
;;; memory-usage.el ends here
