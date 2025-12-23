;;; multi-agent-workflow.el --- Multi-agent workflow example -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; Demonstrates a complete multi-agent workflow using gemini-repl-queue
;; to coordinate between multiple AI agents performing different tasks.
;;
;; Scenario: Code review workflow
;; - Agent 1: Code analyzer (checks style, complexity)
;; - Agent 2: Security reviewer (looks for vulnerabilities)
;; - Agent 3: Documentation reviewer (checks comments, docs)
;; - Coordinator: Aggregates results and produces final report

;;; Code:

(require 'gemini-repl-queue)

;;; Configuration

(defvar multi-agent-workflow-results nil
  "Hash table storing results from each agent.")

(defvar multi-agent-workflow-pending nil
  "List of pending agent tasks.")

;;; Agent Definitions

(defun multi-agent-workflow--code-analyzer-handler (request)
  "Analyze code quality and style.
REQUEST is the request alist."
  (let* ((content (alist-get 'content request))
         (context (alist-get 'context request))
         (file (alist-get 'file context)))

    ;; Simulate code analysis
    (cons 'success
          (format "Code Analysis for %s:
- Style: Good (follows conventions)
- Complexity: Medium (cyclomatic complexity: 8)
- Maintainability: High
- Readability: Good
- Suggestions:
  * Consider breaking down large functions
  * Add more descriptive variable names in section 3"
                  file))))

(defun multi-agent-workflow--security-reviewer-handler (request)
  "Review code for security issues.
REQUEST is the request alist."
  (let* ((content (alist-get 'content request))
         (context (alist-get 'context request))
         (file (alist-get 'file context)))

    ;; Simulate security review
    (cons 'success
          (format "Security Review for %s:
- Input validation: PASS
- SQL injection: No issues found
- XSS vulnerabilities: None detected
- Path traversal: Protected
- Sensitive data: No hardcoded secrets
- Recommendations:
  * Add rate limiting to API endpoints
  * Consider using constant-time comparison for auth tokens"
                  file))))

(defun multi-agent-workflow--doc-reviewer-handler (request)
  "Review documentation quality.
REQUEST is the request alist."
  (let* ((content (alist-get 'content request))
         (context (alist-get 'context request))
         (file (alist-get 'file context)))

    ;; Simulate documentation review
    (cons 'success
          (format "Documentation Review for %s:
- API documentation: Complete
- Inline comments: Good coverage (75%%)
- README: Present and informative
- Examples: Available
- Improvements needed:
  * Add docstrings to 3 public functions
  * Include usage examples in README
  * Document error conditions"
                  file))))

(defun multi-agent-workflow--coordinator-handler (request)
  "Coordinate review results and produce final report.
REQUEST is the request alist."
  (let* ((context (alist-get 'context request))
         (file (alist-get 'file context))
         (review-id (alist-get 'review_id context)))

    ;; Collect results from all agents
    (let ((code-analysis "Pending...")
          (security-review "Pending...")
          (doc-review "Pending..."))

      ;; In a real implementation, you would:
      ;; 1. Wait for all agent responses
      ;; 2. Aggregate results
      ;; 3. Generate final report

      (cons 'success
            (format "=== FINAL REVIEW REPORT ===
File: %s
Review ID: %s

1. CODE ANALYSIS
%s

2. SECURITY REVIEW
%s

3. DOCUMENTATION REVIEW
%s

OVERALL VERDICT: APPROVED with minor suggestions
Reviewer: Coordinator Agent
Date: %s"
                    file review-id
                    code-analysis security-review doc-review
                    (current-time-string))))))

;;; Workflow Execution

;;;###autoload
(defun multi-agent-workflow-setup ()
  "Set up multi-agent workflow handlers."
  (interactive)

  ;; Register handlers for each agent
  (gemini-repl-queue-register-handler 'code_analysis
                                      #'multi-agent-workflow--code-analyzer-handler)

  (gemini-repl-queue-register-handler 'security_review
                                      #'multi-agent-workflow--security-reviewer-handler)

  (gemini-repl-queue-register-handler 'doc_review
                                      #'multi-agent-workflow--doc-reviewer-handler)

  (gemini-repl-queue-register-handler 'coordinate_review
                                      #'multi-agent-workflow--coordinator-handler)

  ;; Enable watch mode for automatic processing
  (gemini-repl-queue-watch-mode 1)

  (message "Multi-agent workflow initialized. Agents ready."))

;;;###autoload
(defun multi-agent-workflow-review-file (file)
  "Submit FILE for multi-agent review.
FILE is the path to the file to review."
  (interactive "fFile to review: ")

  (let ((review-id (gemini-repl-queue--generate-id))
        (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))

    ;; Submit to all review agents
    (gemini-repl-queue-submit
     'code_analysis
     content
     `((file . ,file)
       (review_id . ,review-id)))

    (gemini-repl-queue-submit
     'security_review
     content
     `((file . ,file)
       (review_id . ,review-id)))

    (gemini-repl-queue-submit
     'doc_review
     content
     `((file . ,file)
       (review_id . ,review-id)))

    ;; Queue coordinator task (will run after agents complete)
    (run-with-timer
     5.0 nil ; Wait 5 seconds for agents
     (lambda ()
       (gemini-repl-queue-submit
        'coordinate_review
        "Aggregate review results"
        `((file . ,file)
          (review_id . ,review-id)))))

    (message "Review %s submitted for %s. Check queue status for results." review-id file)))

;;;###autoload
(defun multi-agent-workflow-review-current-buffer ()
  "Submit current buffer for multi-agent review."
  (interactive)
  (if (buffer-file-name)
      (multi-agent-workflow-review-file (buffer-file-name))
    (error "Buffer has no associated file")))

;;; Parallel Task Distribution Example

;;;###autoload
(defun multi-agent-workflow-parallel-analysis (files)
  "Analyze multiple FILES in parallel using queue system.
FILES is a list of file paths."
  (interactive
   (list (completing-read-multiple "Files: "
                                   (directory-files default-directory nil "\\.[[:alpha:]]+$"))))

  (setq multi-agent-workflow-results (make-hash-table :test 'equal))

  ;; Submit all files for parallel processing
  (dolist (file files)
    (let ((content (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))

      ;; Each file gets analyzed by all agents in parallel
      (gemini-repl-queue-submit
       'code_analysis
       content
       `((file . ,file)
         (batch . t)))))

  (message "Submitted %d files for parallel analysis" (length files)))

;;; Pipeline Example

(defun multi-agent-workflow-pipeline-example ()
  "Demonstrate a multi-stage pipeline workflow."
  (interactive)

  ;; Stage 1: Extract functions
  (gemini-repl-queue-register-handler
   'extract_functions
   (lambda (request)
     (let ((content (alist-get 'content request)))
       ;; Extract function definitions (simplified)
       (gemini-repl-queue-submit
        'analyze_functions
        "function list here"
        (alist-get 'context request))
       (cons 'success "Functions extracted, queued for analysis"))))

  ;; Stage 2: Analyze functions
  (gemini-repl-queue-register-handler
   'analyze_functions
   (lambda (request)
     (let ((functions (alist-get 'content request)))
       ;; Analyze each function
       (gemini-repl-queue-submit
        'generate_tests
        functions
        (alist-get 'context request))
       (cons 'success "Functions analyzed, queued for test generation"))))

  ;; Stage 3: Generate tests
  (gemini-repl-queue-register-handler
   'generate_tests
   (lambda (request)
     (cons 'success "Tests generated successfully")))

  ;; Start pipeline
  (gemini-repl-queue-submit
   'extract_functions
   (buffer-substring-no-properties (point-min) (point-max))
   `((file . ,(buffer-file-name)))))

;;; Monitoring and Debugging

;;;###autoload
(defun multi-agent-workflow-show-status ()
  "Show status of multi-agent workflow."
  (interactive)

  ;; Show queue status
  (gemini-repl-queue-status)

  ;; Add workflow-specific stats
  (with-current-buffer "*gemini-repl-queue-status*"
    (goto-char (point-max))
    (insert "\n\nWorkflow Status:\n")
    (insert "================\n")
    (insert (format "Pending tasks:     %d\n" (length multi-agent-workflow-pending)))
    (insert (format "Completed reviews: %d\n"
                   (if multi-agent-workflow-results
                       (hash-table-count multi-agent-workflow-results)
                     0)))))

;;;###autoload
(defun multi-agent-workflow-demo ()
  "Run a complete demo of the multi-agent workflow."
  (interactive)

  (message "=== Multi-Agent Workflow Demo ===")

  ;; Step 1: Setup
  (message "Step 1: Setting up agents...")
  (multi-agent-workflow-setup)
  (sleep-for 1)

  ;; Step 2: Submit a review
  (message "Step 2: Submitting file for review...")
  (let ((demo-file (expand-file-name "gemini-repl-queue.el"
                                     (file-name-directory (or load-file-name
                                                             buffer-file-name)))))
    (when (file-exists-p demo-file)
      (multi-agent-workflow-review-file demo-file)
      (sleep-for 2)))

  ;; Step 3: Show status
  (message "Step 3: Checking status...")
  (sleep-for 3)
  (multi-agent-workflow-show-status)

  (message "Demo complete! Check the queue status buffer for results."))

;;; Integration with gemini-repl

;;;###autoload
(defun multi-agent-workflow-with-ai ()
  "Use actual AI (gemini-repl) for agent tasks."
  (interactive)

  (require 'gemini-repl)

  ;; Override handlers to use real AI
  (gemini-repl-queue-register-handler
   'code_analysis
   (lambda (request)
     (let ((content (alist-get 'content request)))
       (condition-case err
           (let ((result (gemini-repl-exec
                         (format "Analyze this code for quality, style, and maintainability:\n\n%s"
                                content))))
             (cons 'success result))
         (error (cons 'error (format "AI error: %s" err)))))))

  (gemini-repl-queue-register-handler
   'security_review
   (lambda (request)
     (let ((content (alist-get 'content request)))
       (condition-case err
           (let ((result (gemini-repl-exec
                         (format "Review this code for security vulnerabilities:\n\n%s"
                                content))))
             (cons 'success result))
         (error (cons 'error (format "AI error: %s" err)))))))

  (message "Multi-agent workflow configured with real AI integration"))

(provide 'multi-agent-workflow)
;;; multi-agent-workflow.el ends here
