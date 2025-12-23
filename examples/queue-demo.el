;;; queue-demo.el --- Demonstration of gemini-repl-queue usage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh

;;; Commentary:

;; This file demonstrates various usage patterns for the gemini-repl-queue
;; file-based inter-agent communication system.

;;; Code:

(require 'gemini-repl-queue)

;;; Example 1: Basic Request/Response Pattern

(defun queue-demo-basic ()
  "Demonstrate basic request submission and polling."
  (interactive)

  ;; Ensure queue system is initialized
  (gemini-repl-queue--ensure-directories)

  ;; Submit a ping request
  (let ((id (gemini-repl-queue-submit 'ping "Hello from demo")))
    (message "Submitted request: %s" id)

    ;; Poll for the request (in real usage, another agent would do this)
    (let ((request (gemini-repl-queue-poll)))
      (when request
        (message "Found request: %s" (alist-get 'content request))

        ;; Respond to the request
        (gemini-repl-queue-respond id 'success "Pong!")

        ;; Archive the completed request
        (gemini-repl-queue-archive id)))))

;;; Example 2: Custom Handler

(defun queue-demo-custom-handler ()
  "Demonstrate custom request handler registration."
  (interactive)

  ;; Register a custom handler for "math" requests
  (gemini-repl-queue-register-handler
   'math
   (lambda (request)
     (let* ((content (alist-get 'content request))
            (numbers (mapcar #'string-to-number (split-string content "+")))
            (sum (apply #'+ numbers)))
       (cons 'success (format "Sum: %d" sum)))))

  ;; Submit a math request
  (let ((id (gemini-repl-queue-submit 'math "10+20+30")))
    (message "Submitted math request: %s" id)

    ;; Process the request (simulating auto-processing)
    (let ((request (gemini-repl-queue-poll)))
      (when request
        (gemini-repl-queue--process-request request)))))

;;; Example 3: Watch Mode

(defun queue-demo-watch-mode ()
  "Demonstrate automatic request processing with watch mode."
  (interactive)

  ;; Enable watch mode
  (gemini-repl-queue-watch-mode 1)

  (message "Watch mode enabled. Submit requests to be auto-processed.")
  (message "Try: (gemini-repl-queue-submit 'ping \"test\")")

  ;; To disable:
  ;; (gemini-repl-queue-watch-mode -1)
  )

;;; Example 4: Agent Communication

(defun queue-demo-agent-communication ()
  "Demonstrate communication between multiple agents."
  (interactive)

  ;; Agent A sends a message to Agent B
  (gemini-repl-queue-send-to-agent "agent-b" "Please process this data")

  ;; Broadcast to all agents
  (gemini-repl-queue-broadcast "System maintenance in 5 minutes"))

;;; Example 5: Asynchronous Workflow

(defun queue-demo-async-workflow ()
  "Demonstrate asynchronous multi-step workflow."
  (interactive)

  ;; Register handlers for workflow steps
  (gemini-repl-queue-register-handler
   'step1
   (lambda (request)
     (let ((data (alist-get 'content request)))
       ;; Process step 1
       (sleep-for 1)
       ;; Queue next step
       (gemini-repl-queue-submit 'step2 (format "Processed: %s" data))
       (cons 'success "Step 1 complete"))))

  (gemini-repl-queue-register-handler
   'step2
   (lambda (request)
     (let ((data (alist-get 'content request)))
       ;; Process step 2
       (sleep-for 1)
       (cons 'success (format "Step 2 complete: %s" data)))))

  ;; Start the workflow
  (gemini-repl-queue-submit 'step1 "Initial data")

  ;; Enable watch mode to auto-process
  (gemini-repl-queue-watch-mode 1))

;;; Example 6: Batch Processing

(defun queue-demo-batch-processing ()
  "Demonstrate batch request submission and processing."
  (interactive)

  ;; Submit multiple requests
  (dotimes (i 5)
    (gemini-repl-queue-submit 'ping (format "Batch request %d" (1+ i))))

  (message "Submitted 5 batch requests")

  ;; Process all pending (if watch mode is disabled)
  (when (not gemini-repl-queue-watch-mode)
    (gemini-repl-queue--process-all)))

;;; Example 7: Error Handling

(defun queue-demo-error-handling ()
  "Demonstrate error handling in request processing."
  (interactive)

  ;; Register a handler that may fail
  (gemini-repl-queue-register-handler
   'risky
   (lambda (request)
     (if (string-match "fail" (alist-get 'content request))
         (error "Intentional failure for testing")
       (cons 'success "Success!"))))

  ;; Submit requests
  (let ((id1 (gemini-repl-queue-submit 'risky "normal request"))
        (id2 (gemini-repl-queue-submit 'risky "fail this request")))

    ;; Process them
    (gemini-repl-queue--process-all)

    (message "Check output directory for success and error responses")))

;;; Example 8: Monitoring and Statistics

(defun queue-demo-monitoring ()
  "Demonstrate queue monitoring and statistics."
  (interactive)

  ;; Submit some test requests
  (dotimes (i 3)
    (gemini-repl-queue-submit 'ping (format "Test %d" (1+ i))))

  ;; Show status
  (gemini-repl-queue-status))

;;; Example 9: Context and Metadata

(defun queue-demo-context ()
  "Demonstrate using context and metadata."
  (interactive)

  ;; Submit request with context
  (let ((id (gemini-repl-queue-submit
             'prompt
             "Summarize this project"
             '((project . "gemini-repl-010")
               (priority . "high")
               (tags . ("documentation" "summary"))))))

    ;; Handler can access context
    (gemini-repl-queue-register-handler
     'prompt
     (lambda (request)
       (let* ((content (alist-get 'content request))
              (context (alist-get 'context request))
              (project (alist-get 'project context)))
         (cons 'success
               (format "Processing '%s' for project: %s" content project)))))))

;;; Example 10: Cleanup and Maintenance

(defun queue-demo-cleanup ()
  "Demonstrate archive cleanup."
  (interactive)

  ;; Process some requests
  (dotimes (i 3)
    (let ((id (gemini-repl-queue-submit 'ping (format "Old request %d" (1+ i)))))
      (gemini-repl-queue-respond id 'success "done")
      (gemini-repl-queue-archive id)))

  ;; Clean up old archives (older than 7 days by default)
  (gemini-repl-queue-cleanup-archives))

;;; Interactive Demo Menu

;;;###autoload
(defun queue-demo-menu ()
  "Show menu of queue system demos."
  (interactive)
  (let ((choice (completing-read
                 "Select demo: "
                 '("1. Basic Request/Response"
                   "2. Custom Handler"
                   "3. Watch Mode"
                   "4. Agent Communication"
                   "5. Async Workflow"
                   "6. Batch Processing"
                   "7. Error Handling"
                   "8. Monitoring"
                   "9. Context/Metadata"
                   "10. Cleanup"))))
    (pcase choice
      ("1. Basic Request/Response" (queue-demo-basic))
      ("2. Custom Handler" (queue-demo-custom-handler))
      ("3. Watch Mode" (queue-demo-watch-mode))
      ("4. Agent Communication" (queue-demo-agent-communication))
      ("5. Async Workflow" (queue-demo-async-workflow))
      ("6. Batch Processing" (queue-demo-batch-processing))
      ("7. Error Handling" (queue-demo-error-handling))
      ("8. Monitoring" (queue-demo-monitoring))
      ("9. Context/Metadata" (queue-demo-context))
      ("10. Cleanup" (queue-demo-cleanup)))))

;;; Real-World Example: Multi-Agent Code Review

(defun queue-demo-code-review-workflow ()
  "Demonstrate a multi-agent code review workflow."
  (interactive)

  ;; Submitter agent
  (defun submit-code-for-review (file)
    (gemini-repl-queue-submit
     'code_review
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string))
     `((file . ,file)
       (author . ,(user-login-name))
       (timestamp . ,(current-time-string)))))

  ;; Reviewer agent handler
  (gemini-repl-queue-register-handler
   'code_review
   (lambda (request)
     (let* ((code (alist-get 'content request))
            (context (alist-get 'context request))
            (file (alist-get 'file context)))
       ;; Simulate code analysis
       (cons 'success
             (format "Review for %s:\n- Code looks good\n- Consider adding tests\n- Document public APIs"
                     file)))))

  ;; Submit a file for review
  (when-let ((file (buffer-file-name)))
    (submit-code-for-review file)
    (message "Submitted %s for review" file)

    ;; Enable watch mode to auto-process
    (gemini-repl-queue-watch-mode 1)))

(provide 'queue-demo)
;;; queue-demo.el ends here
