;;; ratelimit-demo.el --- Rate limiting examples for gemini-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates various rate limiting features of gemini-repl.

;;; Code:

(require 'gemini-repl-ratelimit)

;;; Example 1: Basic Rate Limit Check

(defun example-check-rate-limit ()
  "Check if we can make a request."
  (let ((model "gemini-2.0-flash-exp"))
    (if (gemini-repl-ratelimit-check model)
        (message "✓ Can make request to %s" model)
      (message "✗ Rate limited for %s, wait %.1fs"
               model
               (gemini-repl-ratelimit--time-until-allowed model)))))

;; Try it:
;; (example-check-rate-limit)

;;; Example 2: Making Multiple Requests with Rate Limiting

(defun example-burst-requests (count)
  "Make COUNT requests with automatic rate limiting.
Shows how the system handles bursts."
  (interactive "nNumber of requests: ")
  (let ((model "gemini-2.0-flash-exp")
        (results '()))
    (dotimes (i count)
      (let ((start (float-time)))
        (gemini-repl-ratelimit-wait model)
        (gemini-repl-ratelimit-record model)
        (let ((wait-time (- (float-time) start)))
          (push (format "Request %d: waited %.2fs" (1+ i) wait-time)
                results))))
    (message "Completed %d requests:\n%s"
             count
             (mapconcat #'identity (reverse results) "\n"))))

;; Try it:
;; (example-burst-requests 5)

;;; Example 3: Monitoring Rate Limit Status

(defun example-monitor-status (model duration)
  "Monitor rate limit status for MODEL over DURATION seconds."
  (interactive
   (list (completing-read "Model: "
                          '("gemini-2.0-flash-exp" "gpt-4o" "llama3.2"))
         (read-number "Duration (seconds): " 10)))
  (let ((end-time (+ (float-time) duration)))
    (while (< (float-time) end-time)
      (let* ((count (gemini-repl-ratelimit--count-recent-requests model))
             (limit (gemini-repl-ratelimit--get-limit model))
             (wait (gemini-repl-ratelimit--time-until-allowed model)))
        (message "[%s] %d/%s requests, wait: %.1fs"
                 model
                 count
                 (if limit (format "%d" limit) "∞")
                 wait))
      (sleep-for 1))))

;; Try it:
;; (example-monitor-status "gemini-2.0-flash-exp" 10)

;;; Example 4: Custom Rate Limits

(defun example-custom-limits ()
  "Demonstrate setting custom rate limits."
  ;; Set a very low limit for testing
  (gemini-repl-ratelimit-configure-limit "test-model" 3)

  ;; Make requests until limited
  (dotimes (i 5)
    (if (gemini-repl-ratelimit-check "test-model")
        (progn
          (gemini-repl-ratelimit-record "test-model")
          (message "Request %d: ✓ Allowed" (1+ i)))
      (message "Request %d: ✗ Rate limited" (1+ i))))

  ;; Reset for next test
  (gemini-repl-ratelimit-reset "test-model"))

;; Try it:
;; (example-custom-limits)

;;; Example 5: Wrapping API Calls

(defun example-wrapped-call ()
  "Demonstrate wrapping an API call with rate limiting."
  (let ((model "gemini-2.0-flash-exp")
        (call-count 0))
    (gemini-repl-ratelimit-wrap-request
     model
     (lambda ()
       (setq call-count (1+ call-count))
       (message "API call executed (count: %d)" call-count)
       ;; Simulate API response
       "API response data"))
    (message "Call completed, total calls: %d" call-count)))

;; Try it:
;; (example-wrapped-call)

;;; Example 6: Safety Margin Configuration

(defun example-safety-margins ()
  "Demonstrate different safety margins."
  (let ((model "test-model")
        (base-limit 10))
    ;; Configure base limit
    (gemini-repl-ratelimit-configure-limit model base-limit)

    ;; Test different safety margins
    (dolist (margin '(1.0 0.9 0.8 0.5))
      (let ((gemini-repl-ratelimit-safety-margin margin))
        (message "Safety margin: %.0f%% → Effective limit: %d/%d"
                 (* 100 margin)
                 (gemini-repl-ratelimit--get-limit model)
                 base-limit)))

    ;; Cleanup
    (gemini-repl-ratelimit-reset model)))

;; Try it:
;; (example-safety-margins)

;;; Example 7: Comparing Models

(defun example-compare-models ()
  "Compare rate limits across different models."
  (let ((models '("gemini-1.5-flash-lite"
                  "gemini-1.5-flash"
                  "gemini-1.5-pro"
                  "gemini-2.0-flash-exp"
                  "gpt-4o"
                  "gpt-4o-mini"
                  "llama3.2")))
    (message "Model Rate Limit Comparison:\n")
    (dolist (model models)
      (let ((limit (gemini-repl-ratelimit--get-limit model))
            (raw-limit (alist-get model gemini-repl-ratelimit--limits
                                  nil nil #'string=)))
        (message "  %s: %s → %s (with safety margin)"
                 (format "%-25s" model)
                 (if raw-limit (format "%2d RPM" raw-limit) "unlimited")
                 (if limit (format "%2d RPM" limit) "unlimited"))))))

;; Try it:
;; (example-compare-models)

;;; Example 8: Simulating High Load

(defun example-high-load (requests-per-second duration)
  "Simulate high load with REQUESTS-PER-SECOND for DURATION seconds.
Shows how rate limiting prevents exceeding limits."
  (interactive
   (list (read-number "Requests per second: " 2)
         (read-number "Duration (seconds): " 5)))
  (let ((model "gemini-2.0-flash-exp")
        (total-requests 0)
        (completed-requests 0)
        (end-time (+ (float-time) duration))
        (interval (/ 1.0 requests-per-second)))

    (message "Starting high load test: %d req/s for %ds"
             requests-per-second duration)

    (while (< (float-time) end-time)
      (setq total-requests (1+ total-requests))

      (if (gemini-repl-ratelimit-check model)
          (progn
            (gemini-repl-ratelimit-record model)
            (setq completed-requests (1+ completed-requests))
            (message "Request %d: completed" total-requests))
        (message "Request %d: rate limited (skipped)" total-requests))

      (sleep-for interval))

    (message "\nHigh load test complete:")
    (message "  Attempted: %d requests" total-requests)
    (message "  Completed: %d requests" completed-requests)
    (message "  Blocked: %d requests" (- total-requests completed-requests))
    (message "  Success rate: %.1f%%"
             (* 100 (/ (float completed-requests) total-requests)))))

;; Try it:
;; (example-high-load 2 10)

;;; Example 9: Interactive Status Display

(defun example-interactive-status ()
  "Display interactive rate limit status for all models."
  (interactive)
  (with-current-buffer (get-buffer-create "*Rate Limit Status*")
    (read-only-mode -1)
    (erase-buffer)
    (insert "Rate Limit Status\n")
    (insert "=================\n\n")

    (dolist (model-limit gemini-repl-ratelimit--limits)
      (let* ((model (car model-limit))
             (limit (gemini-repl-ratelimit--get-limit model))
             (count (gemini-repl-ratelimit--count-recent-requests model))
             (wait (gemini-repl-ratelimit--time-until-allowed model)))
        (insert (format "%-25s " model))
        (if limit
            (insert (format "[%2d/%2d] " count limit)
                    (if (> wait 0)
                        (format "WAIT %.1fs" wait)
                      "READY"))
          (insert "[unlimited] READY"))
        (insert "\n")))

    (read-only-mode 1)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Try it:
;; M-x example-interactive-status

;;; Example 10: Testing Rate Limit Recovery

(defun example-test-recovery ()
  "Test how quickly rate limits recover."
  (interactive)
  (let ((model "test-model"))
    ;; Set a low limit for testing
    (gemini-repl-ratelimit-configure-limit model 3)

    ;; Fill up the limit
    (message "Filling up rate limit...")
    (dotimes (i 3)
      (gemini-repl-ratelimit-record model)
      (message "  Request %d recorded" (1+ i)))

    (message "\nRate limit full. Waiting for recovery...")

    ;; Monitor recovery
    (dotimes (i 60)
      (let ((count (gemini-repl-ratelimit--count-recent-requests model))
            (can-request (gemini-repl-ratelimit-check model)))
        (message "[%2ds] Count: %d/3, Can request: %s"
                 (1+ i) count (if can-request "YES" "NO")))
      (sleep-for 1))

    ;; Cleanup
    (gemini-repl-ratelimit-reset model)
    (message "\nTest complete!")))

;; Try it:
;; M-x example-test-recovery

(provide 'ratelimit-demo)
;;; ratelimit-demo.el ends here
