;;; sage-ratelimit.el --- Rate limiting for sage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools, llm, rate-limiting
;; URL: https://github.com/aygp-dr/sage.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Rate limiting functionality for sage to prevent exceeding
;; API request limits for various LLM providers.
;;
;; Each provider has different rate limits (requests per minute):
;; - gemini-1.5-flash-lite: 30 RPM
;; - gemini-1.5-flash: 15 RPM
;; - gemini-1.5-pro: 5 RPM
;; - gemini-2.0-flash-exp: 10 RPM
;; - ollama (local): unlimited
;; - gpt-4o: 10 RPM
;;
;; Uses ring buffers to track request timestamps in a rolling 60-second window.
;; Applies a 90% safety margin to stay well within limits.

;;; Code:

(require 'ring)
(require 'cl-lib)

;;; Customization

(defgroup sage-ratelimit nil
  "Rate limiting for sage."
  :group 'sage
  :prefix "sage-ratelimit-")

(defcustom sage-ratelimit-safety-margin 0.9
  "Safety margin for rate limits (0.9 = use 90% of limit).
This provides a buffer to avoid hitting hard limits."
  :type 'float
  :group 'sage-ratelimit)

(defcustom sage-ratelimit-window 60
  "Time window in seconds for rate limiting (default: 60 for RPM)."
  :type 'integer
  :group 'sage-ratelimit)

(defcustom sage-ratelimit-show-countdown t
  "Show countdown timer when rate limited."
  :type 'boolean
  :group 'sage-ratelimit)

;;; Variables

(defvar sage-ratelimit--history (make-hash-table :test 'equal)
  "Hash table of request histories per model.
Keys are model names, values are ring buffers of timestamps.")

(defvar sage-ratelimit--limits
  '(("gemini-1.5-flash-lite" . 30)
    ("gemini-1.5-flash" . 15)
    ("gemini-1.5-pro" . 5)
    ("gemini-2.0-flash-exp" . 10)
    ("gemini-exp-1206" . 10)
    ("gpt-4o" . 10)
    ("gpt-4o-mini" . 30)
    ("gpt-4-turbo" . 10)
    ("gpt-3.5-turbo" . 60)
    ("ollama" . nil)  ; unlimited for local models
    ("llama3.2" . nil)
    ("llama3.1" . nil)
    ("llama2" . nil))
  "Alist of model names to their rate limits (requests per minute).
nil means unlimited.")

;;; Core Functions

(defun sage-ratelimit--get-limit (model)
  "Get rate limit for MODEL.
Returns nil for unlimited models."
  (let ((limit (alist-get model sage-ratelimit--limits nil nil #'string=)))
    (when limit
      ;; Apply safety margin
      (floor (* limit sage-ratelimit-safety-margin)))))

(defun sage-ratelimit--get-history (model)
  "Get or create request history ring buffer for MODEL."
  (or (gethash model sage-ratelimit--history)
      (let* ((limit (or (sage-ratelimit--get-limit model) 100))
             (ring (make-ring limit)))
        (puthash model ring sage-ratelimit--history)
        ring)))

(defun sage-ratelimit--clean-old-requests (history now)
  "Remove requests older than the rate limit window from HISTORY.
NOW is the current time as a float."
  (let ((cutoff (- now sage-ratelimit-window)))
    ;; Ring index 0 is newest, last index is oldest
    (while (and (not (ring-empty-p history))
                (< (ring-ref history (1- (ring-length history))) cutoff))
      (ring-remove history (1- (ring-length history))))))

(defun sage-ratelimit--count-recent-requests (model)
  "Count requests for MODEL in the current time window."
  (let* ((history (sage-ratelimit--get-history model))
         (now (float-time)))
    (sage-ratelimit--clean-old-requests history now)
    (ring-length history)))

(defun sage-ratelimit--time-until-allowed (model)
  "Calculate seconds until next request is allowed for MODEL.
Returns 0 if a request can be made now."
  (let ((limit (sage-ratelimit--get-limit model)))
    (if (not limit)
        0  ; unlimited
      (let* ((history (sage-ratelimit--get-history model))
             (now (float-time)))
        (sage-ratelimit--clean-old-requests history now)
        (if (< (ring-length history) limit)
            0  ; under limit
          ;; Over limit - calculate when oldest request expires
          (let ((oldest (ring-ref history 0)))
            (max 0 (- (+ oldest sage-ratelimit-window) now))))))))

;;;###autoload
(defun sage-ratelimit-check (model)
  "Check if a request can be made for MODEL.
Returns t if allowed, nil if rate limited."
  (zerop (sage-ratelimit--time-until-allowed model)))

;;;###autoload
(defun sage-ratelimit-record (model)
  "Record a request for MODEL."
  (let ((history (sage-ratelimit--get-history model))
        (now (float-time)))
    (sage-ratelimit--clean-old-requests history now)
    (ring-insert history now)))

;;;###autoload
(defun sage-ratelimit-wait (model)
  "Wait until a request is allowed for MODEL.
Shows countdown if configured to do so."
  (let ((wait-time (sage-ratelimit--time-until-allowed model)))
    (when (> wait-time 0)
      (if sage-ratelimit-show-countdown
          (sage-ratelimit--wait-with-countdown model wait-time)
        (progn
          (message "Rate limited for %s, waiting %.1f seconds..." model wait-time)
          (sleep-for wait-time))))))

(defun sage-ratelimit--wait-with-countdown (model wait-time)
  "Wait WAIT-TIME seconds for MODEL with countdown display."
  (let ((end-time (+ (float-time) wait-time))
        (original-msg (current-message)))
    (while (> (- end-time (float-time)) 0.1)
      (let ((remaining (- end-time (float-time))))
        (message "Rate limited for %s: %.1fs remaining..." model remaining)
        (sleep-for 0.5)))
    ;; Restore original message or clear
    (if original-msg
        (message "%s" original-msg)
      (message "Rate limit cleared for %s" model))))

;;;###autoload
(defun sage-ratelimit-status (model)
  "Show current rate limit status for MODEL."
  (interactive
   (list (completing-read "Model: "
                          (mapcar #'car sage-ratelimit--limits)
                          nil nil)))
  (let* ((limit (sage-ratelimit--get-limit model))
         (count (sage-ratelimit--count-recent-requests model))
         (wait (sage-ratelimit--time-until-allowed model))
         (can-request (zerop wait)))
    (message "%s"
             (if limit
                 (format "%s: %d/%d requests used (%.0f%% limit, safety margin: %.0f%%). %s"
                         model count limit
                         (* 100 (/ (float count) limit))
                         (* 100 sage-ratelimit-safety-margin)
                         (if can-request
                             "Ready"
                           (format "Wait %.1fs" wait)))
               (format "%s: Unlimited (local model)" model)))))

;;;###autoload
(defun sage-ratelimit-reset (model)
  "Reset rate limit history for MODEL."
  (interactive
   (list (completing-read "Model: "
                          (mapcar #'car sage-ratelimit--limits)
                          nil nil)))
  (remhash model sage-ratelimit--history)
  (message "Rate limit history cleared for %s" model))

;;;###autoload
(defun sage-ratelimit-reset-all ()
  "Reset all rate limit histories."
  (interactive)
  (clrhash sage-ratelimit--history)
  (message "All rate limit histories cleared"))

;;;###autoload
(defun sage-ratelimit-configure-limit (model limit)
  "Set custom rate LIMIT for MODEL.
LIMIT is requests per minute, or nil for unlimited."
  (interactive
   (list (completing-read "Model: "
                          (mapcar #'car sage-ratelimit--limits)
                          nil nil)
         (let ((input (read-string "Limit (RPM, blank for unlimited): ")))
           (if (string-empty-p input)
               nil
             (string-to-number input)))))
  (setf (alist-get model sage-ratelimit--limits nil nil #'string=) limit)
  (message "Set limit for %s to %s RPM"
           model
           (if limit (format "%d" limit) "unlimited")))

;;; Modeline Support

(defvar sage-ratelimit--modeline-string ""
  "Current modeline display string for rate limits.")

(defun sage-ratelimit--update-modeline (model)
  "Update modeline string with rate limit status for MODEL."
  (let* ((limit (sage-ratelimit--get-limit model))
         (count (sage-ratelimit--count-recent-requests model))
         (wait (sage-ratelimit--time-until-allowed model)))
    (setq sage-ratelimit--modeline-string
          (if limit
              (if (> wait 0)
                  (format " [RL:%.0fs]" wait)
                (format " [RL:%d/%d]" count limit))
            ""))))

;;;###autoload
(defun sage-ratelimit-enable-modeline (model)
  "Enable modeline display for MODEL rate limiting."
  (interactive
   (list (completing-read "Model: "
                          (mapcar #'car sage-ratelimit--limits)
                          nil nil)))
  (unless (member '(:eval sage-ratelimit--modeline-string) mode-line-format)
    (setq mode-line-format
          (append mode-line-format
                  '((:eval sage-ratelimit--modeline-string)))))
  (run-with-timer 0 1
                  (lambda ()
                    (sage-ratelimit--update-modeline model)
                    (force-mode-line-update t))))

;;; Integration Helpers

;;;###autoload
(defun sage-ratelimit-wrap-request (model request-fn)
  "Wrap REQUEST-FN with rate limiting for MODEL.
REQUEST-FN should be a function that makes the API request.
Returns the result of REQUEST-FN after waiting if necessary."
  (sage-ratelimit-wait model)
  (sage-ratelimit-record model)
  (funcall request-fn))

(provide 'sage-ratelimit)
;;; sage-ratelimit.el ends here
