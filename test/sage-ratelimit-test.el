;;; sage-ratelimit-test.el --- Tests for rate limiting -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sage-ratelimit module.

;;; Code:

(require 'ert)
(require 'sage-ratelimit)

;;; Limit Configuration Tests

(ert-deftest test-ratelimit-get-limit-gemini ()
  "Test rate limit retrieval for Gemini models."
  (should (= (sage-ratelimit--get-limit "gemini-1.5-flash-lite") 27))  ; 30 * 0.9
  (should (= (sage-ratelimit--get-limit "gemini-1.5-flash") 13))       ; 15 * 0.9 (floor)
  (should (= (sage-ratelimit--get-limit "gemini-1.5-pro") 4))          ; 5 * 0.9 (floor)
  (should (= (sage-ratelimit--get-limit "gemini-2.0-flash-exp") 9)))   ; 10 * 0.9

(ert-deftest test-ratelimit-get-limit-openai ()
  "Test rate limit retrieval for OpenAI models."
  (should (= (sage-ratelimit--get-limit "gpt-4o") 9))         ; 10 * 0.9
  (should (= (sage-ratelimit--get-limit "gpt-4o-mini") 27))   ; 30 * 0.9
  (should (= (sage-ratelimit--get-limit "gpt-3.5-turbo") 54))) ; 60 * 0.9

(ert-deftest test-ratelimit-get-limit-ollama ()
  "Test that Ollama models have unlimited rate limits."
  (should (null (sage-ratelimit--get-limit "ollama")))
  (should (null (sage-ratelimit--get-limit "llama3.2")))
  (should (null (sage-ratelimit--get-limit "llama3.1"))))

(ert-deftest test-ratelimit-get-limit-unknown ()
  "Test unknown model defaults to nil (unlimited)."
  (should (null (sage-ratelimit--get-limit "unknown-model"))))

;;; History Management Tests

(ert-deftest test-ratelimit-history-creation ()
  "Test that history ring is created for new models."
  (clrhash sage-ratelimit--history)
  (let ((history (sage-ratelimit--get-history "test-model")))
    (should (ring-p history))
    (should (ring-empty-p history))))

(ert-deftest test-ratelimit-record-request ()
  "Test recording requests."
  (clrhash sage-ratelimit--history)
  (sage-ratelimit-record "test-model")
  (sage-ratelimit-record "test-model")
  (should (= (sage-ratelimit--count-recent-requests "test-model") 2)))

(ert-deftest test-ratelimit-clean-old-requests ()
  "Test that old requests are cleaned from history."
  (clrhash sage-ratelimit--history)
  (let* ((history (sage-ratelimit--get-history "test-model"))
         (now (float-time))
         (old-time (- now 70)))  ; 70 seconds ago (outside 60s window)
    ;; Insert old request
    (ring-insert history old-time)
    ;; Insert recent request
    (ring-insert history now)
    (should (= (ring-length history) 2))
    ;; Clean old requests
    (sage-ratelimit--clean-old-requests history now)
    (should (= (ring-length history) 1))))

;;; Rate Limiting Logic Tests

(ert-deftest test-ratelimit-check-under-limit ()
  "Test that requests are allowed when under limit."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("test-model" . 10))))
    ;; Make 5 requests (under limit of 10 * 0.9 = 9)
    (dotimes (_ 5)
      (sage-ratelimit-record "test-model"))
    (should (sage-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-check-at-limit ()
  "Test that requests are blocked when at limit."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("test-model" . 10))))
    ;; Make 9 requests (at limit of 10 * 0.9 = 9)
    (dotimes (_ 9)
      (sage-ratelimit-record "test-model"))
    (should-not (sage-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-check-unlimited ()
  "Test that unlimited models always allow requests."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("test-model" . nil))))
    ;; Make many requests
    (dotimes (_ 100)
      (sage-ratelimit-record "test-model"))
    (should (sage-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-time-until-allowed ()
  "Test calculation of wait time."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("test-model" . 10))))
    ;; Under limit - no wait
    (dotimes (_ 5)
      (sage-ratelimit-record "test-model"))
    (should (= (sage-ratelimit--time-until-allowed "test-model") 0))

    ;; At limit - should have wait time
    (dotimes (_ 4)
      (sage-ratelimit-record "test-model"))
    (let ((wait (sage-ratelimit--time-until-allowed "test-model")))
      (should (> wait 0))
      (should (<= wait 60)))))

;;; Safety Margin Tests

(ert-deftest test-ratelimit-safety-margin ()
  "Test that safety margin is applied correctly."
  (let ((sage-ratelimit-safety-margin 0.5)  ; 50% margin
        (sage-ratelimit--limits '(("test-model" . 10))))
    (should (= (sage-ratelimit--get-limit "test-model") 5))))  ; 10 * 0.5

(ert-deftest test-ratelimit-different-safety-margins ()
  "Test different safety margin values."
  (let ((sage-ratelimit--limits '(("test-model" . 100))))
    ;; 100% - no safety margin
    (let ((sage-ratelimit-safety-margin 1.0))
      (should (= (sage-ratelimit--get-limit "test-model") 100)))
    ;; 80% margin
    (let ((sage-ratelimit-safety-margin 0.8))
      (should (= (sage-ratelimit--get-limit "test-model") 80)))
    ;; 50% margin
    (let ((sage-ratelimit-safety-margin 0.5))
      (should (= (sage-ratelimit--get-limit "test-model") 50)))))

;;; Integration Tests

(ert-deftest test-ratelimit-wrap-request ()
  "Test request wrapping with rate limiting."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("test-model" . 100)))
        (call-count 0))
    (sage-ratelimit-wrap-request
     "test-model"
     (lambda () (setq call-count (1+ call-count))))
    (should (= call-count 1))
    (should (= (sage-ratelimit--count-recent-requests "test-model") 1))))

(ert-deftest test-ratelimit-multiple-models ()
  "Test that different models have separate rate limits."
  (clrhash sage-ratelimit--history)
  (let ((sage-ratelimit--limits '(("model-a" . 10)
                                          ("model-b" . 20))))
    ;; Fill model-a to limit
    (dotimes (_ 9)
      (sage-ratelimit-record "model-a"))
    (should-not (sage-ratelimit-check "model-a"))

    ;; model-b should still be available
    (should (sage-ratelimit-check "model-b"))

    ;; Record for model-b
    (dotimes (_ 5)
      (sage-ratelimit-record "model-b"))
    (should (= (sage-ratelimit--count-recent-requests "model-b") 5))))

;;; Reset Tests

(ert-deftest test-ratelimit-reset-single ()
  "Test resetting rate limit for a single model."
  (clrhash sage-ratelimit--history)
  (sage-ratelimit-record "test-model")
  (should (= (sage-ratelimit--count-recent-requests "test-model") 1))
  (sage-ratelimit-reset "test-model")
  (should (= (sage-ratelimit--count-recent-requests "test-model") 0)))

(ert-deftest test-ratelimit-reset-all ()
  "Test resetting all rate limits."
  (clrhash sage-ratelimit--history)
  (sage-ratelimit-record "model-a")
  (sage-ratelimit-record "model-b")
  (should (= (sage-ratelimit--count-recent-requests "model-a") 1))
  (should (= (sage-ratelimit--count-recent-requests "model-b") 1))
  (sage-ratelimit-reset-all)
  (should (= (sage-ratelimit--count-recent-requests "model-a") 0))
  (should (= (sage-ratelimit--count-recent-requests "model-b") 0)))

;;; Status Display Tests

(ert-deftest test-ratelimit-status-returns-string ()
  "Test that sage-ratelimit-status returns without error for limited models.
Regression test for format string bug where message interpreted % as format spec."
  (clrhash sage-ratelimit--history)
  ;; Test with a limited model - this was causing the bug
  (let ((result nil))
    (should (progn
              (setq result (sage-ratelimit-status "gemini-1.5-flash"))
              t))
    ;; Result should be a string (the message returns its argument)
    (should (stringp result))
    ;; Should contain the model name
    (should (string-match-p "gemini-1.5-flash" result))
    ;; Should contain percentage sign (this is what triggered the bug)
    (should (string-match-p "%" result))))

(ert-deftest test-ratelimit-status-unlimited-model ()
  "Test that sage-ratelimit-status works for unlimited models."
  (clrhash sage-ratelimit--history)
  (let ((result (sage-ratelimit-status "ollama")))
    (should (stringp result))
    (should (string-match-p "ollama" result))
    (should (string-match-p "Unlimited" result))))

(ert-deftest test-ratelimit-status-with-requests ()
  "Test status display when there are recorded requests."
  (clrhash sage-ratelimit--history)
  ;; Record some requests
  (dotimes (_ 5)
    (sage-ratelimit-record "gemini-1.5-flash"))
  (let ((result (sage-ratelimit-status "gemini-1.5-flash")))
    (should (stringp result))
    ;; Should show 5 requests
    (should (string-match-p "5/" result))))

;;; Custom Limit Configuration Tests

(ert-deftest test-ratelimit-configure-custom-limit ()
  "Test setting custom rate limits."
  (let ((sage-ratelimit--limits '(("test-model" . 10))))
    (sage-ratelimit-configure-limit "test-model" 50)
    (should (= (alist-get "test-model" sage-ratelimit--limits nil nil #'string=) 50))))

(ert-deftest test-ratelimit-configure-unlimited ()
  "Test setting unlimited rate limit."
  (let ((sage-ratelimit--limits '(("test-model" . 10))))
    (sage-ratelimit-configure-limit "test-model" nil)
    (should (null (alist-get "test-model" sage-ratelimit--limits nil nil #'string=)))))

(provide 'sage-ratelimit-test)
;;; sage-ratelimit-test.el ends here
