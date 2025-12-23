;;; gemini-repl-ratelimit-test.el --- Tests for rate limiting -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for gemini-repl-ratelimit module.

;;; Code:

(require 'ert)
(require 'gemini-repl-ratelimit)

;;; Limit Configuration Tests

(ert-deftest test-ratelimit-get-limit-gemini ()
  "Test rate limit retrieval for Gemini models."
  (should (= (gemini-repl-ratelimit--get-limit "gemini-1.5-flash-lite") 27))  ; 30 * 0.9
  (should (= (gemini-repl-ratelimit--get-limit "gemini-1.5-flash") 13))       ; 15 * 0.9 (floor)
  (should (= (gemini-repl-ratelimit--get-limit "gemini-1.5-pro") 4))          ; 5 * 0.9 (floor)
  (should (= (gemini-repl-ratelimit--get-limit "gemini-2.0-flash-exp") 9)))   ; 10 * 0.9

(ert-deftest test-ratelimit-get-limit-openai ()
  "Test rate limit retrieval for OpenAI models."
  (should (= (gemini-repl-ratelimit--get-limit "gpt-4o") 9))         ; 10 * 0.9
  (should (= (gemini-repl-ratelimit--get-limit "gpt-4o-mini") 27))   ; 30 * 0.9
  (should (= (gemini-repl-ratelimit--get-limit "gpt-3.5-turbo") 54))) ; 60 * 0.9

(ert-deftest test-ratelimit-get-limit-ollama ()
  "Test that Ollama models have unlimited rate limits."
  (should (null (gemini-repl-ratelimit--get-limit "ollama")))
  (should (null (gemini-repl-ratelimit--get-limit "llama3.2")))
  (should (null (gemini-repl-ratelimit--get-limit "llama3.1"))))

(ert-deftest test-ratelimit-get-limit-unknown ()
  "Test unknown model defaults to nil (unlimited)."
  (should (null (gemini-repl-ratelimit--get-limit "unknown-model"))))

;;; History Management Tests

(ert-deftest test-ratelimit-history-creation ()
  "Test that history ring is created for new models."
  (clrhash gemini-repl-ratelimit--history)
  (let ((history (gemini-repl-ratelimit--get-history "test-model")))
    (should (ring-p history))
    (should (ring-empty-p history))))

(ert-deftest test-ratelimit-record-request ()
  "Test recording requests."
  (clrhash gemini-repl-ratelimit--history)
  (gemini-repl-ratelimit-record "test-model")
  (gemini-repl-ratelimit-record "test-model")
  (should (= (gemini-repl-ratelimit--count-recent-requests "test-model") 2)))

(ert-deftest test-ratelimit-clean-old-requests ()
  "Test that old requests are cleaned from history."
  (clrhash gemini-repl-ratelimit--history)
  (let* ((history (gemini-repl-ratelimit--get-history "test-model"))
         (now (float-time))
         (old-time (- now 70)))  ; 70 seconds ago (outside 60s window)
    ;; Insert old request
    (ring-insert history old-time)
    ;; Insert recent request
    (ring-insert history now)
    (should (= (ring-length history) 2))
    ;; Clean old requests
    (gemini-repl-ratelimit--clean-old-requests history now)
    (should (= (ring-length history) 1))))

;;; Rate Limiting Logic Tests

(ert-deftest test-ratelimit-check-under-limit ()
  "Test that requests are allowed when under limit."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 10))))
    ;; Make 5 requests (under limit of 10 * 0.9 = 9)
    (dotimes (_ 5)
      (gemini-repl-ratelimit-record "test-model"))
    (should (gemini-repl-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-check-at-limit ()
  "Test that requests are blocked when at limit."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 10))))
    ;; Make 9 requests (at limit of 10 * 0.9 = 9)
    (dotimes (_ 9)
      (gemini-repl-ratelimit-record "test-model"))
    (should-not (gemini-repl-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-check-unlimited ()
  "Test that unlimited models always allow requests."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("test-model" . nil))))
    ;; Make many requests
    (dotimes (_ 100)
      (gemini-repl-ratelimit-record "test-model"))
    (should (gemini-repl-ratelimit-check "test-model"))))

(ert-deftest test-ratelimit-time-until-allowed ()
  "Test calculation of wait time."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 10))))
    ;; Under limit - no wait
    (dotimes (_ 5)
      (gemini-repl-ratelimit-record "test-model"))
    (should (= (gemini-repl-ratelimit--time-until-allowed "test-model") 0))

    ;; At limit - should have wait time
    (dotimes (_ 4)
      (gemini-repl-ratelimit-record "test-model"))
    (let ((wait (gemini-repl-ratelimit--time-until-allowed "test-model")))
      (should (> wait 0))
      (should (<= wait 60)))))

;;; Safety Margin Tests

(ert-deftest test-ratelimit-safety-margin ()
  "Test that safety margin is applied correctly."
  (let ((gemini-repl-ratelimit-safety-margin 0.5)  ; 50% margin
        (gemini-repl-ratelimit--limits '(("test-model" . 10))))
    (should (= (gemini-repl-ratelimit--get-limit "test-model") 5))))  ; 10 * 0.5

(ert-deftest test-ratelimit-different-safety-margins ()
  "Test different safety margin values."
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 100))))
    ;; 100% - no safety margin
    (let ((gemini-repl-ratelimit-safety-margin 1.0))
      (should (= (gemini-repl-ratelimit--get-limit "test-model") 100)))
    ;; 80% margin
    (let ((gemini-repl-ratelimit-safety-margin 0.8))
      (should (= (gemini-repl-ratelimit--get-limit "test-model") 80)))
    ;; 50% margin
    (let ((gemini-repl-ratelimit-safety-margin 0.5))
      (should (= (gemini-repl-ratelimit--get-limit "test-model") 50)))))

;;; Integration Tests

(ert-deftest test-ratelimit-wrap-request ()
  "Test request wrapping with rate limiting."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 100)))
        (call-count 0))
    (gemini-repl-ratelimit-wrap-request
     "test-model"
     (lambda () (setq call-count (1+ call-count))))
    (should (= call-count 1))
    (should (= (gemini-repl-ratelimit--count-recent-requests "test-model") 1))))

(ert-deftest test-ratelimit-multiple-models ()
  "Test that different models have separate rate limits."
  (clrhash gemini-repl-ratelimit--history)
  (let ((gemini-repl-ratelimit--limits '(("model-a" . 10)
                                          ("model-b" . 20))))
    ;; Fill model-a to limit
    (dotimes (_ 9)
      (gemini-repl-ratelimit-record "model-a"))
    (should-not (gemini-repl-ratelimit-check "model-a"))

    ;; model-b should still be available
    (should (gemini-repl-ratelimit-check "model-b"))

    ;; Record for model-b
    (dotimes (_ 5)
      (gemini-repl-ratelimit-record "model-b"))
    (should (= (gemini-repl-ratelimit--count-recent-requests "model-b") 5))))

;;; Reset Tests

(ert-deftest test-ratelimit-reset-single ()
  "Test resetting rate limit for a single model."
  (clrhash gemini-repl-ratelimit--history)
  (gemini-repl-ratelimit-record "test-model")
  (should (= (gemini-repl-ratelimit--count-recent-requests "test-model") 1))
  (gemini-repl-ratelimit-reset "test-model")
  (should (= (gemini-repl-ratelimit--count-recent-requests "test-model") 0)))

(ert-deftest test-ratelimit-reset-all ()
  "Test resetting all rate limits."
  (clrhash gemini-repl-ratelimit--history)
  (gemini-repl-ratelimit-record "model-a")
  (gemini-repl-ratelimit-record "model-b")
  (should (= (gemini-repl-ratelimit--count-recent-requests "model-a") 1))
  (should (= (gemini-repl-ratelimit--count-recent-requests "model-b") 1))
  (gemini-repl-ratelimit-reset-all)
  (should (= (gemini-repl-ratelimit--count-recent-requests "model-a") 0))
  (should (= (gemini-repl-ratelimit--count-recent-requests "model-b") 0)))

;;; Custom Limit Configuration Tests

(ert-deftest test-ratelimit-configure-custom-limit ()
  "Test setting custom rate limits."
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 10))))
    (gemini-repl-ratelimit-configure-limit "test-model" 50)
    (should (= (alist-get "test-model" gemini-repl-ratelimit--limits nil nil #'string=) 50))))

(ert-deftest test-ratelimit-configure-unlimited ()
  "Test setting unlimited rate limit."
  (let ((gemini-repl-ratelimit--limits '(("test-model" . 10))))
    (gemini-repl-ratelimit-configure-limit "test-model" nil)
    (should (null (alist-get "test-model" gemini-repl-ratelimit--limits nil nil #'string=)))))

(provide 'gemini-repl-ratelimit-test)
;;; gemini-repl-ratelimit-test.el ends here
