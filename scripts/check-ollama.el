;;; check-ollama.el --- Test Ollama connectivity -*- lexical-binding: t; -*-

;; Test script for verifying Ollama API connectivity

(require 'url)
(require 'json)

(defun sage-check-ollama ()
  "Check if Ollama is running and list available models."
  (condition-case err
      (let ((buffer (url-retrieve-synchronously
                     "http://localhost:11434/api/tags" nil t 5)))
        (if buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (let ((data (json-read)))
                (message "Ollama models: %s"
                         (mapcar (lambda (m) (cdr (assq 'name m)))
                                 (cdr (assq 'models data))))))
          (message "No response from Ollama")))
    (error (message "Ollama not running at localhost:11434"))))

(sage-check-ollama)

;;; check-ollama.el ends here
