;;; test-memory.el --- Quick test -*- lexical-binding: t; -*-

(add-to-list 'load-path ".")

(condition-case err
    (progn
      (message "Loading gemini-repl-memory...")
      (load-file "gemini-repl-memory.el")
      (message "Success! Testing functions...")

      ;; Test basic functionality
      (gemini-repl-memory-add "test" "value" 'general)
      (message "Added fact")

      (let ((fact (gemini-repl-memory-get "test")))
        (message "Retrieved: %S" fact))

      (message "All tests passed!"))
  (error
   (message "ERROR: %S" err)
   (message "Error message: %s" (error-message-string err))))
