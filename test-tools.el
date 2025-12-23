;;; test-tools.el --- Test gemini-repl-tools loading

(setq debug-on-error t)

(condition-case err
    (progn
      (load-file "gemini-repl-tools.el")
      (message "Loaded gemini-repl-tools.el"))
  (error (message "Load error: %s" err)))

(condition-case err
    (progn
      (gemini-repl--init-default-tools)
      (message "Initialized tools"))
  (error (message "Init error: %s" err)))

(when (boundp 'gemini-repl-tools)
  (message "Tools count: %d" (length gemini-repl-tools)))

;;; test-tools.el ends here
