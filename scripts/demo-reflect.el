;;; demo-reflect.el --- Demonstrate sage-reflect functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Demonstrates the sage-reflect module's session retrospective capabilities.
;; This shows what sage-reflect-show-summary produces after a typical session.
;;
;; Usage:
;;   make demo-reflect
;;   # or
;;   emacs --batch -L . -l scripts/demo-reflect.el

;;; Code:

(require 'cl-lib)
(require 'sage-reflect)
(require 'sage-tools)

;;; Simulate a session

(defun demo-reflect--simulate-session ()
  "Simulate a typical sage session with tool calls."
  (message "")
  (message "╔══════════════════════════════════════════════════════════════╗")
  (message "║          SAGE REFLECTION DEMO - Session Retrospective        ║")
  (message "╚══════════════════════════════════════════════════════════════╝")
  (message "")

  ;; Start reflection
  (message "1. Enabling reflection for session...")
  (sage-reflect-enable)
  (message "   ✓ Reflection enabled\n")

  ;; Simulate tool calls
  (message "2. Simulating tool calls (typical development session)...")

  ;; File operations
  (sage-reflect-record-tool-call "read_file" '((path . "main.py")) "def main()..." 45 t)
  (sage-reflect-record-tool-call "read_file" '((path . "utils.py")) "import os..." 32 t)
  (sage-reflect-record-tool-call "read_file" '((path . "config.json")) "{}" 28 t)
  (sage-reflect-record-tool-call "write_file" '((path . "output.txt")) "Wrote 100 bytes" 156 t)
  (sage-reflect-record-tool-call "edit_file" '((path . "main.py")) "Edited" 89 t)

  ;; Search operations
  (sage-reflect-record-tool-call "code_search" '((pattern . "TODO")) "3 matches" 234 t)
  (sage-reflect-record-tool-call "glob_files" '((pattern . "*.py")) "5 files" 67 t)
  (sage-reflect-record-tool-call "list_files" '((path . ".")) "10 entries" 45 t)

  ;; Git operations
  (sage-reflect-record-tool-call "git_status" '() "2 modified" 123 t)
  (sage-reflect-record-tool-call "git_diff" '() "diff output..." 89 t)

  ;; Some errors
  (sage-reflect-record-tool-call "read_file" '((path . "missing.txt")) "File not found" 12 nil)
  (sage-reflect-record-tool-call "web_fetch" '((url . "http://example.com")) "Timeout" 5000 nil)

  ;; Slow operation
  (sage-reflect-record-tool-call "code_search" '((pattern . "complex")) "100 matches" 2500 t)

  (message "   ✓ Recorded 13 tool calls (11 success, 2 errors)\n")

  ;; Simulate context usage
  (message "3. Simulating context usage (75%% of limit)...")
  (setq sage-reflect--last-threshold-warned 0.0)
  (sage-reflect-check-context 0.55)
  (sage-reflect-check-context 0.76)
  (message "   ✓ Context warnings triggered at 50%% and 75%%\n"))

(defun demo-reflect--show-status ()
  "Show reflection status."
  (message "4. Current Reflection Status:")
  (message "   ────────────────────────────────────────")

  (let ((status (sage-reflect-context-status)))
    (message "   Session duration: %s" (alist-get 'session-duration status))
    (message "   Warnings issued: %d" (alist-get 'warnings-issued status))
    (message "   Recommendation: %s" (alist-get 'recommendation status)))
  (message ""))

(defun demo-reflect--show-tool-analysis ()
  "Show tool analysis."
  (message "5. Tool Usage Analysis:")
  (message "   ────────────────────────────────────────")

  (let ((analysis (sage-reflect-tool-analysis)))
    (message "   Total calls: %d" (alist-get 'total-calls analysis))
    (message "   Total time: %dms" (alist-get 'total-time-ms analysis))
    (message "   Error rate: %d%%" (alist-get 'error-rate analysis))

    (message "\n   Top tools:")
    (dolist (tool (alist-get 'top-tools analysis))
      (message "     - %s: %d calls" (car tool) (cdr tool)))

    (when-let ((slow (alist-get 'slow-tools analysis)))
      (message "\n   Slow tools (>1s avg):")
      (dolist (tool slow)
        (message "     - %s: %dms avg" (car tool) (cdr tool))))

    (when-let ((errors (alist-get 'error-prone-tools analysis)))
      (message "\n   Error-prone tools:")
      (dolist (tool errors)
        (message "     - %s: %d%% error rate" (car tool) (cdr tool))))

    (message "\n   Insights:")
    (dolist (insight (alist-get 'insights analysis))
      (message "     • %s" insight)))
  (message ""))

(defun demo-reflect--show-summary ()
  "Show session summary (the main retrospective)."
  (message "6. Session Retrospective (sage-reflect-show-summary):")
  (message "   ════════════════════════════════════════════════════════")
  (message "")

  (let ((summary (sage-reflect-session-summary)))
    ;; Duration
    (message "   Duration: %s" (alist-get 'session-duration summary))
    (message "")

    ;; Context
    (let ((context (alist-get 'context summary)))
      (message "   Context Usage:")
      (message "     • Warnings issued: %d" (alist-get 'warnings-count summary))
      (message "     • Recommendation: %s"
               (alist-get 'recommendation context)))
    (message "")

    ;; Learnings
    (message "   📚 Learnings:")
    (dolist (learning (alist-get 'learnings summary))
      (message "     • %s" learning))
    (message "")

    ;; Recommendations
    (message "   💡 Recommendations for Next Session:")
    (dolist (rec (alist-get 'recommendations summary))
      (message "     • %s" rec)))

  (message "")
  (message "   ════════════════════════════════════════════════════════")
  (message ""))

(defun demo-reflect--prediction ()
  "Show what a typical session summary would predict."
  (message "7. Typical Session Summary Prediction:")
  (message "   ────────────────────────────────────────")
  (message "")
  (message "   Based on agentic workflow patterns, a typical session would show:")
  (message "")
  (message "   📊 EXPECTED OUTPUT from sage-reflect-show-summary:")
  (message "")
  (message "   ┌─────────────────────────────────────────────────────────┐")
  (message "   │ === Session Retrospective ===                          │")
  (message "   │                                                         │")
  (message "   │ Duration: 45m 23s                                       │")
  (message "   │                                                         │")
  (message "   │ ** Learnings **                                         │")
  (message "   │ - Session used 42 tool calls - good success rate        │")
  (message "   │ - Context usage high - consider more frequent           │")
  (message "   │   summarization                                         │")
  (message "   │                                                         │")
  (message "   │ ** Recommendations **                                   │")
  (message "   │ - Use /compact more frequently to manage context        │")
  (message "   │ - Consider alternatives for slow tools: code_search     │")
  (message "   │                                                         │")
  (message "   └─────────────────────────────────────────────────────────┘")
  (message "")
  (message "   This implements the REFLECTION pattern from agentic AI:")
  (message "   • Tool Use   → sage-tools.el executes commands")
  (message "   • Memory     → sage-memory.el persists facts")
  (message "   • Planning   → sage-project.el tracks work")
  (message "   • Reflection → sage-reflect.el learns from sessions")
  (message ""))

(defun run-demo-reflect ()
  "Run the full reflection demo."
  (demo-reflect--simulate-session)
  (demo-reflect--show-status)
  (demo-reflect--show-tool-analysis)
  (demo-reflect--show-summary)
  (demo-reflect--prediction)

  (message "╔══════════════════════════════════════════════════════════════╗")
  (message "║                    DEMO COMPLETE                             ║")
  (message "╠══════════════════════════════════════════════════════════════╣")
  (message "║ To use interactively:                                        ║")
  (message "║   M-x sage-reflect-show-status   - Dashboard                 ║")
  (message "║   M-x sage-reflect-show-summary  - Retrospective             ║")
  (message "║   M-x sage-reflect-save-learnings - Persist to memory        ║")
  (message "╚══════════════════════════════════════════════════════════════╝"))

;; Run in batch mode
(when noninteractive
  (run-demo-reflect))

(provide 'demo-reflect)
;;; demo-reflect.el ends here
