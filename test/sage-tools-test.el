;;; sage-tools-test.el --- Tests for sage-tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>

;;; Commentary:

;; Tests for sage-tools.el including:
;; - File tools (read, write, list, edit)
;; - Search tools (code_search, glob_files, search_preview)
;; - Org-mode tools (org_todo_list, org_add_todo, org_set_todo_state)
;; - Web tools (web_fetch, web_search)
;;
;; Uses mocking for external dependencies (network, org-mode).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load sage-tools
(require 'sage-tools)

;;; Test Fixtures

(defvar sage-tools-test--temp-dir nil
  "Temporary directory for tests.")

(defun sage-tools-test--setup ()
  "Set up test environment."
  (setq sage-tools-test--temp-dir
        (make-temp-file "sage-tools-test-" t))
  (setq sage-workspace sage-tools-test--temp-dir))

(defun sage-tools-test--teardown ()
  "Tear down test environment."
  (when (and sage-tools-test--temp-dir
             (file-exists-p sage-tools-test--temp-dir))
    (delete-directory sage-tools-test--temp-dir t))
  (setq sage-tools-test--temp-dir nil))

(defmacro sage-tools-test--with-temp-workspace (&rest body)
  "Execute BODY with a temporary workspace."
  `(unwind-protect
       (progn
         (sage-tools-test--setup)
         ,@body)
     (sage-tools-test--teardown)))

;;; Mock Helpers

(defvar sage-tools-test--url-responses nil
  "Alist of URL to response content for mocking url-retrieve.")

(defun sage-tools-test--mock-url-retrieve (url &rest _args)
  "Mock url-retrieve-synchronously returning canned response for URL."
  (let ((response (alist-get url sage-tools-test--url-responses nil nil #'string=)))
    (when response
      (let ((buf (generate-new-buffer " *mock-url*")))
        (with-current-buffer buf
          (insert "HTTP/1.1 200 OK\n")
          (insert "Content-Type: text/html\n")
          (insert "\n")
          (insert response))
        buf))))

(defmacro sage-tools-test--with-mock-url (url-responses &rest body)
  "Execute BODY with mocked URL responses.
URL-RESPONSES is an alist of (URL . RESPONSE-CONTENT)."
  `(let ((sage-tools-test--url-responses ,url-responses))
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                #'sage-tools-test--mock-url-retrieve))
       ,@body)))

;;; FILE TOOL TESTS

(ert-deftest sage-tools-test-read-file ()
  "Test read_file tool."
  (sage-tools-test--with-temp-workspace
   (let ((test-file (expand-file-name "test.txt" sage-tools-test--temp-dir)))
     (with-temp-file test-file
       (insert "Hello, World!\nLine 2"))
     (let ((result (sage--tool-read-file '((path . "test.txt")))))
       (should (string-match "Hello, World!" result))
       (should (string-match "Line 2" result))))))

(ert-deftest sage-tools-test-read-file-not-found ()
  "Test read_file with non-existent file."
  (sage-tools-test--with-temp-workspace
   (let ((result (sage--tool-read-file '((path . "nonexistent.txt")))))
     (should (string-match "not found" result)))))

(ert-deftest sage-tools-test-write-file ()
  "Test write_file tool."
  (sage-tools-test--with-temp-workspace
   (let ((result (sage--tool-write-file '((path . "new.txt")
                                          (content . "New content")))))
     (should (string-match "Wrote" result))
     (let ((file-path (expand-file-name "new.txt" sage-tools-test--temp-dir)))
       (should (file-exists-p file-path))
       (should (string= "New content"
                        (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))))))

(ert-deftest sage-tools-test-list-files ()
  "Test list_files tool."
  (sage-tools-test--with-temp-workspace
   ;; Create some test files
   (with-temp-file (expand-file-name "file1.el" sage-tools-test--temp-dir)
     (insert ""))
   (with-temp-file (expand-file-name "file2.el" sage-tools-test--temp-dir)
     (insert ""))
   (with-temp-file (expand-file-name "file3.txt" sage-tools-test--temp-dir)
     (insert ""))
   (let ((result (sage--tool-list-files '((path . ".")
                                          (pattern . "*.el")))))
     (should (string-match "file1.el" result))
     (should (string-match "file2.el" result))
     (should-not (string-match "file3.txt" result)))))

(ert-deftest sage-tools-test-edit-file ()
  "Test edit_file tool."
  (sage-tools-test--with-temp-workspace
   (let ((test-file (expand-file-name "edit-test.txt" sage-tools-test--temp-dir)))
     (with-temp-file test-file
       (insert "Hello OLD World"))
     (let ((result (sage--tool-edit-file '((path . "edit-test.txt")
                                           (old_text . "OLD")
                                           (new_text . "NEW")))))
       (should (string-match "Edited" result))
       (should (string= "Hello NEW World"
                        (with-temp-buffer
                          (insert-file-contents test-file)
                          (buffer-string))))))))

;;; SEARCH TOOL TESTS

(ert-deftest sage-tools-test-code-search ()
  "Test code_search tool with Emacs-native implementation."
  (sage-tools-test--with-temp-workspace
   ;; Create test files
   (with-temp-file (expand-file-name "search1.el" sage-tools-test--temp-dir)
     (insert "(defun my-function ()\n  \"Docstring\")\n"))
   (with-temp-file (expand-file-name "search2.el" sage-tools-test--temp-dir)
     (insert "(defvar my-variable nil)\n"))
   (let ((result (sage--tool-code-search '((pattern . "defun")
                                           (file_type . "el")))))
     (should (string-match "search1.el" result))
     (should (string-match "my-function" result))
     (should-not (string-match "search2.el" result)))))

(ert-deftest sage-tools-test-code-search-no-file-type ()
  "Test code_search without file type filter."
  (sage-tools-test--with-temp-workspace
   (with-temp-file (expand-file-name "test.py" sage-tools-test--temp-dir)
     (insert "def hello():\n    pass\n"))
   (with-temp-file (expand-file-name "test.rb" sage-tools-test--temp-dir)
     (insert "def hello\nend\n"))
   (let ((result (sage--tool-code-search '((pattern . "def")))))
     (should (string-match "test.py" result))
     (should (string-match "test.rb" result)))))

(ert-deftest sage-tools-test-code-search-no-matches ()
  "Test code_search with no matches."
  (sage-tools-test--with-temp-workspace
   (with-temp-file (expand-file-name "empty.txt" sage-tools-test--temp-dir)
     (insert "nothing here"))
   (let ((result (sage--tool-code-search '((pattern . "nonexistent_pattern_xyz")))))
     (should (string-match "No matches" result)))))

(ert-deftest sage-tools-test-glob-files ()
  "Test glob_files tool."
  (sage-tools-test--with-temp-workspace
   (with-temp-file (expand-file-name "a.el" sage-tools-test--temp-dir)
     (insert ""))
   (with-temp-file (expand-file-name "b.el" sage-tools-test--temp-dir)
     (insert ""))
   (let ((result (sage--tool-glob-files '((pattern . "*.el")))))
     (should (string-match "a.el" result))
     (should (string-match "b.el" result)))))

;;; ORG-MODE TOOL TESTS

(ert-deftest sage-tools-test-org-todo-list ()
  "Test org_todo_list tool."
  (sage-tools-test--with-temp-workspace
   (let ((org-file (expand-file-name "todos.org" sage-tools-test--temp-dir)))
     (with-temp-file org-file
       (insert "* TODO Task one\n")
       (insert "* DONE Task two\n")
       (insert "* TODO [#A] High priority task\n")
       (insert "* Regular heading\n"))
     ;; Skip if org-mode not available
     (skip-unless (require 'org nil t))
     (let ((result (sage--tool-org-todo-list '((file . "todos.org")))))
       (should (string-match "TODO" result))
       (should (string-match "Task one" result))
       (should (string-match "DONE" result))
       (should (string-match "Task two" result))
       ;; Regular heading should not appear (no TODO state)
       (should-not (string-match "Regular heading" result))))))

(ert-deftest sage-tools-test-org-todo-list-file-not-found ()
  "Test org_todo_list with non-existent file."
  (sage-tools-test--with-temp-workspace
   (let ((result (sage--tool-org-todo-list '((file . "nonexistent.org")))))
     (should (string-match "not found\\|not .org" result)))))

(ert-deftest sage-tools-test-org-add-todo ()
  "Test org_add_todo tool."
  (sage-tools-test--with-temp-workspace
   (let ((org-file (expand-file-name "new-todos.org" sage-tools-test--temp-dir)))
     ;; Create empty org file
     (with-temp-file org-file
       (insert ""))
     (skip-unless (require 'org nil t))
     (let ((result (sage--tool-org-add-todo '((file . "new-todos.org")
                                              (heading . "New task")
                                              (state . "TODO")
                                              (priority . "A")))))
       (should (string-match "Added TODO" result))
       ;; Verify file contents
       (let ((contents (with-temp-buffer
                         (insert-file-contents org-file)
                         (buffer-string))))
         (should (string-match "\\* TODO" contents))
         (should (string-match "New task" contents)))))))

(ert-deftest sage-tools-test-org-add-todo-unsafe-path ()
  "Test org_add_todo rejects unsafe paths."
  (sage-tools-test--with-temp-workspace
   (let ((result (sage--tool-org-add-todo '((file . "../../../etc/passwd")
                                            (heading . "Evil task")))))
     (should (string-match "Unsafe path" result)))))

;;; WEB TOOL TESTS

(ert-deftest sage-tools-test-web-fetch ()
  "Test web_fetch tool with mocked URL."
  (sage-tools-test--with-mock-url
   '(("https://example.com" . "<html><body><h1>Example</h1><p>Hello world</p></body></html>"))
   (let ((result (sage--tool-web-fetch '((url . "https://example.com")))))
     (should (string-match "Example" result))
     (should (string-match "Hello world" result))
     ;; HTML tags should be stripped
     (should-not (string-match "<h1>" result))
     (should-not (string-match "<p>" result)))))

(ert-deftest sage-tools-test-web-fetch-invalid-url ()
  "Test web_fetch rejects invalid URLs."
  (let ((result (sage--tool-web-fetch '((url . "not-a-url")))))
    (should (string-match "Invalid URL" result)))
  (let ((result (sage--tool-web-fetch '((url . "ftp://example.com")))))
    (should (string-match "Invalid URL" result))))

(ert-deftest sage-tools-test-web-fetch-html-entities ()
  "Test web_fetch decodes HTML entities."
  (sage-tools-test--with-mock-url
   '(("https://test.com" . "<p>Tom &amp; Jerry &lt;3 &gt;</p>"))
   (let ((result (sage--tool-web-fetch '((url . "https://test.com")))))
     (should (string-match "Tom & Jerry" result))
     (should (string-match "<3 >" result)))))

(ert-deftest sage-tools-test-web-search ()
  "Test web_search tool with mocked DuckDuckGo response."
  (sage-tools-test--with-mock-url
   '(("https://html.duckduckgo.com/html/?q=emacs%20lisp" .
      "<div class=\"result\"><a class=\"result__a\" href=\"https://gnu.org/emacs\">GNU Emacs</a></div>
       <div class=\"result\"><a class=\"result__a\" href=\"https://example.com/elisp\">Elisp Guide</a></div>"))
   (let ((result (sage--tool-web-search '((query . "emacs lisp")
                                          (max_results . 5)))))
     (should (string-match "GNU Emacs" result))
     (should (string-match "gnu.org" result)))))

(ert-deftest sage-tools-test-web-search-no-query ()
  "Test web_search with no query."
  (let ((result (sage--tool-web-search '((query . nil)))))
    (should (string-match "No search query" result))))

;;; SAFE TOOLS LIST TESTS

(ert-deftest sage-tools-test-safe-tools-includes-new ()
  "Test that new tools are in safe tools list."
  (should (member "org_todo_list" sage-safe-tools))
  (should (member "web_fetch" sage-safe-tools))
  (should (member "web_search" sage-safe-tools))
  ;; Write tools should NOT be in safe list
  (should-not (member "org_add_todo" sage-safe-tools))
  (should-not (member "org_set_todo_state" sage-safe-tools)))

;;; TOOL INITIALIZATION TESTS

(ert-deftest sage-tools-test-tools-registered-on-require ()
  "Test that tools are registered automatically when module is loaded.
This is the critical test for batch mode - ensures (require 'sage-tools)
populates sage-tools without needing to call sage--init-default-tools.

Bug: gemini-repl-010-ac4 - Tools not registered in batch mode."
  ;; Note: This test runs after (require 'sage-tools) at the top of this file.
  ;; If tools are properly registered on load, sage-tools will be populated.
  ;; If not, sage-tools will be nil or void.

  ;; First verify the variable exists and is bound
  (should (boundp 'sage-tools))

  ;; Critical assertion: sage-tools should have tools registered
  ;; This fails if sage--init-default-tools is not called during load
  (should-not (null sage-tools))

  ;; Verify it's a proper list with content
  (should (listp sage-tools))
  (should (> (length sage-tools) 0))

  ;; Verify at least one tool is present with correct structure
  (let ((read-file-tool (cl-find "read_file" sage-tools
                                  :key (lambda (tool) (alist-get 'name tool))
                                  :test #'string=)))
    (should read-file-tool)
    (should (alist-get 'description read-file-tool))
    (should (alist-get 'execute read-file-tool))))

(ert-deftest sage-tools-test-init-registers-all-tools ()
  "Test that initialization registers all expected tools."
  ;; Ensure sage-tools is defined as a dynamic variable
  (defvar sage-tools nil)
  (setq sage-tools nil)
  ;; Make sure sage-register-tool is not bound so fallback is used
  (cl-letf (((symbol-function 'sage-register-tool) nil))
    (sage--init-default-tools)
    ;; Check for key tools
    (should (cl-find "read_file" sage-tools
                     :key (lambda (t) (alist-get 'name t))
                     :test #'string=))
    (should (cl-find "code_search" sage-tools
                     :key (lambda (t) (alist-get 'name t))
                     :test #'string=))
    (should (cl-find "org_todo_list" sage-tools
                     :key (lambda (t) (alist-get 'name t))
                     :test #'string=))
    (should (cl-find "web_fetch" sage-tools
                     :key (lambda (t) (alist-get 'name t))
                     :test #'string=))
    (should (cl-find "web_search" sage-tools
                     :key (lambda (t) (alist-get 'name t))
                     :test #'string=))
    ;; Should have 26+ tools
    (should (>= (length sage-tools) 26))))

;;; EDGE CASE TESTS

(ert-deftest sage-tools-test-code-search-binary-skip ()
  "Test that code_search skips binary files."
  (sage-tools-test--with-temp-workspace
   ;; Create a "binary" file (by extension)
   (with-temp-file (expand-file-name "image.png" sage-tools-test--temp-dir)
     (insert "defun in binary"))
   (with-temp-file (expand-file-name "code.el" sage-tools-test--temp-dir)
     (insert "(defun real-code ())"))
   (let ((result (sage--tool-code-search '((pattern . "defun")))))
     ;; Should find in .el but not in .png
     (should (string-match "code.el" result))
     (should-not (string-match "image.png" result)))))

(ert-deftest sage-tools-test-web-fetch-strips-scripts ()
  "Test that web_fetch removes script tags."
  (sage-tools-test--with-mock-url
   '(("https://test.com" . "<html><script>alert('xss')</script><p>Safe content</p></html>"))
   (let ((result (sage--tool-web-fetch '((url . "https://test.com")))))
     (should (string-match "Safe content" result))
     ;; Script content should be removed
     (should-not (string-match "alert" result)))))

(provide 'sage-tools-test)
;;; sage-tools-test.el ends here
