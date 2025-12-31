;;; gptel-emacs-tools.el --- Emacs introspection tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Tools to allow LLMs to inspect Emacs state, buffers, and projects
;; Inspired by claude-code-ide.el

;;; Commentary:

;; This package provides a set of tools for gptel that allow LLMs to:
;; - List and read open buffers
;; - Get information about the current Emacs state
;; - Search and navigate codebases
;; - Inspect project structure
;; - Read and modify files
;; - Execute elisp code (with confirmation)
;; - Compact/summarize conversation sessions (similar to OpenCode's /compact)
;;
;; Session Compaction:
;; When conversations get long, use `gptel-emacs-tools-compact-session' to
;; summarize the conversation and reduce context size.  The LLM can also
;; invoke the `compact_session' tool to do this automatically.
;;
;; Auto-compaction can be enabled with `gptel-emacs-tools-enable-auto-compact'
;; which will trigger compaction when token usage approaches the model's
;; context limit (configurable via `gptel-emacs-tools-auto-compact-threshold').

;;; Code:

(require 'gptel)
(require 'project)

;;; Customization

(defgroup gptel-emacs-tools nil
  "Tools for gptel to inspect and manipulate Emacs state."
  :group 'gptel)

(defcustom gptel-emacs-tools-compact-prompt
  "Provide a detailed summary for continuing our conversation. Focus on:
- What we discussed and accomplished
- What files or code we're working on
- Current state and context
- What we're planning to do next

The new session will not have access to our conversation history, so include all relevant context."
  "Prompt used when compacting a gptel session."
  :type 'string
  :group 'gptel-emacs-tools)

(defcustom gptel-emacs-tools-compact-model 'claude-sonnet-4-20250514
  "Model to use for session compaction.
Use a smaller/cheaper model for cost efficiency."
  :type 'symbol
  :group 'gptel-emacs-tools)

(defcustom gptel-emacs-tools-auto-compact-threshold 0.8
  "Trigger auto-compaction when token usage exceeds this fraction of context limit.
Set to nil to disable auto-compaction."
  :type '(choice (const :tag "Disabled" nil)
                 (float :tag "Threshold (0.0-1.0)"))
  :group 'gptel-emacs-tools)

;;; Buffer Tools

(defvar gptel-emacs-tools--list-buffers
  (gptel-make-tool
   :name "list_buffers"
   :function (lambda ()
               (let ((buffers
                      (cl-loop for buf in (buffer-list)
                               for name = (buffer-name buf)
                               for file = (buffer-file-name buf)
                               for mode = (buffer-local-value 'major-mode buf)
                               for modified = (buffer-modified-p buf)
                               for size = (buffer-size buf)
                               unless (string-prefix-p " " name)
                               collect (list :name name
                                             :file (or file "")
                                             :mode (symbol-name mode)
                                             :modified modified
                                             :size size))))
                 (json-encode buffers)))
   :description "List all open buffers in Emacs with their file paths, major modes, and modification status"
   :args nil
   :category "emacs")
  "Tool to list all open buffers.")

(defvar gptel-emacs-tools--read-buffer
  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer-name &optional start-line end-line)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (let* ((lines (split-string (buffer-string) "\n"))
                          (total-lines (length lines))
                          (start (max 0 (1- (or start-line 1))))
                          (end (min total-lines (or end-line total-lines)))
                          (selected-lines (cl-subseq lines start end))
                          (numbered-lines
                           (cl-loop for line in selected-lines
                                    for num from (1+ start)
                                    collect (format "%5d: %s" num line))))
                     (format "Buffer: %s\nFile: %s\nMode: %s\nTotal lines: %d\nShowing lines %d-%d:\n\n%s"
                             buffer-name
                             (or (buffer-file-name) "N/A")
                             major-mode
                             total-lines
                             (1+ start)
                             end
                             (string-join numbered-lines "\n"))))))
   :description "Read the contents of an Emacs buffer, optionally specifying a line range"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "The name of the buffer to read")
               '(:name "start_line"
                 :type integer
                 :optional t
                 :description "Starting line number (1-indexed, default: 1)")
               '(:name "end_line"
                 :type integer
                 :optional t
                 :description "Ending line number (inclusive, default: end of buffer)"))
   :category "emacs")
  "Tool to read buffer contents.")

(defvar gptel-emacs-tools--get-buffer-info
  (gptel-make-tool
   :name "get_buffer_info"
   :function (lambda (buffer-name)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (json-encode
                    (list :name buffer-name
                          :file (buffer-file-name)
                          :mode (symbol-name major-mode)
                          :modified (buffer-modified-p)
                          :read-only buffer-read-only
                          :size (buffer-size)
                          :line-count (count-lines (point-min) (point-max))
                          :point (point)
                          :point-min (point-min)
                          :point-max (point-max)
                          :narrowed (buffer-narrowed-p)
                          :encoding (symbol-name buffer-file-coding-system))))))
   :description "Get detailed information about a specific buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "The name of the buffer to inspect"))
   :category "emacs")
  "Tool to get buffer information.")

(defvar gptel-emacs-tools--get-current-buffer
  (gptel-make-tool
   :name "get_current_buffer"
   :function (lambda ()
               (let ((buf (if (minibufferp)
                              (window-buffer (minibuffer-selected-window))
                            (current-buffer))))
                 (with-current-buffer buf
                   (json-encode
                    (list :name (buffer-name)
                          :file (buffer-file-name)
                          :mode (symbol-name major-mode)
                          :point (point)
                          :line (line-number-at-pos)
                          :column (current-column)
                          :selection (when (use-region-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end))))))))
   :description "Get information about the current buffer including cursor position and any active selection"
   :args nil
   :category "emacs")
  "Tool to get current buffer info.")

;;; File Tools

(defvar gptel-emacs-tools--read-file
  (gptel-make-tool
   :name "read_file"
   :function (lambda (file-path &optional start-line end-line)
               (let ((path (expand-file-name file-path)))
                 (unless (file-exists-p path)
                   (error "File '%s' does not exist" path))
                 (unless (file-readable-p path)
                   (error "File '%s' is not readable" path))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (let* ((lines (split-string (buffer-string) "\n"))
                          (total-lines (length lines))
                          (start (max 0 (1- (or start-line 1))))
                          (end (min total-lines (or end-line total-lines)))
                          (selected-lines (cl-subseq lines start end))
                          (numbered-lines
                           (cl-loop for line in selected-lines
                                    for num from (1+ start)
                                    collect (format "%5d: %s" num line))))
                     (format "File: %s\nTotal lines: %d\nShowing lines %d-%d:\n\n%s"
                             path
                             total-lines
                             (1+ start)
                             end
                             (string-join numbered-lines "\n"))))))
   :description "Read contents of a file from disk, optionally specifying a line range"
   :args (list '(:name "file_path"
                 :type string
                 :description "Path to the file to read")
               '(:name "start_line"
                 :type integer
                 :optional t
                 :description "Starting line number (1-indexed, default: 1)")
               '(:name "end_line"
                 :type integer
                 :optional t
                 :description "Ending line number (inclusive, default: end of file)"))
   :category "filesystem")
  "Tool to read file contents.")

(defvar gptel-emacs-tools--write-file
  (gptel-make-tool
   :name "write_file"
   :function (lambda (file-path content)
               (let ((path (expand-file-name file-path)))
                 (when (and (file-exists-p path)
                            (not (yes-or-no-p (format "File '%s' exists. Overwrite? " path))))
                   (error "Operation cancelled by user"))
                 (with-temp-buffer
                   (insert content)
                   (write-file path))
                 (format "Successfully wrote %d bytes to %s" (length content) path)))
   :description "Write content to a file (will prompt for confirmation if file exists)"
   :args (list '(:name "file_path"
                 :type string
                 :description "Path to the file to write")
               '(:name "content"
                 :type string
                 :description "Content to write to the file"))
   :category "filesystem"
   :confirm t)
  "Tool to write file contents.")

(defvar gptel-emacs-tools--list-directory
  (gptel-make-tool
   :name "list_directory"
   :function (lambda (directory-path &optional recursive)
               (let ((path (expand-file-name directory-path)))
                 (unless (file-directory-p path)
                   (error "'%s' is not a directory" path))
                 (let ((files (if recursive
                                  (directory-files-recursively path ".*" nil t)
                                (directory-files path t nil t))))
                   (json-encode
                    (cl-loop for file in files
                             for name = (file-name-nondirectory file)
                             unless (member name '("." ".."))
                             collect (list :name name
                                           :path file
                                           :directory (file-directory-p file)
                                           :size (if (file-directory-p file)
                                                     0
                                                   (file-attribute-size (file-attributes file)))))))))
   :description "List files in a directory"
   :args (list '(:name "directory_path"
                 :type string
                 :description "Path to the directory")
               '(:name "recursive"
                 :type boolean
                 :optional t
                 :description "If true, list files recursively"))
   :category "filesystem")
  "Tool to list directory contents.")

;;; Search Tools

(defvar gptel-emacs-tools--search-buffer
  (gptel-make-tool
   :name "search_buffer"
   :function (lambda (buffer-name pattern &optional regexp)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (let ((matches nil)
                           (search-fn (if regexp #'re-search-forward #'search-forward))
                           (case-fold-search t))
                       (while (funcall search-fn pattern nil t)
                         (let ((match-start (match-beginning 0)))
                           (push (list :line (line-number-at-pos match-start)
                                       :column (save-excursion
                                                 (goto-char match-start)
                                                 (current-column))
                                       :context (buffer-substring-no-properties
                                                 (line-beginning-position)
                                                 (line-end-position)))
                                 matches)))
                       (json-encode (nreverse matches)))))))
   :description "Search for a pattern in a buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to search")
               '(:name "pattern"
                 :type string
                 :description "Pattern to search for")
               '(:name "regexp"
                 :type boolean
                 :optional t
                 :description "If true, treat pattern as a regular expression"))
   :category "search")
  "Tool to search within a buffer.")

(defvar gptel-emacs-tools--grep-project
  (gptel-make-tool
   :name "grep_project"
   :function (lambda (pattern &optional file-pattern)
               (let* ((default-directory (or (when-let ((proj (project-current)))
                                               (project-root proj))
                                             default-directory))
                      (file-glob (or file-pattern "*"))
                      (output (with-temp-buffer
                                (call-process "rg" nil t nil
                                              "--json" "--max-count" "100"
                                              pattern "-g" file-glob ".")
                                (buffer-string)))
                      (lines (split-string output "\n" t))
                      (results nil))
                 (dolist (line lines)
                   (condition-case nil
                       (let* ((json (json-parse-string line :object-type 'alist))
                              (type (alist-get 'type json)))
                         (when (equal type "match")
                           (let* ((data (alist-get 'data json))
                                  (path (alist-get 'path data))
                                  (path-text (alist-get 'text path))
                                  (line-num (alist-get 'line_number data))
                                  (lines-data (alist-get 'lines data))
                                  (text (string-trim (alist-get 'text lines-data))))
                             (push (list :file path-text
                                         :line line-num
                                         :text text)
                                   results))))
                     (error nil)))
                 (json-encode (nreverse results))))
   :description "Search for a pattern across project files using ripgrep"
   :args (list '(:name "pattern"
                 :type string
                 :description "Pattern to search for (ripgrep syntax)")
               '(:name "file_pattern"
                 :type string
                 :optional t
                 :description "Glob pattern to filter files (e.g., '*.el', '*.py')"))
   :category "search")
  "Tool to grep across project files.")

(defvar gptel-emacs-tools--find-files
  (gptel-make-tool
   :name "find_files"
   :function (lambda (pattern &optional directory)
               (let* ((dir (expand-file-name (or directory
                                                  (when-let ((proj (project-current)))
                                                    (project-root proj))
                                                  default-directory)))
                      (output (with-temp-buffer
                                (call-process "fd" nil t nil
                                              "--type" "f" pattern dir)
                                (buffer-string)))
                      (files (split-string output "\n" t)))
                 (json-encode files)))
   :description "Find files matching a pattern using fd"
   :args (list '(:name "pattern"
                 :type string
                 :description "Pattern to match file names (fd syntax)")
               '(:name "directory"
                 :type string
                 :optional t
                 :description "Directory to search in (default: project root or current directory)"))
   :category "search")
  "Tool to find files by name pattern.")

;;; Project Tools

(defvar gptel-emacs-tools--get-project-info
  (gptel-make-tool
   :name "get_project_info"
   :function (lambda ()
               (let ((proj (project-current)))
                 (if proj
                     (let* ((root (project-root proj))
                            (files (project-files proj))
                            (file-count (length files))
                            (extensions (make-hash-table :test 'equal)))
                       ;; Count file extensions
                       (dolist (file files)
                         (let ((ext (file-name-extension file)))
                           (when ext
                             (puthash ext (1+ (gethash ext extensions 0)) extensions))))
                       (json-encode
                        (list :root root
                              :name (file-name-nondirectory (directory-file-name root))
                              :file-count file-count
                              :extensions (let ((exts nil))
                                            (maphash (lambda (k v)
                                                       (push (cons k v) exts))
                                                     extensions)
                                            (sort exts (lambda (a b) (> (cdr a) (cdr b))))))))
                   (json-encode (list :error "No project found")))))
   :description "Get information about the current project"
   :args nil
   :category "project")
  "Tool to get project information.")

(defvar gptel-emacs-tools--list-project-files
  (gptel-make-tool
   :name "list_project_files"
   :function (lambda (&optional pattern)
               (let ((proj (project-current)))
                 (unless proj
                   (error "No project found"))
                 (let* ((files (project-files proj))
                        (filtered (if pattern
                                      (cl-remove-if-not
                                       (lambda (f) (string-match-p pattern f))
                                       files)
                                    files)))
                   (json-encode (cl-subseq filtered 0 (min 500 (length filtered)))))))
   :description "List files in the current project, optionally filtered by pattern"
   :args (list '(:name "pattern"
                 :type string
                 :optional t
                 :description "Regexp pattern to filter files"))
   :category "project")
  "Tool to list project files.")

;;; Emacs State Tools

(defvar gptel-emacs-tools--get-emacs-state
  (gptel-make-tool
   :name "get_emacs_state"
   :function (lambda ()
               (json-encode
                (list :emacs-version emacs-version
                      :system-type (symbol-name system-type)
                      :window-system (symbol-name (or window-system 'tty))
                      :frame-count (length (frame-list))
                      :window-count (length (window-list))
                      :buffer-count (length (buffer-list))
                      :current-directory default-directory
                      :load-path-count (length load-path))))
   :description "Get general information about the current Emacs state"
   :args nil
   :category "emacs")
  "Tool to get Emacs state.")

(defvar gptel-emacs-tools--get-window-layout
  (gptel-make-tool
   :name "get_window_layout"
   :function (lambda ()
               (let ((windows
                      (cl-loop for win in (window-list)
                               collect (list :buffer (buffer-name (window-buffer win))
                                             :width (window-width win)
                                             :height (window-height win)
                                             :selected (eq win (selected-window))
                                             :dedicated (window-dedicated-p win)))))
                 (json-encode windows)))
   :description "Get information about the current window layout"
   :args nil
   :category "emacs")
  "Tool to get window layout.")

;;; Edit Tools

(defvar gptel-emacs-tools--edit-buffer
  (gptel-make-tool
   :name "edit_buffer"
   :function (lambda (buffer-name old-text new-text)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (if (search-forward old-text nil t)
                         (progn
                           (replace-match new-text t t)
                           (format "Successfully replaced text in buffer '%s'" buffer-name))
                       (error "Could not find the specified text in buffer '%s'" buffer-name))))))
   :description "Replace text in a buffer (finds first occurrence of old_text and replaces with new_text)"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to edit")
               '(:name "old_text"
                 :type string
                 :description "Text to find and replace")
               '(:name "new_text"
                 :type string
                 :description "Replacement text"))
   :category "edit"
   :confirm t)
  "Tool to edit buffer contents.")

(defvar gptel-emacs-tools--insert-at-point
  (gptel-make-tool
   :name "insert_at_point"
   :function (lambda (buffer-name line column text)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (forward-char (min column (- (line-end-position) (line-beginning-position))))
                     (insert text)
                     (format "Inserted %d characters at line %d, column %d in buffer '%s'"
                             (length text) line column buffer-name)))))
   :description "Insert text at a specific position in a buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer")
               '(:name "line"
                 :type integer
                 :description "Line number (1-indexed)")
               '(:name "column"
                 :type integer
                 :description "Column number (0-indexed)")
               '(:name "text"
                 :type string
                 :description "Text to insert"))
   :category "edit"
   :confirm t)
  "Tool to insert text at a position.")

;;; Elisp Evaluation Tool

(defvar gptel-emacs-tools--eval-elisp
  (gptel-make-tool
   :name "eval_elisp"
   :function (lambda (code)
               (condition-case err
                   (let ((result (eval (read code))))
                     (format "Result: %S" result))
                 (error (format "Error: %s" (error-message-string err)))))
   :description "Evaluate Emacs Lisp code and return the result (use with caution)"
   :args (list '(:name "code"
                 :type string
                 :description "Emacs Lisp code to evaluate"))
   :category "emacs"
   :confirm t)
  "Tool to evaluate elisp code.")

;;; Symbol Lookup Tools

(defvar gptel-emacs-tools--describe-function
  (gptel-make-tool
   :name "describe_function"
   :function (lambda (function-name)
               (let ((sym (intern-soft function-name)))
                 (if (and sym (fboundp sym))
                     (format "Function: %s\n\nDocumentation:\n%s\n\nArguments: %s"
                             function-name
                             (or (documentation sym) "No documentation available")
                             (or (help-function-arglist sym t) "Unknown"))
                   (error "Function '%s' not found" function-name))))
   :description "Get documentation for an Emacs Lisp function"
   :args (list '(:name "function_name"
                 :type string
                 :description "Name of the function to describe"))
   :category "emacs")
  "Tool to describe a function.")

(defvar gptel-emacs-tools--describe-variable
  (gptel-make-tool
   :name "describe_variable"
   :function (lambda (variable-name)
               (let ((sym (intern-soft variable-name)))
                 (if (and sym (boundp sym))
                     (format "Variable: %s\n\nValue: %S\n\nDocumentation:\n%s"
                             variable-name
                             (symbol-value sym)
                             (or (documentation-property sym 'variable-documentation)
                                 "No documentation available"))
                   (error "Variable '%s' not found" variable-name))))
   :description "Get documentation and value for an Emacs Lisp variable"
   :args (list '(:name "variable_name"
                 :type string
                 :description "Name of the variable to describe"))
   :category "emacs")
  "Tool to describe a variable.")

;;; Session Compaction Functions

(defun gptel-emacs-tools--estimate-tokens (text)
  "Estimate token count for TEXT using ~4 chars per token heuristic."
  (/ (length text) 4))

(defun gptel-emacs-tools--get-context-limit ()
  "Get the context limit for the current model.
Returns the model's context window size, or 100000 as fallback."
  (or (and (boundp 'gptel-model)
           (boundp 'gptel-backend)
           gptel-backend
           (let* ((models (cl-struct-slot-value
                           (type-of gptel-backend) 'models gptel-backend))
                  (model-plist (alist-get gptel-model models)))
             (when (listp model-plist)
               (plist-get model-plist :context-window))))
      100000))

(defun gptel-emacs-tools--compact-replace-buffer (summary)
  "Replace buffer contents with SUMMARY, preserving gptel state."
  (let ((inhibit-read-only t)
        (gptel-mode-was-on (bound-and-true-p gptel-mode)))
    (erase-buffer)
    (insert "[Session Summary]\n\n" summary "\n\n")
    (when gptel-mode-was-on
      (gptel-mode 1))
    (goto-char (point-max))))

(defun gptel-emacs-tools--compact-callback (response info)
  "Handle compaction response, replacing buffer with summary.
RESPONSE is the LLM's summary text, INFO contains request metadata."
  (if (stringp response)
      (with-current-buffer (plist-get info :buffer)
        (gptel-emacs-tools--compact-replace-buffer response)
        (message "Session compacted successfully."))
    (message "Session compaction failed: %s" (plist-get info :status))))

;;;###autoload
(defun gptel-emacs-tools-compact-session (&optional no-confirm)
  "Compact the current gptel session by summarizing the conversation.
With NO-CONFIRM or when called non-interactively, skip confirmation.
Uses `gptel-emacs-tools-compact-model' for summarization."
  (interactive)
  (unless (bound-and-true-p gptel-mode)
    (user-error "Not in a gptel buffer"))
  (when (or no-confirm
            (not (called-interactively-p 'any))
            (yes-or-no-p "Compact session? This will replace the conversation with a summary. "))
    (let* ((conversation (buffer-substring-no-properties (point-min) (point-max)))
           (prompt (concat conversation "\n\n---\n\n" gptel-emacs-tools-compact-prompt))
           (gptel-model gptel-emacs-tools-compact-model))
      (message "Compacting session...")
      (gptel-request prompt
                     :callback #'gptel-emacs-tools--compact-callback
                     :buffer (current-buffer)))))

(defun gptel-emacs-tools--maybe-auto-compact (&optional _beg _end)
  "Check if auto-compaction should trigger based on token threshold.
Intended for use in `gptel-post-response-functions'."
  (when (and gptel-emacs-tools-auto-compact-threshold
             (bound-and-true-p gptel-mode))
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (tokens (gptel-emacs-tools--estimate-tokens text))
           (limit (gptel-emacs-tools--get-context-limit))
           (threshold (* limit gptel-emacs-tools-auto-compact-threshold)))
      (when (> tokens threshold)
        (message "Auto-compacting session (estimated %d tokens, threshold %d)..."
                 tokens (floor threshold))
        (gptel-emacs-tools-compact-session t)))))

;;;###autoload
(defun gptel-emacs-tools-enable-auto-compact ()
  "Enable automatic session compaction when approaching context limits."
  (interactive)
  (add-hook 'gptel-post-response-functions #'gptel-emacs-tools--maybe-auto-compact)
  (message "Auto-compaction enabled (threshold: %.0f%%)"
           (* 100 gptel-emacs-tools-auto-compact-threshold)))

;;;###autoload
(defun gptel-emacs-tools-disable-auto-compact ()
  "Disable automatic session compaction."
  (interactive)
  (remove-hook 'gptel-post-response-functions #'gptel-emacs-tools--maybe-auto-compact)
  (message "Auto-compaction disabled"))

;;; Shell Command Tool

(defvar gptel-emacs-tools--shell-command
  (gptel-make-tool
   :name "shell_command"
   :function (lambda (command &optional directory)
               (let ((default-directory (or directory default-directory)))
                 (shell-command-to-string command)))
   :description "Execute a shell command and return its output"
   :args (list '(:name "command"
                 :type string
                 :description "Shell command to execute")
               '(:name "directory"
                 :type string
                 :optional t
                 :description "Working directory for the command"))
   :category "shell"
   :confirm t)
  "Tool to run shell commands.")

;;; Session Compaction Tool

(defvar gptel-emacs-tools--compact-session
  (gptel-make-tool
   :name "compact_session"
   :function (lambda ()
               (if (bound-and-true-p gptel-mode)
                   (progn
                     (gptel-emacs-tools-compact-session t)
                     "Session compaction initiated. The conversation will be summarized and replaced with a detailed context summary.")
                 (error "Not in a gptel buffer")))
   :description "Compact/summarize the current conversation session to reduce context size. Use this when the conversation is getting long and you want to preserve important context while reducing token usage. The conversation will be replaced with a detailed summary that preserves the key context."
   :args nil
   :category "session")
  "Tool to compact the current gptel session.")

;;; Collection of all tools

(defvar gptel-emacs-tools-all
  (list gptel-emacs-tools--list-buffers
        gptel-emacs-tools--read-buffer
        gptel-emacs-tools--get-buffer-info
        gptel-emacs-tools--get-current-buffer
        gptel-emacs-tools--read-file
        gptel-emacs-tools--write-file
        gptel-emacs-tools--list-directory
        gptel-emacs-tools--search-buffer
        gptel-emacs-tools--grep-project
        gptel-emacs-tools--find-files
        gptel-emacs-tools--get-project-info
        gptel-emacs-tools--list-project-files
        gptel-emacs-tools--get-emacs-state
        gptel-emacs-tools--get-window-layout
        gptel-emacs-tools--edit-buffer
        gptel-emacs-tools--insert-at-point
        gptel-emacs-tools--eval-elisp
        gptel-emacs-tools--describe-function
        gptel-emacs-tools--describe-variable
        gptel-emacs-tools--shell-command
        gptel-emacs-tools--compact-session)
  "List of all gptel Emacs tools.")

;;;###autoload
(defun gptel-emacs-tools-enable ()
  "Enable all Emacs introspection tools for gptel."
  (interactive)
  (setq gptel-tools (append gptel-tools gptel-emacs-tools-all))
  (message "Enabled %d gptel Emacs tools" (length gptel-emacs-tools-all)))

;;;###autoload
(defun gptel-emacs-tools-disable ()
  "Disable all Emacs introspection tools for gptel."
  (interactive)
  (setq gptel-tools (cl-set-difference gptel-tools gptel-emacs-tools-all))
  (message "Disabled gptel Emacs tools"))

(provide 'gptel-emacs-tools)
;;; gptel-emacs-tools.el ends here
