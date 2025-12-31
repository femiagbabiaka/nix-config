;;; gptel-git-tools.el --- Git tools for gptel via magit -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Git integration tools for gptel using magit

;;; Commentary:

;; This package provides Git tools for gptel that allow LLMs to:
;; - View repository status, diffs, logs, and blame
;; - Stage files and create commits
;; - Checkout branches and files

;;; Code:

(require 'gptel)

;; Magit is autoloaded, so we don't require it at top level
(declare-function magit-git-string "magit-git")
(declare-function magit-git-lines "magit-git")
(declare-function magit-git-wash "magit-git")
(declare-function magit-git-insert "magit-git")
(declare-function magit-toplevel "magit-git")
(declare-function magit-stage-file "magit-apply")
(declare-function magit-run-git "magit-process")
(declare-function magit-commit-create "magit-commit")

;;; Helper Functions

(defun gptel-git-tools--ensure-repo ()
  "Ensure we're in a git repository. Signal error if not."
  (unless (magit-toplevel)
    (error "Not in a git repository")))

(defun gptel-git-tools--run-git (&rest args)
  "Run git with ARGS and return output as string."
  (gptel-git-tools--ensure-repo)
  (with-temp-buffer
    (apply #'process-file "git" nil t nil args)
    (string-trim (buffer-string))))

;;; Read-Only Tools

(defvar gptel-git-tools--status
  (gptel-make-tool
   :name "git_status"
   :function (lambda ()
               (gptel-git-tools--ensure-repo)
               (let* ((staged (gptel-git-tools--run-git "diff" "--cached" "--name-status"))
                      (unstaged (gptel-git-tools--run-git "diff" "--name-status"))
                      (untracked (gptel-git-tools--run-git "ls-files" "--others" "--exclude-standard"))
                      (branch (gptel-git-tools--run-git "branch" "--show-current"))
                      (ahead-behind (gptel-git-tools--run-git "rev-list" "--left-right" "--count" "HEAD...@{upstream}"))
                      (clean (and (string-empty-p staged)
                                  (string-empty-p unstaged)
                                  (string-empty-p untracked))))
                 (format "Branch: %s\nUpstream: %s\nClean: %s\n\n=== Staged ===\n%s\n\n=== Unstaged ===\n%s\n\n=== Untracked ===\n%s"
                         branch
                         (if (string-empty-p ahead-behind) "not set" ahead-behind)
                         (if clean "yes" "no")
                         (if (string-empty-p staged) "(none)" staged)
                         (if (string-empty-p unstaged) "(none)" unstaged)
                         (if (string-empty-p untracked) "(none)" untracked))))
   :description "Get the current git repository status including staged, unstaged, and untracked files"
   :args nil
   :category "git")
  "Tool to get git status.")

(defvar gptel-git-tools--diff
  (gptel-make-tool
   :name "git_diff"
   :function (lambda (&optional target file)
               (gptel-git-tools--ensure-repo)
               (let ((args (cond
                            ((equal target "staged") '("diff" "--cached"))
                            ((equal target "unstaged") '("diff"))
                            ((and target (not (equal target "")))
                             (list "diff" target))
                            (t '("diff")))))
                 (when (and file (not (string-empty-p file)))
                   (setq args (append args (list "--" file))))
                 (let ((output (apply #'gptel-git-tools--run-git args)))
                   (if (string-empty-p output)
                       "No differences found."
                     output))))
   :description "Show git diff. Target can be 'staged', 'unstaged', a commit/branch, or empty for unstaged changes. Optionally filter by file path."
   :args (list '(:name "target"
                 :type string
                 :optional t
                 :description "What to diff: 'staged', 'unstaged', a commit SHA, branch name, or commit range (e.g., 'main..HEAD')")
               '(:name "file"
                 :type string
                 :optional t
                 :description "Optional file path to filter diff"))
   :category "git")
  "Tool to show git diff.")

(defvar gptel-git-tools--log
  (gptel-make-tool
   :name "git_log"
   :function (lambda (&optional count file format-string)
               (gptel-git-tools--ensure-repo)
               (let* ((n (or count 10))
                      (fmt (or format-string "%h %s (%an, %ar)"))
                      (args (list "log" (format "-n%d" n) (format "--format=%s" fmt))))
                 (when (and file (not (string-empty-p file)))
                   (setq args (append args (list "--" file))))
                 (gptel-git-tools--run-git args)))
   :description "Show git commit log"
   :args (list '(:name "count"
                 :type integer
                 :optional t
                 :description "Number of commits to show (default: 10)")
               '(:name "file"
                 :type string
                 :optional t
                 :description "Optional file path to filter history")
               '(:name "format_string"
                 :type string
                 :optional t
                 :description "Git log format string (default: '%h %s (%an, %ar)')"))
   :category "git")
  "Tool to show git log.")

(defvar gptel-git-tools--blame
  (gptel-make-tool
   :name "git_blame"
   :function (lambda (file &optional start-line end-line)
               (gptel-git-tools--ensure-repo)
               (let ((args (list "blame" "--line-porcelain")))
                 (when (and start-line end-line)
                   (setq args (append args (list (format "-L%d,%d" start-line end-line)))))
                 (setq args (append args (list file)))
                 (let* ((output (apply #'gptel-git-tools--run-git args))
                        (lines (split-string output "\n"))
                        (results nil)
                        (current-commit nil)
                        (current-author nil)
                        (current-time nil)
                        (current-line-num nil))
                   ;; Parse porcelain format
                   (dolist (line lines)
                     (cond
                      ((string-match "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" line)
                       (setq current-commit (substring (match-string 1 line) 0 8))
                       (setq current-line-num (match-string 3 line)))
                      ((string-match "^author \\(.*\\)" line)
                       (setq current-author (match-string 1 line)))
                      ((string-match "^author-time \\([0-9]+\\)" line)
                       (setq current-time (format-time-string "%Y-%m-%d" (seconds-to-time (string-to-number (match-string 1 line))))))
                      ((string-match "^\t\\(.*\\)" line)
                       (push (format "%s %s (%s %s): %s"
                                     current-line-num current-commit current-author current-time
                                     (match-string 1 line))
                             results))))
                   (string-join (nreverse results) "\n"))))
   :description "Show git blame for a file, optionally for a specific line range"
   :args (list '(:name "file"
                 :type string
                 :description "File path to blame")
               '(:name "start_line"
                 :type integer
                 :optional t
                 :description "Starting line number")
               '(:name "end_line"
                 :type integer
                 :optional t
                 :description "Ending line number"))
   :category "git")
  "Tool to show git blame.")

(defvar gptel-git-tools--branch-list
  (gptel-make-tool
   :name "git_branch_list"
   :function (lambda (&optional all)
               (gptel-git-tools--ensure-repo)
               (let* ((args (if all '("branch" "-a" "-v") '("branch" "-v")))
                      (output (apply #'gptel-git-tools--run-git args))
                      (current (gptel-git-tools--run-git "branch" "--show-current")))
                 (format "Current branch: %s\n\n%s" current output)))
   :description "List git branches"
   :args (list '(:name "all"
                 :type boolean
                 :optional t
                 :description "If true, show remote branches as well"))
   :category "git")
  "Tool to list git branches.")

(defvar gptel-git-tools--stash-list
  (gptel-make-tool
   :name "git_stash_list"
   :function (lambda ()
               (gptel-git-tools--ensure-repo)
               (let ((output (gptel-git-tools--run-git "stash" "list")))
                 (if (string-empty-p output)
                     "No stashes found."
                   output)))
   :description "List all git stashes"
   :args nil
   :category "git")
  "Tool to list git stashes.")

;;; Write Tools (require confirmation)

(defvar gptel-git-tools--stage
  (gptel-make-tool
   :name "git_stage"
   :function (lambda (files)
               (gptel-git-tools--ensure-repo)
               (let ((file-list (if (listp files) files (list files))))
                 (dolist (file file-list)
                   (gptel-git-tools--run-git "add" file))
                 (format "Staged %d file(s): %s" (length file-list) (string-join file-list ", "))))
   :description "Stage files for commit. Pass 'all' to stage all changes, or a list of file paths."
   :args (list '(:name "files"
                 :type string
                 :description "File path to stage, or 'all' to stage everything"))
   :category "git"
   :confirm t)
  "Tool to stage files.")

(defvar gptel-git-tools--commit
  (gptel-make-tool
   :name "git_commit"
   :function (lambda (message)
               (gptel-git-tools--ensure-repo)
               (let ((output (gptel-git-tools--run-git "commit" "-m" message)))
                 output))
   :description "Create a git commit with the staged changes"
   :args (list '(:name "message"
                 :type string
                 :description "Commit message"))
   :category "git"
   :confirm t)
  "Tool to create a git commit.")

(defvar gptel-git-tools--checkout
  (gptel-make-tool
   :name "git_checkout"
   :function (lambda (target &optional create-branch)
               (gptel-git-tools--ensure-repo)
               (let ((args (if create-branch
                               (list "checkout" "-b" target)
                             (list "checkout" target))))
                 (apply #'gptel-git-tools--run-git args)))
   :description "Checkout a branch, commit, or file. Use create_branch=true to create a new branch."
   :args (list '(:name "target"
                 :type string
                 :description "Branch name, commit SHA, or file path to checkout")
               '(:name "create_branch"
                 :type boolean
                 :optional t
                 :description "If true, create a new branch with the given name"))
   :category "git"
   :confirm t)
  "Tool to checkout branches or files.")

;;; Collection of all git tools

(defvar gptel-git-tools-all
  (list gptel-git-tools--status
        gptel-git-tools--diff
        gptel-git-tools--log
        gptel-git-tools--blame
        gptel-git-tools--branch-list
        gptel-git-tools--stash-list
        gptel-git-tools--stage
        gptel-git-tools--commit
        gptel-git-tools--checkout)
  "List of all gptel git tools.")

(provide 'gptel-git-tools)
;;; gptel-git-tools.el ends here
