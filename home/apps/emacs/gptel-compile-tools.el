;;; gptel-compile-tools.el --- Compilation tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Compilation and build tools for gptel

;;; Commentary:

;; This package provides compilation tools for gptel that allow LLMs to:
;; - Run project compilation commands
;; - Parse compilation errors
;; - Recompile projects

;;; Code:

(require 'gptel)
(require 'compile)
(require 'project)

;;; Compile Project Tool

(defvar gptel-compile-tools--compile
  (gptel-make-tool
   :name "compile_project"
   :function (lambda (&optional command directory)
               (let ((default-directory (or directory
                                             (when-let ((proj (project-current)))
                                               (project-root proj))
                                             default-directory)))
                 (if command
                     (compile command)
                   (if (project-current)
                       (call-interactively #'project-compile)
                     (call-interactively #'compile)))
                 (format "Started compilation in %s with command: %s"
                         default-directory
                         (or command compile-command))))
   :description "Run a compilation command. If no command is given, uses project-compile or prompts."
   :args (list '(:name "command"
                 :type string
                 :optional t
                 :description "Compilation command to run (default: compile-command or project-compile)")
               '(:name "directory"
                 :type string
                 :optional t
                 :description "Directory to run compilation in (default: project root)"))
   :category "compile"
   :confirm t)
  "Tool to compile project.")

;;; Get Compilation Errors Tool

(defvar gptel-compile-tools--get-errors
  (gptel-make-tool
   :name "get_compilation_errors"
   :function (lambda ()
               (let ((buf (get-buffer "*compilation*")))
                 (unless buf
                   (error "No *compilation* buffer found. Run a compilation first."))
                 (with-current-buffer buf
                   (let ((errors nil))
                     (save-excursion
                       (goto-char (point-min))
                       (while (not (eobp))
                         (let ((msg (get-text-property (point) 'compilation-message)))
                           (when msg
                             (let* ((loc (compilation--message->loc msg))
                                    (file (when loc
                                            (caar (compilation--loc->file-struct loc))))
                                    (line (when loc
                                            (compilation--loc->line loc)))
                                    (col (when loc
                                           (compilation--loc->col loc)))
                                    (type (compilation--message->type msg))
                                    (type-name (cond
                                                ((eq type 0) "INFO")
                                                ((eq type 1) "WARNING")
                                                ((>= type 2) "ERROR")
                                                (t "UNKNOWN")))
                                    ;; Get the error text from the line
                                    (text (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))))
                               (when (and file line)
                                 (push (format "[%s] %s:%d:%s: %s"
                                               type-name
                                               file line (or col 0)
                                               (string-trim text))
                                       errors)))))
                         (forward-line 1)))
                     (if errors
                         (format "Found %d compilation message(s):\n\n%s"
                                 (length errors)
                                 (string-join (nreverse errors) "\n"))
                       "No compilation errors or warnings found.")))))
   :description "Get errors and warnings from the *compilation* buffer"
   :args nil
   :category "compile")
  "Tool to get compilation errors.")

;;; Recompile Tool

(defvar gptel-compile-tools--recompile
  (gptel-make-tool
   :name "recompile"
   :function (lambda ()
               (if (get-buffer "*compilation*")
                   (progn
                     (recompile)
                     "Recompilation started.")
                 (error "No previous compilation to rerun. Use compile_project first.")))
   :description "Re-run the last compilation command"
   :args nil
   :category "compile"
   :confirm t)
  "Tool to recompile.")

;;; Collection of all compile tools

(defvar gptel-compile-tools-all
  (list gptel-compile-tools--compile
        gptel-compile-tools--get-errors
        gptel-compile-tools--recompile)
  "List of all gptel compile tools.")

(provide 'gptel-compile-tools)
;;; gptel-compile-tools.el ends here
