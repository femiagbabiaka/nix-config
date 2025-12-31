;;; gptel-env-tools.el --- Environment tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Environment and process inspection tools for gptel

;;; Commentary:

;; This package provides environment tools for gptel that allow LLMs to:
;; - Read environment variables
;; - List running Emacs subprocesses

;;; Code:

(require 'gptel)

;;; Get Environment Variable Tool

(defvar gptel-env-tools--get-env
  (gptel-make-tool
   :name "get_environment_variable"
   :function (lambda (name)
               (let ((value (getenv name)))
                 (if value
                     (format "%s=%s" name value)
                   (format "Environment variable '%s' is not set" name))))
   :description "Get the value of an environment variable"
   :args (list '(:name "name"
                 :type string
                 :description "Name of the environment variable"))
   :category "environment")
  "Tool to get an environment variable.")

;;; List Processes Tool

(defvar gptel-env-tools--list-processes
  (gptel-make-tool
   :name "list_processes"
   :function (lambda ()
               (let ((procs (process-list)))
                 (if (null procs)
                     "No running subprocesses."
                   (let ((results nil))
                     (dolist (proc procs)
                       (let* ((name (process-name proc))
                              (status (process-status proc))
                              (buffer (process-buffer proc))
                              (buffer-name (when buffer (buffer-name buffer)))
                              (command (process-command proc))
                              (cmd-str (when command
                                         (mapconcat #'identity command " "))))
                         (push (format "- %s [%s]%s%s"
                                       name
                                       status
                                       (if buffer-name (format " buffer: %s" buffer-name) "")
                                       (if cmd-str (format "\n  command: %s" cmd-str) ""))
                               results)))
                     (format "Running processes (%d):\n\n%s"
                             (length procs)
                             (string-join (nreverse results) "\n"))))))
   :description "List all running Emacs subprocesses"
   :args nil
   :category "environment")
  "Tool to list Emacs subprocesses.")

;;; Collection of all environment tools

(defvar gptel-env-tools-all
  (list gptel-env-tools--get-env
        gptel-env-tools--list-processes)
  "List of all gptel environment tools.")

(provide 'gptel-env-tools)
;;; gptel-env-tools.el ends here
