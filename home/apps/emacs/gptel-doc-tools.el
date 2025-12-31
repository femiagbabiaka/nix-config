;;; gptel-doc-tools.el --- Documentation tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Documentation lookup tools for gptel

;;; Commentary:

;; This package provides documentation tools for gptel that allow LLMs to:
;; - Look up man pages for commands

;;; Code:

(require 'gptel)

;; Declare Man functions
(declare-function Man-fontify-manpage "man")

;;; Man Page Tool

(defvar gptel-doc-tools--man-page
  (gptel-make-tool
   :name "man_page"
   :function (lambda (command &optional section)
               (let* ((topic (if section
                                 (format "%s(%s)" command section)
                               command))
                      (output (shell-command-to-string
                               (format "man %s 2>/dev/null | col -b"
                                       (shell-quote-argument
                                        (if section
                                            (format "%s %s" section command)
                                          command))))))
                 (if (or (string-empty-p output)
                         (string-match-p "No manual entry" output))
                     (format "No man page found for '%s'" topic)
                   ;; Truncate if too long
                   (let ((max-len 10000))
                     (if (> (length output) max-len)
                         (format "%s\n\n[Output truncated at %d characters]"
                                 (substring output 0 max-len)
                                 max-len)
                       output)))))
   :description "Look up a man page for a command"
   :args (list '(:name "command"
                 :type string
                 :description "Command to look up")
               '(:name "section"
                 :type string
                 :optional t
                 :description "Man page section (e.g., '1' for commands, '3' for library functions)"))
   :category "documentation")
  "Tool to look up man pages.")

;;; Collection of all documentation tools

(defvar gptel-doc-tools-all
  (list gptel-doc-tools--man-page)
  "List of all gptel documentation tools.")

(provide 'gptel-doc-tools)
;;; gptel-doc-tools.el ends here
