;;; gptel-tools.el --- Unified entry point for all gptel tools -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Combined gptel tool suite for software development

;;; Commentary:

;; This package provides a unified entry point for all gptel tools.
;; It loads and combines tools from multiple modules:
;;
;; - gptel-emacs-tools: Core Emacs introspection (buffers, files, search, etc.)
;; - gptel-git-tools: Git integration via magit
;; - gptel-lsp-tools: LSP/eglot with xref fallbacks
;; - gptel-compile-tools: Compilation and diagnostics
;; - gptel-edit-tools: Advanced buffer manipulation
;; - gptel-nav-tools: Navigation and symbols
;; - gptel-env-tools: Environment and processes
;; - gptel-doc-tools: Documentation lookup
;;
;; Usage:
;;   (require 'gptel-tools)
;;   (setq gptel-tools gptel-tools-all)
;;
;; Or use the interactive function:
;;   M-x gptel-tools-enable-all

;;; Code:

(require 'gptel)
(require 'gptel-emacs-tools)
(require 'gptel-git-tools)
(require 'gptel-lsp-tools)
(require 'gptel-compile-tools)
(require 'gptel-edit-tools)
(require 'gptel-nav-tools)
(require 'gptel-env-tools)
(require 'gptel-doc-tools)

;;; Combined tool list

(defvar gptel-tools-all
  (append gptel-emacs-tools-all
          gptel-git-tools-all
          gptel-lsp-tools-all
          gptel-compile-tools-all
          gptel-edit-tools-all
          gptel-nav-tools-all
          gptel-env-tools-all
          gptel-doc-tools-all)
  "All gptel tools combined from all modules.")

;;; Interactive functions

;;;###autoload
(defun gptel-tools-enable-all ()
  "Enable all gptel tools from all modules."
  (interactive)
  (setq gptel-tools gptel-tools-all)
  (message "Enabled %d gptel tools" (length gptel-tools-all)))

;;;###autoload
(defun gptel-tools-disable-all ()
  "Disable all gptel tools."
  (interactive)
  (setq gptel-tools nil)
  (message "Disabled all gptel tools"))

;;;###autoload
(defun gptel-tools-list ()
  "Display a list of all available gptel tools."
  (interactive)
  (let ((buf (get-buffer-create "*gptel-tools*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Available gptel tools:\n")
      (insert "======================\n\n")
      (let ((categories (make-hash-table :test 'equal)))
        ;; Group by category
        (dolist (tool gptel-tools-all)
          (let* ((category (or (plist-get tool :category) "uncategorized"))
                 (existing (gethash category categories)))
            (puthash category (cons tool existing) categories)))
        ;; Print by category
        (maphash
         (lambda (category tools)
           (insert (format "## %s (%d tools)\n\n" (upcase category) (length tools)))
           (dolist (tool (nreverse tools))
             (let ((name (plist-get tool :name))
                   (desc (plist-get tool :description))
                   (confirm (plist-get tool :confirm)))
               (insert (format "- %s%s\n  %s\n\n"
                               name
                               (if confirm " [confirm]" "")
                               desc))))
           (insert "\n"))
         categories))
      (goto-char (point-min)))
    (display-buffer buf)))

(provide 'gptel-tools)
;;; gptel-tools.el ends here
