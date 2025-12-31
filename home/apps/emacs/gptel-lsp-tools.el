;;; gptel-lsp-tools.el --- LSP/Eglot tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: LSP integration tools for gptel with eglot, xref, and dumb-jump fallbacks

;;; Commentary:

;; This package provides LSP tools for gptel that allow LLMs to:
;; - Find definitions and references
;; - Get diagnostics (errors/warnings)
;; - View hover documentation
;; - Format buffers and rename symbols
;;
;; Tools gracefully fall back when eglot isn't active:
;; - Definition: eglot -> xref -> dumb-jump
;; - References: eglot -> xref
;; - Hover: eglot -> eldoc

;;; Code:

(require 'gptel)
(require 'xref)
(require 'flymake)

;; Declare external functions
(declare-function eglot-managed-p "eglot")
(declare-function eglot--hover-info "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-rename "eglot")
(declare-function eldoc-documentation-function "eldoc")
(declare-function dumb-jump-get-results "dumb-jump")

;;; Helper Functions

(defun gptel-lsp-tools--eglot-active-p ()
  "Return non-nil if eglot is active in current buffer."
  (and (featurep 'eglot)
       (fboundp 'eglot-managed-p)
       (eglot-managed-p)))

(defun gptel-lsp-tools--format-xref-location (xref)
  "Format an XREF item as a readable string."
  (let* ((loc (xref-item-location xref))
         (file (xref-location-group loc))
         (line (xref-location-line loc))
         (summary (xref-item-summary xref)))
    (format "%s:%d: %s" file line summary)))

(defun gptel-lsp-tools--get-symbol-at-point (buffer-name &optional line column)
  "Get symbol at point in BUFFER-NAME, optionally at LINE and COLUMN."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer '%s' does not exist" buffer-name))
    (with-current-buffer buf
      (save-excursion
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (when column
            (forward-char (min column (- (line-end-position) (line-beginning-position))))))
        (thing-at-point 'symbol t)))))

;;; Definition Tool

(defvar gptel-lsp-tools--find-definition
  (gptel-make-tool
   :name "lsp_find_definition"
   :function (lambda (buffer-name &optional symbol line column)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (when line
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (when column
                         (forward-char (min column (- (line-end-position) (line-beginning-position))))))
                     (let* ((sym (or symbol (thing-at-point 'symbol t)))
                            (backend (xref-find-backend))
                            (defs (when sym
                                    (condition-case nil
                                        (xref-backend-definitions backend sym)
                                      (error nil)))))
                       (cond
                        ((null sym)
                         (error "No symbol at point"))
                        ((null defs)
                         ;; Try dumb-jump as fallback
                         (if (featurep 'dumb-jump)
                             (let ((results (dumb-jump-get-results)))
                               (if (and results (plist-get results :results))
                                   (let ((matches (plist-get results :results)))
                                     (mapconcat (lambda (m)
                                                  (format "%s:%d: %s"
                                                          (plist-get m :path)
                                                          (plist-get m :line)
                                                          (plist-get m :context)))
                                                matches "\n"))
                                 (format "No definition found for '%s'" sym)))
                           (format "No definition found for '%s'" sym)))
                        (t
                         (mapconcat #'gptel-lsp-tools--format-xref-location defs "\n"))))))))
   :description "Find the definition of a symbol. Falls back to xref and dumb-jump if LSP is not available."
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer to search in")
               '(:name "symbol"
                 :type string
                 :optional t
                 :description "Symbol to look up (default: symbol at point)")
               '(:name "line"
                 :type integer
                 :optional t
                 :description "Line number to position at before lookup")
               '(:name "column"
                 :type integer
                 :optional t
                 :description "Column number to position at before lookup"))
   :category "lsp")
  "Tool to find symbol definitions.")

;;; References Tool

(defvar gptel-lsp-tools--find-references
  (gptel-make-tool
   :name "lsp_find_references"
   :function (lambda (buffer-name &optional symbol line column)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (when line
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (when column
                         (forward-char (min column (- (line-end-position) (line-beginning-position))))))
                     (let* ((sym (or symbol (thing-at-point 'symbol t)))
                            (backend (xref-find-backend))
                            (refs (when sym
                                    (condition-case nil
                                        (xref-backend-references backend sym)
                                      (error nil)))))
                       (cond
                        ((null sym)
                         (error "No symbol at point"))
                        ((null refs)
                         (format "No references found for '%s'" sym))
                        (t
                         (format "Found %d reference(s) for '%s':\n\n%s"
                                 (length refs)
                                 sym
                                 (mapconcat #'gptel-lsp-tools--format-xref-location refs "\n")))))))))
   :description "Find all references to a symbol"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer to search in")
               '(:name "symbol"
                 :type string
                 :optional t
                 :description "Symbol to look up (default: symbol at point)")
               '(:name "line"
                 :type integer
                 :optional t
                 :description "Line number to position at before lookup")
               '(:name "column"
                 :type integer
                 :optional t
                 :description "Column number to position at before lookup"))
   :category "lsp")
  "Tool to find symbol references.")

;;; Diagnostics Tool

(defvar gptel-lsp-tools--get-diagnostics
  (gptel-make-tool
   :name "lsp_get_diagnostics"
   :function (lambda (&optional buffer-name)
               (let ((buf (if buffer-name
                              (get-buffer buffer-name)
                            (current-buffer))))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (let ((diags (flymake-diagnostics)))
                     (if (null diags)
                         "No diagnostics found."
                       (mapconcat
                        (lambda (diag)
                          (let* ((beg (flymake-diagnostic-beg diag))
                                 (type (flymake-diagnostic-type diag))
                                 (text (flymake-diagnostic-text diag))
                                 (line (line-number-at-pos beg))
                                 (severity (cond
                                            ((eq type :error) "ERROR")
                                            ((eq type :warning) "WARNING")
                                            ((eq type :note) "NOTE")
                                            (t (symbol-name type)))))
                            (format "%s:%d [%s] %s"
                                    (buffer-name) line severity text)))
                        diags "\n"))))))
   :description "Get diagnostics (errors, warnings) for a buffer via flymake"
   :args (list '(:name "buffer_name"
                 :type string
                 :optional t
                 :description "Buffer to get diagnostics for (default: current buffer)"))
   :category "lsp")
  "Tool to get buffer diagnostics.")

;;; Hover Tool

(defvar gptel-lsp-tools--hover
  (gptel-make-tool
   :name "lsp_hover"
   :function (lambda (buffer-name &optional line column)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (when line
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (when column
                         (forward-char (min column (- (line-end-position) (line-beginning-position))))))
                     (let ((symbol (thing-at-point 'symbol t)))
                       (cond
                        ;; Try eglot hover first
                        ((gptel-lsp-tools--eglot-active-p)
                         (or (condition-case nil
                                 (eglot--hover-info (point))
                               (error nil))
                             (format "No hover info for '%s'" symbol)))
                        ;; Fall back to eldoc
                        ((and (boundp 'eldoc-documentation-function)
                              eldoc-documentation-function)
                         (or (funcall eldoc-documentation-function)
                             (format "No documentation for '%s'" symbol)))
                        (t
                         (format "No hover provider available for '%s'" symbol))))))))
   :description "Get hover documentation for symbol at point"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer to inspect")
               '(:name "line"
                 :type integer
                 :optional t
                 :description "Line number (1-indexed)")
               '(:name "column"
                 :type integer
                 :optional t
                 :description "Column number (0-indexed)"))
   :category "lsp")
  "Tool to get hover documentation.")

;;; Format Tool

(defvar gptel-lsp-tools--format-buffer
  (gptel-make-tool
   :name "lsp_format_buffer"
   :function (lambda (buffer-name)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (cond
                    ((gptel-lsp-tools--eglot-active-p)
                     (eglot-format-buffer)
                     (format "Formatted buffer '%s' via eglot" buffer-name))
                    (t
                     ;; Fallback to indent-region
                     (indent-region (point-min) (point-max))
                     (format "Formatted buffer '%s' via indent-region (no LSP)" buffer-name))))))
   :description "Format buffer contents using LSP or indent-region as fallback"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer to format"))
   :category "lsp"
   :confirm t)
  "Tool to format buffer.")

;;; Rename Tool

(defvar gptel-lsp-tools--rename-symbol
  (gptel-make-tool
   :name "lsp_rename_symbol"
   :function (lambda (buffer-name new-name &optional line column)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (when line
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (when column
                         (forward-char (min column (- (line-end-position) (line-beginning-position))))))
                     (let ((old-name (thing-at-point 'symbol t)))
                       (unless old-name
                         (error "No symbol at point"))
                       (unless (gptel-lsp-tools--eglot-active-p)
                         (error "LSP (eglot) is required for rename but is not active in this buffer"))
                       (eglot-rename new-name)
                       (format "Renamed '%s' to '%s'" old-name new-name))))))
   :description "Rename symbol at point across the project using LSP. Requires eglot to be active."
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer containing the symbol")
               '(:name "new_name"
                 :type string
                 :description "New name for the symbol")
               '(:name "line"
                 :type integer
                 :optional t
                 :description "Line number of the symbol")
               '(:name "column"
                 :type integer
                 :optional t
                 :description "Column number of the symbol"))
   :category "lsp"
   :confirm t)
  "Tool to rename symbols.")

;;; Collection of all LSP tools

(defvar gptel-lsp-tools-all
  (list gptel-lsp-tools--find-definition
        gptel-lsp-tools--find-references
        gptel-lsp-tools--get-diagnostics
        gptel-lsp-tools--hover
        gptel-lsp-tools--format-buffer
        gptel-lsp-tools--rename-symbol)
  "List of all gptel LSP tools.")

(provide 'gptel-lsp-tools)
;;; gptel-lsp-tools.el ends here
