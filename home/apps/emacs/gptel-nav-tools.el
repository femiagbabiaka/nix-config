;;; gptel-nav-tools.el --- Navigation tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Code navigation and symbol tools for gptel

;;; Commentary:

;; This package provides navigation tools for gptel that allow LLMs to:
;; - List symbols/definitions in a buffer via imenu
;; - Jump to definition of symbol at cursor

;;; Code:

(require 'gptel)
(require 'imenu)

;;; Imenu Symbols Tool

(defvar gptel-nav-tools--imenu-symbols
  (gptel-make-tool
   :name "imenu_symbols"
   :function (lambda (buffer-name)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (let ((index (ignore-errors (imenu--make-index-alist t))))
                     (if (null index)
                         "No symbols found (imenu not available for this buffer)."
                       (let ((results nil))
                         (cl-labels ((process-index (items &optional prefix)
                                       (dolist (item items)
                                         (cond
                                          ;; Nested alist (submenu)
                                          ((and (consp item) (consp (cdr item)) (not (number-or-marker-p (cdr item))))
                                           (process-index (cdr item) (concat prefix (car item) "/")))
                                          ;; Regular item with position
                                          ((and (consp item) (or (number-or-marker-p (cdr item))
                                                                  (overlayp (cdr item))))
                                           (let* ((name (car item))
                                                  (pos (cdr item))
                                                  (pos-num (cond
                                                            ((overlayp pos) (overlay-start pos))
                                                            ((markerp pos) (marker-position pos))
                                                            (t pos)))
                                                  (line (when pos-num
                                                          (save-excursion
                                                            (goto-char pos-num)
                                                            (line-number-at-pos)))))
                                             (when (and name (not (string= name "*Rescan*")))
                                               (push (format "%s%s (line %d)"
                                                             (or prefix "")
                                                             name
                                                             (or line 0))
                                                     results))))
                                          ;; Item with overlay or marker in a list
                                          ((and (consp item) (listp (cdr item)))
                                           (let* ((name (car item))
                                                  (pos (cadr item))
                                                  (pos-num (cond
                                                            ((overlayp pos) (overlay-start pos))
                                                            ((markerp pos) (marker-position pos))
                                                            ((number-or-marker-p pos) pos)
                                                            (t nil)))
                                                  (line (when pos-num
                                                          (save-excursion
                                                            (goto-char pos-num)
                                                            (line-number-at-pos)))))
                                             (when (and name (not (string= name "*Rescan*")))
                                               (push (format "%s%s (line %d)"
                                                             (or prefix "")
                                                             name
                                                             (or line 0))
                                                     results))))))))
                           (process-index index))
                         (if results
                             (format "Symbols in '%s':\n\n%s"
                                     buffer-name
                                     (string-join (nreverse results) "\n"))
                           "No symbols found.")))))))
   :description "List all symbols (functions, classes, variables) in a buffer using imenu"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to inspect"))
   :category "navigation")
  "Tool to list buffer symbols via imenu.")

;;; Goto Definition at Point Tool

(defvar gptel-nav-tools--goto-definition
  (gptel-make-tool
   :name "goto_definition_at_point"
   :function (lambda (buffer-name line column)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (forward-char (min column (- (line-end-position) (line-beginning-position))))
                     (let* ((symbol (thing-at-point 'symbol t))
                            (backend (xref-find-backend))
                            (defs (when symbol
                                    (condition-case nil
                                        (xref-backend-definitions backend symbol)
                                      (error nil)))))
                       (cond
                        ((null symbol)
                         (error "No symbol at line %d, column %d" line column))
                        ((null defs)
                         (format "No definition found for '%s'" symbol))
                        (t
                         (let ((def (car defs)))
                           (let* ((loc (xref-item-location def))
                                  (file (xref-location-group loc))
                                  (def-line (xref-location-line loc))
                                  (summary (xref-item-summary def)))
                             ;; Actually jump to the definition
                             (xref-pop-to-location loc)
                             (format "Jumped to definition of '%s' at %s:%d\n%s"
                                     symbol file def-line summary))))))))))
   :description "Jump to the definition of the symbol at a specific position"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Buffer containing the symbol")
               '(:name "line"
                 :type integer
                 :description "Line number (1-indexed)")
               '(:name "column"
                 :type integer
                 :description "Column number (0-indexed)"))
   :category "navigation")
  "Tool to jump to definition at point.")

;;; Collection of all navigation tools

(defvar gptel-nav-tools-all
  (list gptel-nav-tools--imenu-symbols
        gptel-nav-tools--goto-definition)
  "List of all gptel navigation tools.")

(provide 'gptel-nav-tools)
;;; gptel-nav-tools.el ends here
