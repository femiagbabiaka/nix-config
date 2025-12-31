;;; gptel-edit-tools.el --- Advanced buffer editing tools for gptel -*- lexical-binding: t; -*-

;; Author: Femi Agbabiaka
;; Description: Advanced buffer manipulation tools for gptel

;;; Commentary:

;; This package provides advanced editing tools for gptel that allow LLMs to:
;; - Delete and replace line ranges
;; - Undo changes
;; - Save and kill buffers
;;
;; These complement the basic edit_buffer and insert_at_point tools
;; in gptel-emacs-tools.el with more precise operations.

;;; Code:

(require 'gptel)

;;; Delete Lines Tool

(defvar gptel-edit-tools--delete-lines
  (gptel-make-tool
   :name "delete_lines"
   :function (lambda (buffer-name start-line end-line)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- start-line))
                     (let ((beg (point)))
                       (forward-line (- end-line start-line -1))
                       (let ((deleted-lines (count-lines beg (point))))
                         (delete-region beg (point))
                         (format "Deleted %d line(s) (%d-%d) from buffer '%s'"
                                 deleted-lines start-line end-line buffer-name)))))))
   :description "Delete a range of lines from a buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to edit")
               '(:name "start_line"
                 :type integer
                 :description "First line to delete (1-indexed)")
               '(:name "end_line"
                 :type integer
                 :description "Last line to delete (inclusive)"))
   :category "edit"
   :confirm t)
  "Tool to delete lines from a buffer.")

;;; Replace Lines Tool

(defvar gptel-edit-tools--replace-lines
  (gptel-make-tool
   :name "replace_lines"
   :function (lambda (buffer-name start-line end-line new-content)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- start-line))
                     (let ((beg (point)))
                       (forward-line (- end-line start-line -1))
                       (delete-region beg (point))
                       (goto-char beg)
                       (insert new-content)
                       (unless (string-suffix-p "\n" new-content)
                         (insert "\n"))
                       (format "Replaced lines %d-%d in buffer '%s'"
                               start-line end-line buffer-name))))))
   :description "Replace a range of lines with new content"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to edit")
               '(:name "start_line"
                 :type integer
                 :description "First line to replace (1-indexed)")
               '(:name "end_line"
                 :type integer
                 :description "Last line to replace (inclusive)")
               '(:name "new_content"
                 :type string
                 :description "New content to insert"))
   :category "edit"
   :confirm t)
  "Tool to replace lines in a buffer.")

;;; Undo Tool

(defvar gptel-edit-tools--undo
  (gptel-make-tool
   :name "undo_buffer"
   :function (lambda (buffer-name &optional count)
               (let ((buf (get-buffer buffer-name))
                     (n (or count 1)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (dotimes (_ n)
                     (undo))
                   (format "Undid %d change(s) in buffer '%s'" n buffer-name))))
   :description "Undo changes in a buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer")
               '(:name "count"
                 :type integer
                 :optional t
                 :description "Number of changes to undo (default: 1)"))
   :category "edit"
   :confirm t)
  "Tool to undo changes in a buffer.")

;;; Save Buffer Tool

(defvar gptel-edit-tools--save-buffer
  (gptel-make-tool
   :name "save_buffer"
   :function (lambda (buffer-name)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (if (buffer-file-name)
                       (progn
                         (save-buffer)
                         (format "Saved buffer '%s' to %s" buffer-name (buffer-file-name)))
                     (error "Buffer '%s' is not visiting a file" buffer-name)))))
   :description "Save a buffer to its file"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to save"))
   :category "edit")
  "Tool to save a buffer.")

;;; Kill Buffer Tool

(defvar gptel-edit-tools--kill-buffer
  (gptel-make-tool
   :name "kill_buffer"
   :function (lambda (buffer-name &optional force)
               (let ((buf (get-buffer buffer-name)))
                 (unless buf
                   (error "Buffer '%s' does not exist" buffer-name))
                 (with-current-buffer buf
                   (when (and (buffer-modified-p) (not force))
                     (error "Buffer '%s' has unsaved changes. Use force=true to kill anyway." buffer-name))
                   (let ((file (buffer-file-name)))
                     (kill-buffer buf)
                     (if file
                         (format "Killed buffer '%s' (was visiting %s)" buffer-name file)
                       (format "Killed buffer '%s'" buffer-name))))))
   :description "Kill (close) a buffer"
   :args (list '(:name "buffer_name"
                 :type string
                 :description "Name of the buffer to kill")
               '(:name "force"
                 :type boolean
                 :optional t
                 :description "If true, kill even if buffer has unsaved changes"))
   :category "edit"
   :confirm t)
  "Tool to kill a buffer.")

;;; Collection of all edit tools

(defvar gptel-edit-tools-all
  (list gptel-edit-tools--delete-lines
        gptel-edit-tools--replace-lines
        gptel-edit-tools--undo
        gptel-edit-tools--save-buffer
        gptel-edit-tools--kill-buffer)
  "List of all gptel edit tools.")

(provide 'gptel-edit-tools)
;;; gptel-edit-tools.el ends here
