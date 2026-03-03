;;; gptel-native-file.el --- Native file tools for gptel -*- lexical-binding: t; -*-

;; Author: Gregory Grubbs & AI Assistant
;; Description: Native Elisp file manipulation tools for gptel that bypass MCP.

;;; Commentary:
;;
;; This package provides native file tools for gptel:
;; - list_allowed_directories
;; - read_text_file
;; - write_file
;; - edit_file (with built-in dryRun / diff generation!)
;;
;; Security:
;;   Only files within `gptel-file-allowed-directories` can be accessed.
;;   Subdirectories are automatically allowed via `file-in-directory-p`.

;;; Code:

(require 'gptel)
(require 'cl-lib)

(defcustom gptel-file-allowed-directories (list (expand-file-name "~/projects/"))
  "List of directories that the LLM is allowed to access."
  :type '(repeat directory)
  :group 'gptel)

(defun gptel-file--path-allowed-p (path)
  "Check if PATH is within `gptel-file-allowed-directories`."
  (let ((true-path (file-truename path)))
    (cl-some (lambda (dir)
               (string-prefix-p (file-truename dir) true-path))
             gptel-file-allowed-directories)))

(defun gptel-file-allow-directory (dir)
  "Add DIR to the allowed directories list for the LLM."
  (interactive "DAllow directory for LLM: ")
  (add-to-list 'gptel-file-allowed-directories (expand-file-name dir))
  (message "Added %s to allowed LLM directories." dir))

(defun gptel-file--backup (path)
  "Create a standard Emacs backup file (path~) before modifying."
  (when (file-exists-p path)
    (let ((backup-path (concat path "~")))
      (copy-file path backup-path t)
      (message "gptel-file: Backed up %s to %s" path backup-path))))

(defun gptel-file--get-json-prop (obj prop-string prop-symbol)
  "Safely extract a property from a JSON object parsed by Emacs."
  (cond
   ((hash-table-p obj) (gethash prop-string obj))
   ((listp obj) (or (cdr (assoc prop-string obj))
                    (cdr (assoc prop-symbol obj))))
   (t nil)))

;; Tool: read_file
(gptel-make-tool
 :name "read_file"
 :function (lambda (path &optional start_line max_lines)
             (unless (gptel-file--path-allowed-p path)
               (error "Access denied: Path %s is not in allowed directories." path))
             (unless (file-exists-p path)
               (error "File does not exist: %s" path))
             (let ((start (or start_line 1))
                   (limit (or max_lines 100))
                   (content ""))
               (with-temp-buffer
                 (insert-file-contents path)
                 (goto-char (point-min))
                 (forward-line (1- start))
                 (let ((start-pos (point)))
                   (forward-line limit)
                   (setq content (buffer-substring start-pos (point)))))
               (format "File: %s (Lines %d to %d)\n\n%s"
                       path start (+ start limit -1) content)))
 :description "Read a text file. Includes line limits to prevent dumping gigabytes into context. Default max_lines is 100. If you get an access denied error, explicitly ask the user to run M-x gptel-file-allow-directory."
 :args (list '(:name "path" :type string :description "Path to the file to read")
             '(:name "start_line" :type integer :description "Line number to start reading from (1-indexed). Defaults to 1." :optional t)
             '(:name "max_lines" :type integer :description "Maximum number of lines to read. Defaults to 100." :optional t))
 :category "filesystem")

;; Tool: write_file
(gptel-make-tool
 :name "write_file"
 :function (lambda (path content)
             (unless (gptel-file--path-allowed-p path)
               (error "Access denied: Path %s is not in allowed directories." path))
             (gptel-file--backup path)
             (with-temp-buffer
               (insert content)
               (write-region (point-min) (point-max) path nil 'silent))
             (format "Successfully wrote to %s (Backup created at %s~)" path path))
 :description "Create a new file or completely overwrite an existing file with new content. Automatically creates a backup file (~). If denied, ask the user to run M-x gptel-file-allow-directory."
 :args (list '(:name "path" :type string :description "Path to the file to write")
             '(:name "content" :type string :description "The full content to write to the file"))
 :category "filesystem")

;; Tool: edit_file
(gptel-make-tool
 :name "edit_file"
 :function (lambda (path edits &optional dry_run)
             (unless (gptel-file--path-allowed-p path)
               (error "Access denied: Path %s is not in allowed directories." path))
             (unless (file-exists-p path)
               (error "File does not exist: %s" path))
             (let ((buffer (generate-new-buffer " *gptel-edit*"))
                   (success-count 0)
                   (fail-count 0)
                   (results '()))
               (unwind-protect
                   (progn
                     (with-current-buffer buffer
                       (insert-file-contents path)
                       (cl-loop for edit across (if (vectorp edits) edits (vconcat edits))
                                do
                                (let* ((old-text (gptel-file--get-json-prop edit "oldText" 'oldText))
                                       (new-text (gptel-file--get-json-prop edit "newText" 'newText)))
                                  (goto-char (point-min))
                                  (if (search-forward old-text nil t)
                                      (progn
                                        (replace-match new-text t t)
                                        (cl-incf success-count))
                                    (cl-incf fail-count)
                                    (push (format "FAILED: Could not find exact string \n%s" old-text) results)))))

                     ;; Dry Run Diff Generation
                     (if dry_run
                         (let* ((temp-orig (make-temp-file "gptel-orig-"))
                                (temp-new (make-temp-file "gptel-new-"))
                                (diff-output ""))
                           (copy-file path temp-orig t)
                           (with-current-buffer buffer
                             (write-region (point-min) (point-max) temp-new nil 'silent))
                           (with-temp-buffer
                             (call-process "diff" nil t nil "-u" temp-orig temp-new)
                             (setq diff-output (buffer-string)))
                           (delete-file temp-orig)
                           (delete-file temp-new)
                           (format "DRY RUN DIFF:\n\n%s\n\nEdit summary: %d succeeded, %d failed."
                                   diff-output success-count fail-count))

                       ;; Actual File Save
                       (if (> fail-count 0)
                           (format "ABORTED: %d edits failed to match exactly. No changes were written to disk.\n%s"
                                   fail-count (mapconcat #'identity (nreverse results) "\n"))
                         (gptel-file--backup path)
                         (with-current-buffer buffer
                           (write-region (point-min) (point-max) path nil 'silent))
                         (format "SUCCESS: %d edits applied. File saved and backed up to %s~"
                                 success-count path))))
                 (kill-buffer buffer))))
 :description "Make exact string-based edits to a text file. Supports dryRun to preview git-style diffs. Automatically creates a backup (~). Aborts entirely if ANY oldText fails to match exactly. If denied, ask the user to run M-x gptel-file-allow-directory."
 :args (list '(:name "path" :type string :description "Path to the file to edit")
             '(:name "edits" :type array :description "Array of edit objects with 'oldText' and 'newText'"
               :items (:type object
                       :properties (:oldText (:type string :description "Text to search for")
                                    :newText (:type string :description "Text to replace with"))))
             '(:name "dryRun" :type boolean :description "If true, returns a diff without modifying the file" :optional t))
 :category "filesystem")


;; Tool: list_allowed_directories
(gptel-make-tool
 :name "list_allowed_directories"
 :function (lambda ()
             (format "Allowed directories:\n%s"
                     (mapconcat #'identity gptel-file-allowed-directories "\n")))
 :description "List all directories the LLM is currently allowed to access. If a directory you need is not included in this list, stop and alert the human to run M-x gptel-file-allow-directory."
 :category "filesystem")

(provide 'gptel-native-file)
;;; gptel-native-file.el ends here
