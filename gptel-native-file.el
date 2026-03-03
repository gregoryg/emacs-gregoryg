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
;;
;; Directory management:
;;   The global default is set via `gptel-file-allowed-directories' (defcustom).
;;   Per-project directories are added buffer-locally via:
;;     - `gptel-file-allow-directory' (interactive, no prefix = buffer-local)
;;     - `gptel-file-allow-project-directory' (auto-detects project root)
;;     - `.dir-locals.el' eval forms
;;
;;   Example gptel-mode-hook usage (recommended):
;;     (add-hook 'gptel-mode-hook #'gptel-file-allow-buffer-directory)
;;
;;   This always adds the buffer's CWD, plus the project root
;;   (if project.el recognises one).  Both are buffer-local.
;;
;;   Example .dir-locals.el (for extra directories):
;;     ((nil . ((eval . (gptel-file-allow-directory "/path/to/data/")))))

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'project)

;; (defcustom gptel-file-allowed-directories (list (expand-file-name "~/projects/"))
(defcustom gptel-file-allowed-directories nil
  "Global default directories that the LLM is allowed to access.
This is the base list.  Buffer-local additions (e.g. the current
project root) are merged in automatically --- see
`gptel-file--effective-directories'.

Per-project directories should be added buffer-locally via
`gptel-file-allow-directory' or `gptel-file-allow-project-directory'
rather than modifying this global default."
  :type '(repeat directory)
  :group 'gptel)

(defun gptel-file--effective-directories ()
  "Return the effective allowed-directory list for the current buffer.
This is the buffer-local value of `gptel-file-allowed-directories',
which may include project-specific additions made by
`gptel-file-allow-directory' or `.dir-locals.el'."
  gptel-file-allowed-directories)

(defun gptel-file--path-allowed-p (path)
  "Check if PATH is within the effective allowed directories."
  (let ((true-path (file-truename path)))
    (cl-some (lambda (dir)
               (string-prefix-p (file-truename dir) true-path))
             (gptel-file--effective-directories))))

(defun gptel-file-allow-directory (dir)
  "Add DIR to the allowed directories list for the current buffer.
With a prefix argument, add to the global default list instead.

When called from `.dir-locals.el' or `gptel-mode-hook', the
variable is made buffer-local so that project-specific directories
do not leak into unrelated buffers."
  (interactive "DAllow directory for LLM: ")
  (let ((expanded (expand-file-name dir)))
    (if current-prefix-arg
        ;; Global: modify the defcustom default
        (progn
          (add-to-list 'gptel-file-allowed-directories expanded)
          (message "Added %s to GLOBAL allowed LLM directories." expanded))
      ;; Buffer-local: safe for per-project use
      (make-local-variable 'gptel-file-allowed-directories)
      (add-to-list 'gptel-file-allowed-directories expanded)
      (message "Added %s to buffer-local allowed LLM directories." expanded))))

(defun gptel-file-allow-buffer-directory ()
  "Add the buffer's working directory (and project root) to allowed dirs.
Always adds `default-directory' buffer-locally.  If `project-current'
also recognises a project root, that is added too.

Idempotent --- safe to call from `gptel-mode-hook' or `.dir-locals.el' eval."
  (interactive)
  (make-local-variable 'gptel-file-allowed-directories)
  (let ((cwd (expand-file-name default-directory)))
    (unless (member cwd gptel-file-allowed-directories)
      (add-to-list 'gptel-file-allowed-directories cwd)
      (message "gptel-file: auto-allowed CWD %s" cwd)))
  ;; Additionally allow the project root if one is detected
  (when-let* ((proj (project-current nil))
              (root (expand-file-name (project-root proj))))
    (unless (member root gptel-file-allowed-directories)
      (add-to-list 'gptel-file-allowed-directories root)
      (message "gptel-file: auto-allowed project root %s" root))))

(defun gptel-file--backup (path)
  "Create a standard Emacs backup file (path~) before modifying."
  (when (file-exists-p path)
    (let ((backup-path (concat path "~")))
      (copy-file path backup-path t)
      (message "gptel-file: Backed up %s to %s" path backup-path))))

(defun gptel-file--get-json-prop (obj prop-string)
  "Safely extract a property from a JSON object parsed by Emacs.
OBJ can be a plist (gptel's default JSON parse format), a hash table,
or an alist.  PROP-STRING is the JSON key name as a string, e.g. \"oldText\".
The keyword form (e.g. :oldText) is derived automatically for plist lookup."
  (let ((keyword (intern (concat ":" prop-string))))
    (cond
     ((hash-table-p obj) (gethash prop-string obj))
     ;; plist check: starts with a keyword symbol
     ((and (listp obj) (keywordp (car obj)))
      (plist-get obj keyword))
     ;; alist fallback
     ((listp obj) (or (cdr (assoc prop-string obj))
                      (cdr (assoc keyword obj))))
     (t nil))))

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
                                (let* ((old-text (gptel-file--get-json-prop edit "oldText"))
                                       (new-text (gptel-file--get-json-prop edit "newText")))
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
                     (mapconcat #'identity
                                (gptel-file--effective-directories) "\n")))
 :description "List all directories the LLM is currently allowed to access. If a directory you need is not included in this list, stop and alert the human to run M-x gptel-file-allow-directory."
 :category "filesystem")

(provide 'gptel-native-file)
;;; gptel-native-file.el ends here
