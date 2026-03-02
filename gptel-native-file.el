;;; gptel-native-file.el --- Native filesystem tools for gptel -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'files)

;;; ============================================================================
;;; Customization & Security
;;; ============================================================================

(defgroup gptel-native-file nil
  "Native file tools for gptel."
  :group 'gptel
  :prefix "gptel-file-")

(defcustom gptel-file-allowed-directories
  (list (expand-file-name "~/projects/"))
  "List of directories that gptel tools are allowed to access.
Subdirectories within these directories are automatically permitted."
  :type '(repeat directory)
  :group 'gptel-native-file)

(defun gptel-file-allow-directory (dir)
  "Add DIR to the allowed directories list."
  (interactive "DDirectory to allow: ")
  (let ((expanded (file-name-as-directory (expand-file-name dir))))
    (add-to-list 'gptel-file-allowed-directories expanded)
    (message "Added %s to gptel allowed directories." expanded)))

(defun gptel-file-disallow-directory (dir)
  "Remove DIR from the allowed directories list."
  (interactive
   (list (completing-read "Directory to disallow: " gptel-file-allowed-directories nil t)))
  (setq gptel-file-allowed-directories (delete dir gptel-file-allowed-directories))
  (message "Removed %s from gptel allowed directories." dir))

(defun gptel-file--path-allowed-p (filepath)
  "Return t if FILEPATH is within an allowed directory."
  (let ((expanded (expand-file-name filepath)))
    (cl-some (lambda (allowed-dir)
               (file-in-directory-p expanded allowed-dir))
             gptel-file-allowed-directories)))

(defun gptel-file--assert-allowed (filepath)
  "Signal an error if FILEPATH is not allowed."
  (unless (gptel-file--path-allowed-p filepath)
    (error "Access denied: Path '%s' is not within allowed directories. Ask user to run M-x gptel-file-allow-directory" filepath)))

;;; ============================================================================
;;; Core Implementation
;;; ============================================================================

(defun gptel-file--read-file (filepath)
  "Return the contents of FILEPATH as a string."
  (gptel-file--assert-allowed filepath)
  (unless (file-exists-p filepath)
    (error "File not found: %s" filepath))
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun gptel-file--write-file (filepath content)
  "Write CONTENT to FILEPATH, overwriting completely."
  (gptel-file--assert-allowed filepath)
  (let ((dir (file-name-directory filepath)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) filepath nil 'silent))
  (format "Successfully wrote to %s" filepath))

(defun gptel-file--generate-diff (orig-content new-content)
  "Generate a unified diff string between ORIG-CONTENT and NEW-CONTENT."
  (let ((f1 (make-temp-file "gptel-diff-orig-"))
        (f2 (make-temp-file "gptel-diff-new-"))
        (diff-buf (generate-new-buffer " *gptel-diff*")))
    (unwind-protect
        (progn
          (with-temp-file f1 (insert orig-content))
          (with-temp-file f2 (insert new-content))
          ;; call-process diff returns 1 if differences found, 0 if identical
          (call-process "diff" nil diff-buf nil "-u" f1 f2)
          (with-current-buffer diff-buf
            (if (= (buffer-size) 0)
                "No changes detected."
              (buffer-string))))
      (ignore-errors
        (delete-file f1)
        (delete-file f2)
        (kill-buffer diff-buf)))))

(defun gptel-file--get-prop (obj prop)
  "Helper to extract PROP from OBJ (handles both hash-tables and alists)."
  (if (hash-table-p obj)
      (gethash prop obj)
    (alist-get (intern prop) obj)))

(defun gptel-file--edit-file (filepath edits dry-run)
  "Apply EDITS to FILEPATH. If DRY-RUN is t, return diff without saving."
  (gptel-file--assert-allowed filepath)
  (unless (file-exists-p filepath)
    (error "File not found: %s" filepath))
  
  (let* ((orig-content (gptel-file--read-file filepath))
         (new-content orig-content)
         (edits-seq (append edits nil))) ; ensure list

    ;; Apply edits sequentially in a temp buffer
    (with-temp-buffer
      (insert orig-content)
      (dolist (edit edits-seq)
        (let ((old-text (gptel-file--get-prop edit "oldText"))
              (new-text (gptel-file--get-prop edit "newText")))
          (goto-char (point-min))
          (if (search-forward old-text nil t)
              (replace-match new-text t t)
            (error "Could not find exact text match for: '%s'" 
                   (substring old-text 0 (min (length old-text) 50))))))
      (setq new-content (buffer-string)))
    
    (if dry-run
        (concat "DRY RUN DIFF:\n" (gptel-file--generate-diff orig-content new-content))
      (progn
        (gptel-file--write-file filepath new-content)
        (concat "File updated successfully.\nCHANGES:\n" 
                (gptel-file--generate-diff orig-content new-content))))))

;;; ============================================================================
;;; Tool Registrations
;;; ============================================================================

(with-eval-after-load 'gptel
  
  (gptel-make-tool
   :name "list_allowed_directories"
   :description "Returns the list of directories that this server is allowed to access. Subdirectories within these are also accessible."
   :category "filesystem"
   :function (lambda ()
               (format "Allowed Directories:\n%s\n\nAsk the user to run M-x gptel-file-allow-directory to add more."
                       (mapconcat #'identity gptel-file-allowed-directories "\n"))))

  (gptel-make-tool
   :name "read_text_file"
   :description "Read the content of a file within allowed directories."
   :category "filesystem"
   :function #'gptel-file--read-file
   :args '( (:name "path" :type string :description "Path to the file") ))

  (gptel-make-tool
   :name "write_file"
   :description "Create a new file or completely overwrite an existing file. Use with caution."
   :category "filesystem"
   :function #'gptel-file--write-file
   :args '( (:name "path" :type string :description "Path to write to")
            (:name "content" :type string :description "File content") ))

  (gptel-make-tool
   :name "edit_file"
   :description "Make line-based exact string replacements in a text file. Returns a git-style diff."
   :category "filesystem"
   :function (lambda (path edits &optional dryRun)
               (gptel-file--edit-file path edits dryRun))
   :args '( (:name "path" :type string :description "Path to the file")
            (:name "edits" :type array :description "Array of edit objects containing oldText and newText"
                   :items (:type object
                           :properties (:oldText (:type string :description "Text to search for - must match exactly")
                                        :newText (:type string :description "Text to replace with"))
                           :required ("oldText" "newText")))
            (:name "dryRun" :type boolean :description "Preview changes using git-style diff format") )))

(provide 'gptel-native-file)
;;; gptel-native-file.el ends here