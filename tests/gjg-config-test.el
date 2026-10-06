;;; gjg-config-test.el --- Test helpers for the Org-based config  -*- lexical-binding: t; -*-

;;; Commentary:

;; The config lives in Org files that `org-babel-tangle' turns into Emacs
;; Lisp.  These helpers let tests and checks work from the Org source
;; directly, in a bare `emacs -Q --batch':
;;
;; - `gjg-config-test-load' evaluates the code of one subtree, named by
;;   its CUSTOM_ID property, so a test never loads the whole config.
;; - `gjg-config-test-drift' compares what the Org files would tangle with
;;   the tangled files on disk.
;;
;; Blocks are collected with `org-babel-tangle-collect-blocks', the
;; tangler's own collector, so `:tangle no', COMMENT and ARCHIVE headings
;; and noweb references count exactly as they do for C-c C-v t.
;;
;; See "Checking the config" in README.org.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ob-tangle)

(defconst gjg-config-test-repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "The config repository: the directory above tests/.")

(defconst gjg-config-test-org-files
  '("README.org" "AI.org" "EXWM.org" "gjg-functions.org")
  "Org files whose Emacs Lisp the drift check compares with the tangled files.")

(defun gjg-config-test-source (org-file &optional custom-id)
  "Return the Emacs Lisp that tangling ORG-FILE would write, by target.
The value is an alist of (TARGET . SOURCE): TARGET is the absolute
name of a tangled file and SOURCE the code of its blocks, in order.
ORG-FILE is relative to `gjg-config-test-repo', or absolute.  With
CUSTOM-ID, only the subtree whose CUSTOM_ID property is CUSTOM-ID
counts, as if that subtree were narrowed and tangled."
  (let* ((file (expand-file-name org-file gjg-config-test-repo))
         (visited (get-file-buffer file))
         (buffer (or visited (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (when custom-id
              (gjg-config-test--narrow-to-custom-id custom-id org-file))
            (mapcar (lambda (by-target)
                      (cons (car by-target)
                            (mapconcat (lambda (lang-and-block)
                                         ;; Block: (LINE FILE LINK NAME PARAMS BODY COMMENT).
                                         (nth 5 (cdr lang-and-block)))
                                       (cdr by-target)
                                       "\n")))
                    (org-babel-tangle-collect-blocks
                     "\\`\\(emacs-lisp\\|elisp\\)\\'"))))
      (unless visited
        (kill-buffer buffer)))))

(defun gjg-config-test--narrow-to-custom-id (custom-id org-file)
  "Narrow to the subtree whose CUSTOM_ID is CUSTOM-ID.
Signal an error naming ORG-FILE unless exactly one heading has it."
  (let ((count (save-excursion
                 (goto-char (point-min))
                 (how-many (format "^[ \t]*:CUSTOM_ID:[ \t]+%s[ \t]*$"
                                   (regexp-quote custom-id))))))
    (unless (= count 1)
      (error "%s: %d headings have CUSTOM_ID %s, need exactly 1"
             org-file count custom-id)))
  (goto-char (org-find-property "CUSTOM_ID" custom-id))
  (org-narrow-to-subtree))

(defun gjg-config-test-load (org-file custom-id)
  "Evaluate the Emacs Lisp of the CUSTOM-ID subtree of ORG-FILE.
Its blocks are evaluated in tangle order with lexical binding, as
`load' would evaluate the tangled file.  Installed packages are
activated first, as init.el does, so the code finds what it uses."
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (let ((sources (gjg-config-test-source org-file custom-id)))
    (unless sources
      (error "%s: subtree %s has no Emacs Lisp to tangle" org-file custom-id))
    (with-temp-buffer
      (setq-local lexical-binding t)
      (dolist (source sources)
        (insert (cdr source) "\n"))
      (eval-buffer nil nil (format "%s#%s" org-file custom-id)))))

;;; Drift

(defun gjg-config-test--read-forms (string)
  "Return the list of forms read from STRING."
  (let ((pos 0)
        forms)
    (condition-case nil
        (while t
          (let ((read (read-from-string string pos)))
            (push (car read) forms)
            (setq pos (cdr read))))
      (end-of-file nil))
    (nreverse forms)))

(defun gjg-config-test--subtract (forms others)
  "Return FORMS without one `equal' occurrence of each of OTHERS."
  (let ((rest (copy-sequence forms)))
    (dolist (other others rest)
      (setq rest (cl-remove other rest :test #'equal :count 1)))))

(defun gjg-config-test-drift (&optional org-files)
  "Compare each of ORG-FILES with the files it tangles to.
ORG-FILES defaults to `gjg-config-test-org-files'.
Return a list of reports, one per tangled Emacs Lisp file, each a
plist with :org, :target, :missing (the file does not exist),
:only-org and :only-target (forms found on just one side, by
multiset difference) and :order (same forms, different order).
Code is compared as forms, so comments and layout do not count."
  (let (reports)
    (dolist (org-file (or org-files gjg-config-test-org-files) (nreverse reports))
      (dolist (by-target (gjg-config-test-source org-file))
        (let ((target (car by-target)))
          (when (string-suffix-p ".el" target)
            (if (not (file-exists-p target))
                (push (list :org org-file :target target :missing t) reports)
              (let* ((org-forms (gjg-config-test--read-forms (cdr by-target)))
                     (target-forms (with-temp-buffer
                                     (insert-file-contents target)
                                     (gjg-config-test--read-forms (buffer-string))))
                     (only-org (gjg-config-test--subtract org-forms target-forms))
                     (only-target (gjg-config-test--subtract target-forms org-forms)))
                (push (list :org org-file :target target
                            :only-org only-org :only-target only-target
                            :order (and (null only-org) (null only-target)
                                        (not (equal org-forms target-forms))))
                      reports)))))))))

(defun gjg-config-test--drifted-p (report)
  "Return non-nil if drift REPORT shows a difference."
  (or (plist-get report :missing)
      (plist-get report :only-org)
      (plist-get report :only-target)
      (plist-get report :order)))

(defun gjg-config-test--form-label (form)
  "Return a one-line label for FORM, such as (defun gjg/foo)."
  (truncate-string-to-width
   (prin1-to-string (if (consp form) (seq-take form 2) form))
   72 nil nil "…"))

(defun gjg-config-test-drift-batch ()
  "Print the drift check and exit: status 0 when every file is in sync."
  (let ((drifted 0))
    (dolist (report (gjg-config-test-drift))
      (let ((org-file (plist-get report :org))
            (target (abbreviate-file-name (plist-get report :target))))
        (if (not (gjg-config-test--drifted-p report))
            (princ (format "in sync  %s -> %s\n" org-file target))
          (setq drifted (1+ drifted))
          (princ (format "DRIFT    %s -> %s\n" org-file target))
          (when (plist-get report :missing)
            (princ "           tangled file is missing\n"))
          (dolist (form (plist-get report :only-org))
            (princ (format "           only in %s: %s\n"
                           org-file (gjg-config-test--form-label form))))
          (dolist (form (plist-get report :only-target))
            (princ (format "           only in %s: %s\n"
                           (file-name-nondirectory target)
                           (gjg-config-test--form-label form))))
          (when (plist-get report :order)
            (princ "           same forms, different order\n")))))
    (when (> drifted 0)
      (princ (format "\n%d tangled file(s) out of date: tangle the whole Org file (C-c C-v t, not narrowed)\n"
                     drifted)))
    (kill-emacs (if (zerop drifted) 0 1))))

(provide 'gjg-config-test)
;;; gjg-config-test.el ends here
