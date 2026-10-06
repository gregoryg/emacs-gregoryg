;;; gjg-config-test-tests.el --- Tests for the config test helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Every other check relies on these helpers collecting the same blocks
;; that C-c C-v t would tangle, so they get tests of their own against a
;; small throwaway Org file.

;;; Code:

(require 'ert)
(require 'gjg-config-test)

(declare-function gjg-config-test-tests--adder "ext:test.org" (n))

(defmacro gjg-config-test-tests--with-org (org &rest body)
  "Run BODY with `org-file', `out' and `other' bound in a temporary directory.
ORG is the Org text to write to `org-file'; %OUT% and %OTHER% in it
stand for the two tangle targets `out' and `other'."
  (declare (indent 1) (debug t))
  `(let* ((dir (make-temp-file "gjg-config-test" t))
          (org-file (expand-file-name "test.org" dir))
          (out (expand-file-name "out.el" dir))
          (other (expand-file-name "other.el" dir)))
     (ignore other)
     (unwind-protect
         (progn
           (with-temp-file org-file
             (insert (replace-regexp-in-string
                      "%OTHER%" other
                      (replace-regexp-in-string "%OUT%" out ,org t t)
                      t t)))
           ,@body)
       (delete-directory dir t))))

(defconst gjg-config-test-tests--org
  "#+property: header-args:emacs-lisp :tangle %OUT%
* One
  #+begin_src emacs-lisp
    (defvar gjg-config-test-tests--a 1)
  #+end_src
** Child
   #+begin_src emacs-lisp
     (defvar gjg-config-test-tests--b 2)
   #+end_src
   #+begin_src emacs-lisp :tangle no
     (defvar gjg-config-test-tests--skipped 0)
   #+end_src
   #+begin_src bash
     echo not lisp
   #+end_src
* COMMENT Commented out
  #+begin_src emacs-lisp
    (defvar gjg-config-test-tests--commented 0)
  #+end_src
* Two
  :PROPERTIES:
  :CUSTOM_ID: two
  :END:
  #+begin_src emacs-lisp
    (defun gjg-config-test-tests--adder (n)
      (lambda (m) (+ n m)))
  #+end_src
  #+begin_src emacs-lisp :tangle %OTHER%
    (defvar gjg-config-test-tests--d 4)
  #+end_src
"
  "Org text with blocks that tangle, blocks that don't, and two targets.")

(defun gjg-config-test-tests--forms (source-alist target)
  "Return the forms SOURCE-ALIST holds for TARGET."
  (gjg-config-test--read-forms (cdr (assoc target source-alist))))

(ert-deftest gjg-config-test-source-matches-tangle ()
  "Only Emacs Lisp that C-c C-v t would write, in order, by target."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    (let ((source (gjg-config-test-source org-file)))
      (should (equal (mapcar #'car source) (list out other)))
      (should (equal (gjg-config-test-tests--forms source out)
                     '((defvar gjg-config-test-tests--a 1)
                       (defvar gjg-config-test-tests--b 2)
                       (defun gjg-config-test-tests--adder (n)
                         (lambda (m) (+ n m)))))))))

(ert-deftest gjg-config-test-source-custom-id ()
  "A CUSTOM_ID limits the code to that subtree, whatever its targets."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    (let ((source (gjg-config-test-source org-file "two")))
      (should (equal (mapcar #'car source) (list out other)))
      (should (equal (car (gjg-config-test-tests--forms source out))
                     '(defun gjg-config-test-tests--adder (n)
                        (lambda (m) (+ n m))))))))

(ert-deftest gjg-config-test-source-custom-id-must-be-unique ()
  "A missing or repeated CUSTOM_ID is an error, not a silent guess."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    (should-error (gjg-config-test-source org-file "nope"))
    (with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-max))
      (insert "* Again\n  :PROPERTIES:\n  :CUSTOM_ID: two\n  :END:\n")
      (write-region nil nil org-file))
    (should-error (gjg-config-test-source org-file "two"))))

(ert-deftest gjg-config-test-load-uses-lexical-binding ()
  "Loaded code behaves as the tangled file would: closures capture."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    (gjg-config-test-load org-file "two")
    (should (= 5 (funcall (gjg-config-test-tests--adder 2) 3)))
    (should (= 4 (symbol-value 'gjg-config-test-tests--d)))))

(defun gjg-config-test-tests--drift-for (org-file target)
  "Return the drift report for TARGET of ORG-FILE."
  (seq-find (lambda (report) (equal (plist-get report :target) target))
            (gjg-config-test-drift (list org-file))))

(defconst gjg-config-test-tests--tangled
  ";; Tangled, with comments and layout the Org file doesn't have.
(defvar gjg-config-test-tests--a 1)
(defvar gjg-config-test-tests--b
  2)
(defun gjg-config-test-tests--adder (n) (lambda (m) (+ n m)))
"
  "What `out' holds when it is in sync with the test Org file.")

(ert-deftest gjg-config-test-drift-in-sync ()
  "Comments and layout don't count as drift."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    (with-temp-file out (insert gjg-config-test-tests--tangled))
    (should-not (gjg-config-test--drifted-p
                 (gjg-config-test-tests--drift-for org-file out)))))

(ert-deftest gjg-config-test-drift-reports-differences ()
  "Missing files, one-sided forms, repeats and reordering are drift."
  (gjg-config-test-tests--with-org gjg-config-test-tests--org
    ;; `other' was never tangled.
    (should (plist-get (gjg-config-test-tests--drift-for org-file other) :missing))
    ;; An edit not yet tangled.
    (with-temp-file out
      (insert (replace-regexp-in-string "--b\n  2" "--b\n  3"
                                        gjg-config-test-tests--tangled t t)))
    (let ((report (gjg-config-test-tests--drift-for org-file out)))
      (should (equal (plist-get report :only-org)
                     '((defvar gjg-config-test-tests--b 2))))
      (should (equal (plist-get report :only-target)
                     '((defvar gjg-config-test-tests--b 3)))))
    ;; A form tangled twice counts twice.
    (with-temp-file out
      (insert gjg-config-test-tests--tangled
              "(defvar gjg-config-test-tests--a 1)\n"))
    (should (equal (plist-get (gjg-config-test-tests--drift-for org-file out)
                              :only-target)
                   '((defvar gjg-config-test-tests--a 1))))
    ;; Same forms, moved.
    (with-temp-file out
      (insert "(defvar gjg-config-test-tests--b 2)\n"
              "(defvar gjg-config-test-tests--a 1)\n"
              "(defun gjg-config-test-tests--adder (n) (lambda (m) (+ n m)))\n"))
    (should (plist-get (gjg-config-test-tests--drift-for org-file out) :order))))

;;; gjg-config-test-tests.el ends here
