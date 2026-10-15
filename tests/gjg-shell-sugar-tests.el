;;; gjg-shell-sugar-tests.el --- Tests for the shell sugar in README.org  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the `shell-sugar' subtree of README.org: the buffer names that
;; `gjg/open-remote-shell' and `gjg/open-remote-ghostel' give, and how
;; each hands them on.  `shell' and `ghostel' are faked for the naming
;; tests; TRAMP names are only parsed, never connected to.
;;
;; The last test drives real Ghostel from ~/projects/emacs/ghostel/lisp/,
;; where the config loads it, and is skipped when that or its native
;; module is missing.  It runs local shells in a temporary directory and
;; kills them when it ends.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gjg-config-test)

(gjg-config-test-load "README.org" "shell-sugar")

(declare-function gjg/shell-buffer-name "ext:README.org" (suffix))
(declare-function gjg/open-remote-shell "ext:README.org" ())
(declare-function gjg/open-remote-ghostel "ext:README.org" (&optional arg))
(declare-function ghostel--update-directory "ext:ghostel" (dir))
(defvar ghostel-buffer-name)
(defvar ghostel-buffer-name-function)

(defconst gjg-shell-sugar-test--ghostel-dir
  (expand-file-name "~/projects/emacs/ghostel/lisp/")
  "Where the config loads Ghostel from.")

(ert-deftest gjg-shell-sugar-buffer-name ()
  "Local names come from the directory, remote names from the host."
  (let ((default-directory "/home/someone/proj-x/"))
    (should (equal (gjg/shell-buffer-name "-gh") "proj-x-gh")))
  (let ((default-directory "/ssh:box:/var/log/"))
    (should (equal (gjg/shell-buffer-name "-gh") "box-gh")))
  (let ((default-directory "/ssh:root@box|sudo:box:/etc/"))
    (should (equal (gjg/shell-buffer-name "-sh") "box-sh"))))

(ert-deftest gjg-shell-sugar-shell-names ()
  "`gjg/open-remote-shell' still opens HOST-sh or DIR-sh, and does stuff."
  (let (names messages)
    (cl-letf (((symbol-function 'shell) (lambda (name) (push name names)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((default-directory "/home/someone/proj-x/"))
        (gjg/open-remote-shell))
      (should-not messages)
      (let ((default-directory "/ssh:box:/var/log/"))
        (gjg/open-remote-shell)))
    (should (equal names '("box-sh" "proj-x-sh")))
    (should (equal messages '("Now I shall do stuff")))))

(ert-deftest gjg-shell-sugar-ghostel-names ()
  "`gjg/open-remote-ghostel' hands `ghostel' HOST-gh or DIR-gh and the prefix."
  (let (calls)
    (cl-letf (((symbol-function 'ghostel)
               (lambda (&optional arg) (push (list ghostel-buffer-name arg) calls))))
      (let ((default-directory "/home/someone/proj-x/"))
        (gjg/open-remote-ghostel))
      (let ((default-directory "/ssh:box:/var/log/"))
        (gjg/open-remote-ghostel '(4))
        (gjg/open-remote-ghostel 2)))
    (should (equal (nreverse calls)
                   '(("proj-x-gh" nil) ("box-gh" (4)) ("box-gh" 2))))))

(ert-deftest gjg-shell-sugar-ghostel-reuse ()
  "A real terminal is reused after Ghostel renames it; \\[universal-argument] adds one."
  (skip-unless (file-directory-p gjg-shell-sugar-test--ghostel-dir))
  (let ((load-path (cons gjg-shell-sugar-test--ghostel-dir load-path)))
    (skip-unless (require 'ghostel nil t)))
  (skip-unless (ignore-errors (ghostel--load-module t) t))
  (let* ((dir (make-temp-file "gjg-shell-sugar-" t))
         (caller (generate-new-buffer " *caller*"))
         (ghostel-buffer-name-function #'ghostel-buffer-name-by-directory)
         terminals)
    (cl-flet ((open (&optional arg)
                ;; `ghostel' makes the terminal current, so always call
                ;; from the same buffer in the same directory.
                (let ((buffer (with-current-buffer caller
                                (gjg/open-remote-ghostel arg))))
                  (cl-pushnew buffer terminals)
                  buffer)))
      (unwind-protect
          (progn
            (with-current-buffer caller
              (setq default-directory (file-name-as-directory dir)))
            (let ((first (open)))
              (should (equal (buffer-name first)
                             (concat (file-name-nondirectory dir) "-gh")))
              (should (process-live-p (buffer-local-value 'ghostel--process first)))
              ;; As the shell reports a `cd' (OSC 7).
              (with-current-buffer first
                (ghostel--update-directory
                 (concat "file://" (system-name) temporary-file-directory)))
              (should (string-prefix-p "*ghostel: " (buffer-name first)))
              (should (eq (open) first))
              (let ((second (open '(4))))
                (should-not (eq second first))
                (should (equal (buffer-name second)
                               (concat (file-name-nondirectory dir) "-gh<2>")))
                (should (eq (open 2) second)))))
        (dolist (buffer terminals)
          (when (buffer-live-p buffer)
            (let ((process (buffer-local-value 'ghostel--process buffer)))
              (when (processp process)
                (set-process-query-on-exit-flag process nil)))
            (kill-buffer buffer)))
        (kill-buffer caller)
        (delete-directory dir t)))))

;;; gjg-shell-sugar-tests.el ends here
