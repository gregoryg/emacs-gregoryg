;; Keep track of loading time
(defconst emacs-start-time (current-time))
;;   On MacOS, define the meta key up front for usability when init does not load fully
(setq ns-command-modifier (quote meta))
(setq garbage-collection-messages t)
;; (setq gc-cons-threshold 64000000)
(setq package-enable-at-startup nil)

(let ((elapsed (float-time (time-subtract (current-time)
										  emacs-start-time))))
  (message "Loaded absolutely zero packages in %.3fs" elapsed))


;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; keep Emacs from garbage collection during init + other helpers
(setq gc-cons-threshold 1000000000) ;; ref https://github.com/MatthewZMD/.emacs.d#defer-garbage-collection (x 10)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.

;; If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(setq org-replace-disputed-keys t)
(setq org-disputed-keys
   '(([(shift left)]
      .
      [(meta -)])
     ([(shift right)]
      .
      [(meta +)])
     ([(control shift right)]
      .
      [(meta shift +)])
     ([(control shift left)]
      .
      [(meta shift -)])))
(load "~/projects/emacs/straight.el/bootstrap.el")
(straight-use-package 'use-package)
;; (straight-use-package 'gcmh) ;; garbage collection magic hack
;; (gcmh-mode 1)
(straight-use-package 'org)
(require 'org)
;;(org-babel-tangle-file "~/emacs-gregoryg/README.org")
(load-file "~/.emacs.d/emacs-init.el")

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading complete config...done (%.3fs)" elapsed))
(put 'list-timers 'disabled nil)
;; set memory usage for interactive use
;; (setq gc-cons-threshold 20000000) ; ref https://github.com/lewang/flx#gc-optimization
;;(setq gc-cons-threshold 134217728)
   (setq gc-cons-threshold 33554432)
(setq file-name-handler-alist file-name-handler-alist-original)
