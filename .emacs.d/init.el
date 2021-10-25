;; Keep track of loading time
(defconst emacs-start-time (current-time))
;;   On MacOS, define the meta key up front for usability when init does not load fully
(setq ns-command-modifier (quote meta))
(setq garbage-collection-messages t)
(setq gc-cons-threshold 64000000)
(setq package-enable-at-startup nil)

(let ((elapsed (float-time (time-subtract (current-time)
										  emacs-start-time))))
  (message "Loaded absolutely zero packages in %.3fs" elapsed))


;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; keep Emacs from garbage collection during init
(setq gc-cons-threshold 1000000000) ;; ref https://github.com/MatthewZMD/.emacs.d#defer-garbage-collection

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
(straight-use-package 'org)
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory)
 nil ; byte-compile the result?
 )


;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading complete config...done (%.3fs)" elapsed))
(put 'list-timers 'disabled nil)
;; set memory usage for interactive use
(setq gc-cons-threshold 20000000) ; ref https://github.com/lewang/flx#gc-optimization
