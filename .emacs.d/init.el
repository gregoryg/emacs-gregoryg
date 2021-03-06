;; Keep track of loading time
(defconst emacs-start-time (current-time))
;;   On MacOS, define the meta key up front for usability when init does not load fully
(setq ns-command-modifier (quote meta))
(setq garbage-collection-messages t)
(setq gc-cons-threshold 64000000)
;; initalize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq use-package-always-ensure t)

;; (package-initialize)

;; (setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
										  emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))


;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; keep Emacs from garbage collection during init
(setq gc-cons-threshold 100000000) ;; ref https://github.com/MatthewZMD/.emacs.d#defer-garbage-collection


(require 'org)

(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory)
 nil ; compile the result
 )


;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading complete config...done (%.3fs)" elapsed))
(put 'list-timers 'disabled nil)
;; set memory usage for interactive use
(setq gc-cons-threshold 20000000) ; ref https://github.com/lewang/flx#gc-optimization
