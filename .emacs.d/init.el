;; Keep track of loading time
(defconst emacs-start-time (current-time))
;;   On MacOS, define the meta key up front for usability when init does not load fully
(setq ns-command-modifier (quote meta))

;; initalize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; (setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
										  emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))


(require 'org)

(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory)
 nil ; compile the result
 )

;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading complete config...done (%.3fs)" elapsed))
