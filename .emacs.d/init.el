;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
;; (setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


(require 'org)

(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))

 ;; Message how long it took to load everything (minus packages)
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading settings...done (%.3fs)" elapsed))
