
(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ("\221" . "`")
                            ("\222" . "'")
                            ("\223" . "\"")
                            ("\224" . "\"")
                            ("\226" . "-")
                            ("\227" . "--")
                            )
                          nil beg end))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun gjg/nag-timer () "Nag me when there isn't a clock running"  
  (unless (marker-buffer org-clock-marker)
    (call-process-shell-command gjg/visual-notify-command nil 0 nil "Org-Mode\\ Nudge" "What\\'s\\ \\going\\ on?")
    ;;    (call-process gjg/say-command nil 0 nil "What's going on?")
;;     (call-process "/usr/bin/aoss" nil 0 nil "/usr/bin/flite" "-t" "What is going on?")
    (call-process "espeak" nil nil nil  "What is going on?")
    ))

(defun gjg/nag () "Start nagging me every 15 minutes when not clocked in to an Org task"
  (interactive)
  (run-with-timer 1 900 'gjg/nag-timer))

(defun gjg/cancel-nag () "Cancel the org-mode clock nag timer"
  (interactive)
  (cancel-function-timers 'gjg/nag-timer))

(defun gjg/acronyminize (text &optional do-capitalize)
  "Make an acronym from the text 
do-capitalize: t means run text through capitalize function, nil will respect CamelCase
"
  (save-excursion
    (setq case-fold-search nil)
    (downcase
     (replace-regexp-in-string
      "[^A-Z]" ""
      (if do-capitalize (capitalize text) text) nil t))
    ))

(defun gjg/add-ids-to-headers ()
  "Add unique IDs to all headers in a buffer from current point; for use with org-mobile"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (while (not (eobp))
      (org-id-get-create) ;; only creates an ID property if none exists
      (outline-next-heading))))

(defun gjg/move-next-sexp-past-current-scope ()
  "kill sexp following point, move past current scope/sexp/function, yank"
  (beginning-of-line)
  ;; (save-excursion
  (let ((beg (point)))
    (re-search-forward "^[ \t]*function[ \t]+[^}]+?}" (point-max) nil)
    (mark-defun)
    (kill-region (point) (mark)))
  (forward-line)
  (yank)
  (indent-region (mark) (point)))


(defcustom gjg/audioplayer "mplayer"
  "Audio player for this machine"
  :type 'string
  :group 'gjg)


;; TODO: integrate lsp-mode maybe someday

;; ** LSP-Mode
;; #+begin_src emacs-lisp :tangle no
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   (lsp-auto-guess-root nil)
;;   (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :hook ((python-mode c-mode c++-mode) . lsp))


;; (use-package lsp-ui
;;   :after lsp-mode
;;   :ensure t
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;   (lsp-ui-doc-background ((t (:background nil))))
;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (setq lsp-ui-doc-use-webkit t)
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))
;; #+end_src


(provide 'gjg-functions)
