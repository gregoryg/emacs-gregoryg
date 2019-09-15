;; *** Org-mode-hook



;;     Primarily used to set key strokes to my preference

;;     org-crypt-use-before-save-magic is called here because it has the
;;     side effect of adding to org-mode-hook

;; (require 'org-crypt)
;; (org-crypt-use-before-save-magic)

;; *** Org-mode BIG DUMP
;; persist clock history across emacs sessions

;; TODO: evaluate whether this is working
;; export to html - use light theme for export
;; tip o' the hat to legoscia https://github.com/legoscia/dotemacs/blob/master/dotemacs.org#theme-for-org-html-export
(defvar my-org-html-export-theme 'material-light)

(defun my-with-theme (orig-fun &rest args)
  (load-theme my-org-html-export-theme)
  (unwind-protect
      (apply orig-fun args)
    (disable-theme my-org-html-export-theme)))

(with-eval-after-load "ox-html"
  (advice-add 'org-export-to-buffer :around 'my-with-theme))




(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-report-include-clocking-task t)

(setq org-export-latex-tables-centered nil)
(setq org-read-date-prefer-future 'time)
(setq org-clock-sound "~/projectsc136.wav")
;; make c-y pay attention to current level
(setq org-yank-adjusted-subtrees t)
(setq org-yank-folded-subtrees nil)
(setq org-insert-heading-respect-content t); new headings go AFTER CONTENT, even from the middle of content area
(setq org-indirect-buffer-display 'current-window)
(setq org-reverse-note-order t) ; notes go at the top
(setq org-return-follows-link t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;; use S-arrow without having to log state change!
;;(setq org-log-state-notes-into-drawer t) ;;
(setq org-log-into-drawer t)
(setq org-cycle-include-plain-lists nil) ; nil, t, or 'integrate
(setq org-alphabetical-lists t) ;; allow alphabetical lists

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (save-excursion
    (insert "\n")
    (org-cycle)
    (org-insert-time-stamp nil t t nil nil nil)))

;; (add-hook 'org-insert-heading-hook 'bh/insert-inactive-timestamp)


;; (add-hook 'outline-minor-mode-hook
;;           'th-outline-minor-mode-init)


;; delete the seleted region when something is typed - don't do that
(delete-selection-mode -1)

;; *** Sad Panda (things set because something doesn't work right)


;; show only current/today's/all time in the clock task modeline
;; this SHOULD be settable on a file basis by using property CLOCK_MODELINE_TOTAL
(setq org-clock-modeline-total 'current)

