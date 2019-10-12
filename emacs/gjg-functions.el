(defun gjg/open-remote-shell ()
  "If current buffer is remote, open a new uniquely named shell based on host name"
  (interactive)
  (if (file-remote-p default-directory)
      (progn
        ;; do stuff
        (message "Now I shall do stuff")
        (shell (concat (file-remote-p default-directory 'host) "-sh"))
        )
	(progn
	  (shell (concat "local-" default-directory "-sh"))
	  ;; (message "Buffer is local - not opening shell"))
	  )))

(defun gjg/tramp-sudo-to-etc ()
  "Dired browse as root (sudo) to /etc on current machine"
  (interactive)
  (let* ((trampvec (tramp-dissect-file-name default-directory))
         (tramphop (elt trampvec 4))
         (conntype (elt trampvec 0))
         (trampuser (elt trampvec 1)) ; may be nil, which is cool
         (tramphost (elt trampvec 2))
         (sudopath (concat "/" tramphop conntype ":" tramphost "|sudo:" tramphost ":/etc/"))
         )
    (find-file sudopath)
    )
  )

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

;; (defun gjg/nag-timer () "Nag me when there isn't a clock running"  
;;   (unless (marker-buffer org-clock-marker)
;;     (call-process-shell-command gjg/visual-notify-command nil 0 nil "Org-Mode\\ Nudge" "What\\'s\\ \\going\\ on?")
;;     ;;    (call-process gjg/say-command nil 0 nil "What's going on?")
;; ;;     (call-process "/usr/bin/aoss" nil 0 nil "/usr/bin/flite" "-t" "What is going on?")
;;     (call-process "espeak" nil nil nil  "What is going on?")
;;     ))

;; (defun gjg/nag () "Start nagging me every 15 minutes when not clocked in to an Org task"
;;   (interactive)
;;   (run-with-timer 1 900 'gjg/nag-timer))

;; (defun gjg/cancel-nag () "Cancel the org-mode clock nag timer"
;;   (interactive)
;;   (cancel-function-timers 'gjg/nag-timer))

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

(defun gjg/org-visit-file (&optional restrict)
  "Visit an Org file in org-agenda-files or in my org-directory.
With prefix, restrict to files currently being visited"
  (interactive "p")
  (let ((f (if (functionp 'ido-completing-read) 'ido-completing-read
             'completing-read))
        (files (if (> restrict 1) (mapcar 'buffer-file-name (org-buffer-list 'files))
                 (append org-agenda-files (directory-files org-directory t "\.org$")))))
    (find-file (funcall f "Visit org file: " files))))

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


;; lol pseudocode ; api ref http://cheezburger.com/apidocs/ContentRetrieval.aspx
;; build url  http://api.cheezburger.com/xml/category/{category}/lol/random
;; categories: cats,objects,dogs,other animals,news,celebs,fail,engrish,comix
;; parse using xml-read from xml.el
;; use (assoc 'LolImageUrl (car myxmlparse))
;; keys are LolId, LolImageUrl, ThumbnailImageUrl, LolPageUrl, FullText, PictureId, PictureImageUrl, Title, Description, SourcePictures, TimeStamp

(require 'vc)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; snarfed from gmane.emacs.help
(defun my-dired-multi-occur (string)
  "Search string in files marked by dired."
  (interactive "MList lines matching regexp: ")
  (require 'dired)
  (multi-occur (mapcar 'find-file (dired-get-marked-files)) string))


(defcustom gjg/audioplayer "mplayer"
  "Audio player for this machine"
  :type 'string
  :group 'gjg)


(defcustom gjg/appointment-icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
  "Icon to display when announcing an appointment"
  :type 'string
  :group 'gjg)
(defcustom gjg/appointment-sound "/usr/share/sounds/ubuntu/stereo/service-login.ogg"
  "Sound to play when announcing an appointment"
  :type 'string
  :group 'gjg)

(defun rgr/org-add-note-to-current-task ()
  (interactive)
  (save-window-excursion
    (if(org-clock-is-active)
        (org-clock-goto)
      (org-clock-goto t))
    (org-narrow-to-subtree)
    (org-add-note)))

(define-key global-map "\C-cn" 'rgr/org-add-note-to-current-task)

(defun gjg/choose-agenda-file-set ()
  (interactive)
  (let* ((gort (completing-read "Choose an agenda file set: " (mapcar 'car gjg/agenda-file-sets)))
         (gort2 (assoc gort gjg/agenda-file-sets)))
    (setq org-agenda-files (cadr gort2)
          org-refile-targets '(( org-agenda-files :maxlevel . 3)))
    (message (format "org-agenda-files set to %s" org-agenda-files))))


(defun gjg/ec2-nab-hosts-from-cm ()
  "Take contents of copied hosts from Cloudera Manager Hosts page and pull out the hostnames"
  (interactive)
  (get-buffer-create "*gort temp*")
  (with-current-buffer "*gort temp*"
    (goto-char (point-min))
    (yank)
    (goto-char (point-min))
    (replace-regexp "^[ 	]+" "" nil (point-min) (point-max))
    (goto-char (point-min))
    (delete-non-matching-lines "^ip-")
    (goto-char (point-min))
    (replace-regexp "internal.+" "internal" nil (point-min) (point-max))
    (goto-char (point-max)))
  (switch-to-buffer "*gort temp*")
  ;; (find-file "/aws-director:cluster-nodes.txt")
  
  )


;; from github user coredump https://stackoverflow.com/questions/2088029/find-files-getting-a-dired-buffer-of-files-specified-by-filter-containing-text
(defun find-iname-grep-dired (dir pattern regexp)
  (interactive
   "DFind-name (directory): \nsFind-name (filename wildcard): \nsFind-grep (grep regexp): ")
  (find-dired dir (concat "-iname " (shell-quote-argument pattern) " "
                          "-type f -exec " grep-program " " find-grep-options " -e "
                          (shell-quote-argument regexp) " "
                          (shell-quote-argument "{}") " "
                          (shell-quote-argument ";"))))

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
