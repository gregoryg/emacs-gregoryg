;; *** Org-mode-hook



;;     Primarily used to set key strokes to my preference

;;     org-crypt-use-before-save-magic is called here because it has the
;;     side effect of adding to org-mode-hook

(require 'ox-confluence)
;; (require 'org-crypt)
;; (org-crypt-use-before-save-magic)
(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 90)
	    (org-defkey org-mode-map [(control tab)] 'org-next-link)
	    (org-defkey org-mode-map [C-S-iso-lefttab] 'org-previous-link)
	    ;; (org-defkey org-mode-map [C-c C-/] 'org-decrypt-entries)
	    ;; steal back my keys
	    (org-defkey org-mode-map [home] 'beginning-of-buffer)
	    (org-defkey org-mode-map [end] 'end-of-buffer)
	    ;; (local-set-key (kbd "C-c TAB") 'org-next-link)
	    ;; (local-set-key (kbd "S C-c TAB") 'org-previous-link)
	    ;; and what the hell - turn on auto fill mode for every org file
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    ;; set sub/superscript interpretation OFFFFFFOOOFFF
	    (setq org-use-sub-superscripts nil)
            (setq org-catch-invisible-edits 'show-and-error )
            ))

;; *** Org capture
;; TODO: improve meeting template with
;;       + :NOBLOCKING: t
;;       + meeting|call

;; PLEASE SET USING CUSTOMIZATION (setq org-directory "~/Copy/projects/")


;; taken from StackOverflow answer https://stackoverflow.com/questions/11902620/org-mode-how-do-i-create-a-new-file-with-org-capture
;; (defun capture-blog-filename (path)
(defun capture-blog-filename ()
  (let ((thedate (org-read-date nil nil nil "Date of post: "))
        (name (read-string "Name of blog post file description: ")))
    (expand-file-name (format "%s-%s.md"
                              (format-time-string "%Y-%m-%d")
                              name) "~/projects/jekyll/gregorygrubbs/_posts/")))


(setq org-default-notes-file "~/projects/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(
        ("b" "Blog post" plain
         (file capture-blog-filename)
         "---
layout: post
title:  %^{title|Generic Post}
date:   %<%F %T %z>
categories: jekyll update
---

# some thoughts"
         )
        ("r" "Regular todo" entry
	 (file+headline "~/todos.org" "General")
	 "* TODO %? \nSCHEDULED: %^T\n:LOGBOOK:\n:CREATED:%U\n:END:\n%i\n " :prepend nil :time-prompt t)
	("p" "Mesosphere Phone/Meeting" entry
	 (file+headline "~/mesosphere.org.gpg" "Calls and Meetings Log")
	 "* %t %^{type|Call|Meeting} with %^{with|Unknown|Kirk Marty|Nick Kane|Jerry Connors}: %^{Subject|Sync-up|Follow-up|Team|Presentation|Introduction}
:PROPERTIES:
:NOBLOCKING: t
:END:
:LOGBOOK:
:CREATED:%U
:END:
%i
   + From Mesosphere: GG, 
   + From %\\2: 
   + %?
 " :prepend t :clock-in t :clock-resume t)

        
	("w" "todo With clip" entry
	 (file+headline "~/projects/notes.org" "Tasks")
	 "* TODO %?%c\n:LOGBOOK:\n:CREATED:%U\n:END:\n\n%i\n" :prepend t)
	("b" "bookmarks" entry
	 (file+headline "~/projects/notes.org" "Bookmarks")
	 "* %c\n%i\n %u" :prepend t)
	("v" "Vocabulary" entry
	 (file+headline (concat org-directory "/vocab.org")
			"Vocabulary")
	 "* %^{The word} :drill:\n %t\n %^{Extended word (may be empty)} \n** Answer \n%^{The definition}")
	))

;; *** Org-mode BIG DUMP


;; persist clock history across emacs sessions
(defcustom gjg/agenda-file-sets
  '(("Cloudera"
     '("cloudera.org.gpg" "misc.org"))
    ("Mesosphere"
     '("mesosphere.org.gpg"))
    ("Home"
     '("misc.org" "notes.org" "projects.org")))
  "Named sets of agenda files"
  :group 'gjg)

;; ease export/sharing from org-mode
(defun gjg/org-export-to-odt ()
    "Export to odt without the theme interfering"
    (interactive)
    (let ((gort custom-enabled-themes))
      (mapcar 'disable-theme custom-enabled-themes)
      (org-odt-export-to-odt)
      (mapcar 'enable-theme gort)))

(defun gjg/org-export-to-html-and-open ()
  "Export to HTML with no theme active"
  (interactive)
  (let ((gort custom-enabled-themes))
    (mapcar 'disable-theme custom-enabled-themes)
    (org-html-export-to-html)
    (browse-url (browse-url-file-url (concat (file-name-sans-extension (buffer-file-name)) ".html")))
    (mapcar 'enable-theme gort)
    ))


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-report-include-clocking-task t)
(setq org-agenda-files '("~/mesosphere.org.gpg"))

(setq org-use-speed-commands t) ;; POWER USER BABY
(setq org-speed-commands-user 
      '(
        ("," . org-columns)
        ("q" . bury-buffer)))
;; DAN DAVISON nifty speed commands
(defun ded/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun ded/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(add-to-list 'org-speed-commands-user
             '("]" ded/org-show-next-heading-tidily))
(add-to-list 'org-speed-commands-user 
             '("[" ded/org-show-previous-heading-tidily))

;; END DAN DAVISON nifty speed commands
(setq org-export-latex-tables-centered nil)
(setq org-read-date-prefer-future 'time)
(setq org-clock-sound "~/projectsc136.wav")
;; make c-y pay attention to current level
(setq org-yank-adjusted-subtrees t)
(setq org-yank-folded-subtrees nil)
(setq org-insert-heading-respect-content t); new headings go AFTER CONTENT, even from the middle of content area
(setq org-indirect-buffer-display 'current-window)
(setq org-reverse-note-order t) ; notes go at the top
;;(global-set-key (kbd "C-c TAB") 'org-cycle-agenda-files)
;; Set basic org-mode keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-show-inherited-tags t)
(setq org-agenda-hide-tags-regexp "noagenda")
(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))
(setq org-completion-use-ido nil)
(setq org-completion-use-iswitchb nil)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;; use S-arrow without having to log state change!
;;(setq org-log-state-notes-into-drawer t) ;;
(setq org-log-into-drawer t)
(setq org-cycle-include-plain-lists nil) ; nil, t, or 'integrate
(setq org-alphabetical-lists t) ;; allow alphabetical lists
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-entry-text-maxlines 10)
;; (setq org-agenda-diary-file "~/projectsdiary.org")
;; (setq org-agenda-include-diary t)
;; (require 'cal-tex)
;; (setq cal-tex-diary t) ;; show diary entries in the calendar (used in org2hpda)

;; set NO XML preamble on export
(setq org-export-html-xml-declaration 
      (quote (("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))


(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 10)
        (htmlize-output-type 'css)))

;;(setq org-agenda-start-with-clockreport-mode t)
(setq org-special-ctrl-a/e t)
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 99))
(setq org-cycle-open-archived-trees nil)
(setq org-cycle-separator-lines 1) ;
(setq org-clock-history-length 12)
(setq org-enforce-todo-dependencies t)
(setq org-tag-alist '(("Project" . ?p)
                      ("NEXT" . ?n)
                      ("noexport" . ?x)
		      ("cloud" . ?c)
                      ("customer")
		      ("Hadoop" . ?h)
		      ("m&a" . ?m)
                      ))
;; Projects are identified by a :Project: tag unless they are marked DONE;
;;   Any project must have one sub-task identified by :NEXT: to be considered un-stuck
;;   MAYBE: exclude special tags like Someday/Maybe
(setq org-stuck-projects '("Project|project/-DONE"
                           nil
                           ("NEXT")
                           ""))
(setq org-columns-default-format "%TODO %CATEGORY %60ITEM(Task)%5Effort(Estim){:} %5CLOCKSUM(Clock) %SCHEDULED(Time) %DEADLINE")
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)")      ;; ordinary sequence is Todo -> Started -> Done
                                (sequence "PROPOSED(p)" "WAITING(w@/!)" "MAYBE(m!)" "OPEN(O@)" "|" "CANCELLED(c@/!)") ;; "oddball" states that any task may be set to
                                ;; (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")     ;; sequence for quotes
                                )))
(setq org-todo-keyword-faces
      '(("TODO"
         (:foreground "#ff39a3" :weight bold))
        ("STARTED" . "#E35DBF")
        ("CANCELLED" :foreground "green" :weight bold :strike-through t)
        ("PROPOSED" . "pink")
        ("WAITING" . "yellow")))

;; (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
;;                                      ("STARTED" :foreground "deep sky blue" :weight bold)
;;                                      ("DONE" :foreground "forest green" :weight bold)
;;                                      ("WAITING" :foreground "orange" :weight bold)
;;                                      ("SOMEDAY" :foreground "magenta" :weight bold)
;;                                      ("CANCELLED" :foreground "forest green" :weight bold)
;;                                      ("QUOTE" :foreground "red" :weight bold)
;;                                      ("QUOTED" :foreground "magenta" :weight bold)
;;                                      ("APPROVED" :foreground "forest green" :weight bold)
;;                                      ("EXPIRED" :foreground "forest green" :weight bold)
;;                                      ("REJECTED" :foreground "forest green" :weight bold)
;;                                      ("OPEN" :foreground "blue" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("STARTED" ("WAITING") ("NEXT" . t)))))
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

(setq org-tags-exclude-from-inheritance '("Project" "project" "interview2"))    
                                        ;(setq org-tag-alist '(("project" . ?p)("car" . ?c)))
;; navigate to files using org-refile; use C-u C-c C-w
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-targets '(( org-agenda-files :maxlevel . 3))
;;       org-refile-use-outline-path 'file
;;       org-refile-target-verify-function
;;       (lambda ()
;;         (not (member "ARCHIVE" (org-get-tags)))))

(defadvice org-refile (around org-refile-pseudo-hook )
  "Turn ido flex matching off for org-refile"
  (let ((ido-enable-flex-matching nil))
    ad-do-it))
(ad-activate 'org-refile)

(defun gjg/org-refile-goto ()
  "Call org-refile with arg to visit an org-mode location. A convenience function to bind to a global key."
  (interactive)
  (org-refile 1))

;;*** Org-mobile
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;;    (setq org-mobile-directory "/gregj@smeagol.hoochiepep.home:/home/gregj/work/org/web")
(setq org-mobile-inbox-for-pull "~/projectsfrom-mobile.org")
(setq org-agenda-before-write-hook nil)
(setq org-mobile-files '( "~/Copy/ingress/ingress.org" 'org-agenda-files))
;; (setq org-mobile-agendas '("P" "S" "w"))
;;***  [ .emacs remember setup ]

;; (global-set-key (kbd "C-c p") 'my-phone-call)

;; (defvar gjg/remember-phone-record nil
;;   "Either BBDB record vector, or person's name as a string, or nil")

;; (defun my-phone-call ()
;;   (interactive)
;;   (let* ((org-remember-templates my-phone-remember-templates)
;;          (myname (completing-read "Who is calling: " (bbdb-hashtable) 'bbdb-completion-predicate 'confirm))
;;          (my-bbdb-name (if (> (length myname) 0) myname nil)))
;;     (setq gjg/remember-phone-record 
;;           (or (first (bbdb-search (bbdb-records) my-bbdb-name  nil nil))
;;               myname))
;;     (other-window 1)
;;     (org-remember)))

;; (defun gjg/bbdb-name ()
;;   "Return full name of saved bbdb record, or empty string - for use in Remember templates"
;;   (if (and gjg/remember-phone-record (vectorp gjg/remember-phone-record))
;;       (bbdb-record-name gjg/remember-phone-record)
;;     (or gjg/remember-phone-record "")))

;; (defun gjg/bbdb-company ()
;;   "Return company of saved bbdb record, or empty string - for use in Remember templates"
;;   (if (and gjg/remember-phone-record (vectorp gjg/remember-phone-record))
;;       (or (bbdb-record-company gjg/remember-phone-record) "")
;;     ""))

;; (defun my-start-clock-if-needed ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (re-search-forward ":CLOCK-IN:" nil t)
;;       (replace-match "")
;;       (org-clock-in))))
;; (add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

;; (require 'remember)
;; (global-set-key (kbd "C-M-r") 'org-remember)
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)


(setq org-agenda-show-inherited-tags t)
(setq org-agenda-custom-commands
      (quote (("P" "Projects" tags "Project|project|PROJECT" ((org-use-tag-inheritance nil)))
              ("S" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("dc" "Clock report" agenda ""
               ((org-agenda-ndays 1)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled 'deadline 'scheduled))))
              ("dw" "Weekly clock report" agenda ""
               ((org-agenda-ndays 7)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled 'deadline 'scheduled))))
              ("da" "GORT Agenda"
               ((agenda "")
                (todo "STARTED")))
              )))

;; org-agenda: some handy keys
;;(define-key org-agenda-mode-map "o" 'org-agenda-show)

;; you might also want to set:
;;   (setq org-agenda-skip-unavailable-files t)
;; so these warnings won't annoy the little remember-frame
;; also: I have noted infrequent problems when using ElScreen
;;  (the wrong frame might be chosen for Remember in about 10% of the cases)

;; (defun popup-remember-frame ()
;;   "turn the current frame into a small popup frame for remember mode;
;; this is meant to be called with
;;      emacsclient -c -e '(popup-remember-frame)'"
;;   (modify-frame-parameters nil
;;                         '( (name . "*Remember*") ;; must be same as in mode-hook below
;;                            (width .  80)
;;                            (height . 16)
;;                            (vertical-scroll-bars . nil)
;;                            (menu-bar-lines . nil)
;;                            (tool-bar-lines . nil)))
;;   (org-remember nil ?w)
;;   (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only
;;   (delete-other-windows))

;; when we're in such a remember-frame, close it when done.
;; (add-hook 'org-remember-mode-hook
;;        (lambda()
;;          (define-key org-remember-mode-map (kbd "C-c C-c")
;;            (lambda()(interactive)
;;               (let ((remember-frame-p
;;                      (string= (frame-parameter nil 'name) "*Remember*")))
;;                 (when remember-frame-p (make-frame-invisible))  ;; hide quickly

;;                 (org-remember-finalize)
;;                 (when remember-frame-p (delete-frame)))))))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (hl-line-mode 1)
            ;; undo the horrendous "o" key binding (bound to 'delete-other-windows)
            ;;       (local-unset-key "o")
            ;; rebind to a sweet and beautiful function
            (org-defkey org-agenda-mode-map "o" 'org-agenda-show)
            (local-unset-key "a") ;; don't let this accidentally archive a tree!
            ))
(add-hook 'org-export-preprocess-hook
          (lambda ()
            (org-dblock-update 2) ;; update all dynamic blocks in the buffer
            ))
;; Function stolen from Bernt Hansen on gmane.emacs.orgmode: insert inactive time stamp for new entries
(defun bh/insert-inactive-timestamp ()
  (interactive)
  (save-excursion
    (insert "\n")
    (org-cycle)
    (org-insert-time-stamp nil t t nil nil nil)))

;; (add-hook 'org-insert-heading-hook 'bh/insert-inactive-timestamp)


;; (add-hook 'outline-minor-mode-hook
;;           'th-outline-minor-mode-init)

(global-unset-key [(meta tab)])
(define-key esc-map "\t" 'org-cycle)
(global-set-key [(meta tab)] 'org-cycle)
(global-set-key [M-left] 'hide-body)
(global-set-key [M-right] 'show-all)
(global-set-key [M-up] 'outline-previous-heading)
(global-set-key [M-down] 'outline-next-heading)
(global-set-key [C-M-left] 'hide-sublevels)
(global-set-key [C-M-right] 'show-children)
(global-set-key [C-M-up] 'outline-previous-visible-heading)
(global-set-key [C-M-down] 'outline-next-visible-heading)


;; delete the seleted region when something is typed
(delete-selection-mode -1)

;; **** org-collector



;; *** Agenda prefix format control


(setq org-agenda-format-date
      (concat "%Y-%m-%d %a "
              (make-string (- (window-width) 15) (string-to-char "_"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With this typing "L" in agenda and todo buffers allows toggling
;; whether category/file names appear or not at the left or entries in
;; agenda/todo listings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar my-org-agenda-list-category t)
;; (defun my-org-agenda-toggle-list-category ()
;;   "Toggles whether category/file name appears or not at the left
;; of entries in agenda listings. Useful to unclutter listings."
;;   (interactive)
;;   (if my-org-agenda-list-category
;;       (progn 
;;         (setq my-org-agenda-list-category nil)
;;         (setq org-agenda-prefix-format
;;               '((agenda  . "  %-12:c%?-12t% s")
;;                 (timeline  . "  % s")
;;                 (todo  . "  %-12:c")
;;                 (tags  . "  %-12:c")
;;                 (search . "  %-12:c")))
;;         )
;;     (setq my-org-agenda-list-category t)
;;     (setq org-agenda-prefix-format
;;           '((agenda  . "  %?-12t% s")
;;             (timeline  . "  % s")
;;             (todo  . "  ")
;;             (tags  . "  ")
;;             (search . "  ")))
;;     )
;;   (org-agenda-redo))

;; (add-hook 
;;  'org-mode-hook
;;  (lambda ()
;;    (define-key org-agenda-keymap   "L" 'my-org-agenda-toggle-list-category)
;;    (define-key org-agenda-mode-map "L" 'my-org-agenda-toggle-list-category)
;;    ))

;; *** Ryan Thompson's cycling code


(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil."
  (and (looking-at "[ \t]*$")
       (save-excursion
         (beginning-of-line 1)
         (looking-at (concat "^\\(\\*+\\)[ \t]+\\(" org-todo-regexp "\\)?[ \t]*")))))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defvar org-previous-line-level-cached nil)

(defun org-recalculate-previous-line-level ()
  "Same as `org-get-previous-line-level', but does not use cached
value. It does *set* the cached value, though."
  (set 'org-previous-line-level-cached
       (let ((current-level (org-current-level))
             (prev-level (when (> (line-number-at-pos) 1)
                           (save-excursion
			     (forward-line -1)
                             (org-current-level)))))
         (cond ((null current-level) nil) ; Before first headline
               ((null prev-level) 0)      ; At first headline
               (prev-level)))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the
current line. Returns 0 for the first headline in the buffer, and
nil if before the first headline."
  ;; This calculation is quite expensive, with all the regex searching
  ;; and stuff. Since org-cycle-level won't change lines, we can reuse
  ;; the last value of this command.
  (or (and (eq last-command 'org-cycle-level)
           org-previous-line-level-cached)
      (org-recalculate-previous-line-level)))

(defun org-cycle-level ()
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (org-point-at-end-of-empty-headline)
      (setq this-command 'org-cycle-level) ;Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (loop repeat (/ (- prev-level 1) (org-level-increment))
                do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
                do (org-do-promote))))
        t))))

;; *** Bernt Hansen's scheduled TODO hiding



;; #+BEGIN_QUOTE
;; I remove entries from the global TODO lists using the variables
;; org-agenda-todo-ignore-scheduled, org-agenda-todo-ignore-deadlines,
;; org-agenda-todo-ignore-with-date

;; This leaves SCHEDULED and DEADLINE items only on the daily agenda
;; view.

;; So to see deadlines in the warning period I just go to the agenda for
;; today (C-c a a) and it shows up.

;; From Matt Lundin:
;; (setq org-agenda-todo-ignore-deadline 'near)

;; With this setting, your items with a deadline will not show up in your
;; TODO list until they are within the warning period.


;; #+END_QUOTE

(setq org-agenda-todo-ignore-scheduled nil)
(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-todo-ignore-with-date nil)

(setq org-publish-project-alist
      '(
        ("director-23-testing" :components("director-23-testing-www" "director-23-testing-static"))
        ("director-23-testing-www"
         :base-directory "~/Google Drive/testing/director/2.3"
         :base-extension "org"
         :publishing-directory "/rsync:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/testing/director/2.3/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         )
        ("director-23-testing-static"
         :base-directory "~/Google Drive/testing/director/2.3"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|dxt\\|dxj\\|sdk\\|txt\\|conf"
         :publishing-directory "/rsync:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/testing/director/2.3/"
         :recursive t
         :publishing-function org-publish-attachment)
	("ted-talks"
	 :components
	 ("ted-talks-notes" "ted-talks-static"))
	("ted-talks-notes"
	 :base-directory "/c/Copy/projects/programming/ted"
	 :base-extension "org"
	 :publishing-directory "/rsync:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/ted"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4)  ;; default for this project
	("ted-talks-static"
	 :base-directory "/c/Copy/projects/programming/ted"
	 :base-extension "html"
	 :publishing-directory "/rsync:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/ted"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("syncsort-pm2015-notes"
	 :base-directory "~/syncsort/db/pm/syncsort-pm2015"
	 :base-extension "org"
	 :publishing-directory "/cygssh:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/syncsort-pm2015"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4  ;; default for this project
	 :html-head "<style>.timestamp {color: #ff0033;} .notes-image img {width: 60%;} body {counter-reset: section;} h4 {counter-increment: section; color: auto;} h4:before { counter(section) " ";}</style><style>html {font-family: Georgia, "Times New Roman", Times, serif; font-size: large;}</style>"
	 )
	("syncsort-pm2015-static"
	 :base-directory "~/syncsort/db/pm/syncsort-pm2015/images"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|dxt\\|dxj\\|sdk\\|txt"
	 :publishing-directory "/cygssh:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/syncsort-pm2015/images"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("syncsort-pm2015"
	 :components
	 ("syncsort-pm2015-notes" "syncsort-pm2015-static"))
	("hadoop-howtows-notes"
	 :base-directory "~/syncsort/db/docs/hadoop/howto"
	 :base-extension "org"
	 :publishing-directory "/plink:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/private/joins"
	 :recursive t
	 :publishing-function org-publish-org-to-html
	 :headline-levels 4  ;; default for this project
	 )
	("hadoop-howtos-static"
	 :base-directory "~/syncsort/db/docs/hadoop/howto"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|dxt\\|dxj\\|sdk\\|txt"
	 :publishing-directory "/plink:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/private/joins"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("hadoop-howtos"
	 :components
	 ("hadoop-howtos-notes" "hadoop-howtos-static"))
	("syncsort-pm-blog-posts"
	 :base-directory "~/syncsort/db/pm"
	 :base-extension "org"
	 :publishing-directory "/plink:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/blog"
	 :recursive nil
	 :with-toc nil
	 :section-numbers nil
	 :with-todo-keywords nil
	 :publishing-function org-html-publish-to-html
	 )
	("blog-static"
	 :base-directory "~/syncsort/db/pm"
	 :recursive t
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|dxt\\|dxj\\|sdk\\|txt"
	 :publishing-directory "/plink:gortsleigh@hoochiepep.com:/home/gortsleigh/dynapse.net/blog"
	 :publishing-function org-publish-attachment
	 )
	("syncsort-blog"
	:components ("syncsort-pm-blog-posts" "blog-static"))
	))

;; *** Sad Panda (things set because something doesn't work right)


;; show only current/today's/all time in the clock task modeline
;; this SHOULD be settable on a file basis by using property CLOCK_MODELINE_TOTAL
(setq org-clock-modeline-total 'current)

