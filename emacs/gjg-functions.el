(defun gjg/ssh-abbrevs-for-ec2-cluster ()
  "Take text of instances pasted copied from EC2 and add to ~/.ssh/config; expect copied text to be in current clipboard"
  (get-buffer-create "*gort temp*")
  (with-current-buffer "*gort temp*"
    (goto-char (point-min))
    (yank)
    (goto-char (point-min))
    (delete-non-matching-lines "^[0-9][0-9\.]+")
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

(defun coursera-slide-to-orgmode ()
  "Change Coursera.org slides in .Rmd markdown to org-mode format.  Expects entire (narrowed) buffer to be the slides"
  (interactive)
  ;; replace Markdown bullets to org-mode
  (replace-regexp "^##" "**" nil (point-min) (point-max))
  ;; format R code blocks as org-mode
  (replace-regexp "^```{?r}?" "  #+BEGIN_SRC R" nil (point-min) (point-max))
  (replace-regexp "^```" "  #+END_SRC" nil (point-min) (point-max))
  ;; replace markdown code indicator
  (replace-string "`" "=" nil (point-min) (point-max))
  ;; get rid of the damned curly quotes
  (replace-smart-quotes (point-min) (point-max)) ;; do last
  )

(defun gjg/org-drill-set-cloze1 ()
  "Set current question as 'cloze1' type"
  (interactive)
  (insert ":PROPERTIES:\n:DRILL_CARD_TYPE: hide1cloze\n:END:\n")
)
;; (require 'sql)

;; (defun gjg/parse-wp-config-db (wpconfig-path)
;;   "Read in and parse the DB settings from a WordPress config file; binds 'global' vars for use by sql-mode"
;;   (save-excursion ;; will restore current buffer and default dir afterwards
;;     (set-buffer (get-buffer-create (generate-new-buffer-name " wp-config.php")))
;;     (insert-file-contents wpconfig-path)
;;     ;; in regex: subexpr 1 is variable name, subexpr 3 is value: DB_{HOST,NAME,PASSWORD,USER}
;;     (while (search-forward-regexp "define\s*(\s*['\"]\\(DB_\\(HOST\\|NAME\\|PASSWORD\\|USER\\)\\)['\"]\s*,\s*['\"]\\([^'\"]*\\)['\"]\s*)" (point-max) 42   )
;;       (let ((mykey (match-string-no-properties 1))
;; 	    (myval (match-string-no-properties 3)))
;; 	(cond 
;; 	 ((equal "DB_HOST" mykey)
;; 	  (setq sql-server myval))
;; 	 ((equal "DB_NAME" mykey)
;; 	  (setq sql-database myval))
;; 	 ((equal "DB_PASSWORD" mykey)
;; 	  (setq sql-password myval))
;; 	 ((equal "DB_USER" mykey)
;; 	  (setq sql-user myval)))))
;;     (kill-buffer )))


;; (defun gjg/sql-mysql-wordpress ()
;;   "Find WordPress config file in current tree, log into WP database if found."
;;   (interactive)
;;   (let ((mypath (locate-dominating-file default-directory "wp-config.php")))
;;     (if mypath
;; 	(progn
;; 	  (gjg/parse-wp-config-db (concat mypath "wp-config.php"))
;; 	  (pop-to-buffer (sql-connect-mysql)) ;; emacs <24 syntax
;; 	  ;; (pop-to-buffer (sql-connect-mysql 'mysql nil)) ;; emacs 24 syntax
;; 	  (setq sql-interactive-product 'mysql)
;; 	  (setq sql-buffer (current-buffer))
;; 	  (sql-interactive-mode)
;; 	  (let* ((match (string-match (nth 0 tramp-file-name-structure) mypath))
;; 		 (myformat (if (eq nil match) 
;; 			       (format " WordPress: local; %s; dbhost %s " 
;; 				       mypath
;; 				       sql-server
;; 				       )
;; 			     (format " WordPress: Remote %s@%s %s; dbhost %s "
;; 				     (match-string (nth 2 tramp-file-name-structure) mypath)
;; 				     (match-string (nth 3 tramp-file-name-structure) mypath)
;; 				     (match-string (nth 4 tramp-file-name-structure) mypath)
;; 				     sql-server))))
;; 	    (setq header-line-format myformat))
;; 	  )
;;       (message "Did not find wp-config.php in current path"))
;;     ))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; swiped from Gilaras Drakeson <gilaras@gmail.com>
(require 'dired)
(defcustom gjg/os-open "xdg-open"
  "Command to open a document, e.g. 'xdg-open' on Linux, 'open' on OS X, 'explorer' or 'cygpath' on Windows"
  :type 'string
  :group 'gjg)


(defun dired-open (&optional file-list)
  (interactive
   (list (dired-get-marked-files t current-prefix-arg)))
  (progn
    (message (format "Calling %s %s" gjg/os-open file-list))
    (apply 'call-process gjg/os-open nil 0 nil file-list)))
(define-key dired-mode-map (kbd "C-;") 'dired-open)


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

(defcustom gjg/visual-notify-command "mumbles-send"
  "Location of Growl/Snarl/Mumblles notification command. Order of arguments: Header Text {IconPath}" 
  :type 'string
  :group 'gjg)

(defcustom gjg/visual-notify-options ""
  "Options for Growl/Snarl/Mumbles-like notification command"
  :type 'string
  :group 'gjg)

(defcustom gjg/say-command "flite"
  "Location of text-to-speech command"
  :type 'string
  :group 'gjg)

;; (defun gjg/sanitize (text)
;;   "sanitize text a la WordPress"
;;   (replace-regexp-in-string 
;;    "['\"\&]" "" 
;;    (replace-regexp-in-string 
;;     "[ 	\+_]+" "-"
;;     (downcase text))))

;; (defun gjg/whitespace-to-underscore (text)
;;   "Yep"
;;   (replace-regexp-in-string "[ ]+" "_" text)
;;   )

;; (defun gjg/namespace ()
;;   ""
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (re-search-forward "^[ ]+Namespace:[ ]+\\([^ \n]+\\)" (point-max) t)
;;       (match-string-no-properties 1))
;;     ))

;; (defun gjg/namespace-with-prefix ()
;;   ""
;;   (let ((ns (gjg/grab-namespace)))
;;     (when ns (concat ns "_"))))

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

;; (defun gjg/wp-namespace ()
;;   (save-excursion
;;     (point-min)
;;     (re-search-forward "^[ ]*Namespace:[ ]+\\([^ \n]+\\)" (point-max) t)
;;     (match-string 1)
;;     )
;;   )

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

;; php lint, modified from a comment on saca chua's blog
;;(load-library "compile.el")
;; (pushnew '(php "syntax error.* in \\(.*\\) on line \\([0-9]+\\)$" 1 2)
;;          compilation-error-regexp-alist-alist)

;; (setq compilation-error-regexp-alist
;;       (append (list 'php) compilation-error-regexp-alist))

;; (defun php-lint ()
;;   "Performs a PHP lint-check on the current file."
;;   (interactive)
;;   (string-match (car tramp-file-name-structure) (buffer-file-name))
  
;;   (let ((localpath (if (string-match (car tramp-file-name-structure) (format "%s" (buffer-file-name)))
;; 		       (match-string-no-properties 8 (buffer-file-name))
;; 		     (buffer-file-name))))
;;     (if localpath (compile (concat "php -l -f \"" localpath "\"")))))

;; (add-hook 'php-mode-hook 
;; 	  '(lambda ()
;; 	     (define-key php-mode-map "\C-c\C-l" 'php-lint)))
;; ;;(define-key osx-key-mode-map "\C-x~" 'previous-error)

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

(require 'bookmark) ;; load so that bookmark-alist will be available
(defun gjg/ido-bookmark-jump ()
  (interactive)
  (let ((readfunc (if (functionp 'ido-completing-read) 'ido-completing-read 'completing-read)))
    (bookmark-maybe-load-default-file)
    (bookmark-jump 
     (funcall readfunc "Jump to bookmark: " (mapcar 'car bookmark-alist)))
    ))
(global-set-key (kbd "C-x rb") 'gjg/ido-bookmark-jump)

;; lol pseudocode ; api ref http://cheezburger.com/apidocs/ContentRetrieval.aspx
;; build url  http://api.cheezburger.com/xml/category/{category}/lol/random
;; categories: cats,objects,dogs,other animals,news,celebs,fail,engrish,comix
;; parse using xml-read from xml.el
;; use (assoc 'LolImageUrl (car myxmlparse))
;; keys are LolId, LolImageUrl, ThumbnailImageUrl, LolPageUrl, FullText, PictureId, PictureImageUrl, Title, Description, SourcePictures, TimeStamp

;; TRAMP SUDO FUN - snarfed from Peter Dyballa on gmane.emacs.help
(defun my-tramp-header-line-function ()
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (setq header-line-format
	  (format-mode-line "----- THIS BUFFER IS VISITED WITH ROOT PRIVILEGES -----"
			    'font-lock-warning-face))))

(add-hook 'find-file-hooks 'my-tramp-header-line-function)
(add-hook 'dired-mode-hook 'my-tramp-header-line-function)

(require 'vc)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; (defun gjg/generate-bogus-socialmedia-click ()
;;   (interactive)
;;   (let* ((campaigns (list 421 422 433))
;; 	 (types (list "impression" "click" "referral" "affiliate"))
;; 	 (networks (list "facebook" "twitter" "myspace" "digg" "foursquare" "brighkite" "linkedin"))
;; 	 (decades (list "0-10" "11-20" "21-30" "31-40" "41-50" "51-60" "61-70" "71-80" "81-90" "91-100" "101-110" "111-120"))
;; 	 (cities (list "san francisco" "denver" "telluride" "paris" "sydney" "los angeles" "sedona" "seattle" "new york city" "sao paolo"))
;; 	 (mystring (format "campaignId=%d&type=%s&referer=%d.%d.%d.%d&sex=%s&age=%s&city=%s%s%s"
;; 			   (nth (random (length campaigns)) campaigns)
;; 			   (nth (random (length types)) types)
;; 			   (random 129) (random 65) (random 129) (random 129)
;; 			   (nth (random 2) (list "m" "f"))
;; 			   (nth (random (length decades)) decades)
;; 			   (nth (random (length cities)) cities)
;; 			   (if (= 0 (random 2)) (concat "&primary-socnet=" (nth (random (length networks)) networks)) "")
;; 			   (if (= 0 (random 2)) (concat "&" (nth (random (length networks)) networks) "=" (int-to-string (+ 1000 (random 123456)))) "")
;; 			   )
;; 		   ))
;;     mystring))

;; (defun gjg/insert-bogus-socialmedia-clicks (&optional num)
;;   (interactive "NNumber of clicks to insert: ")
;;   (dotimes (idx num)
;;     (insert (concat (gjg/generate-bogus-socialmedia-click) "\n"))
;;     )
;;   )

;; (defun gjg/package-name ()
;;   "Return the package name in the current source file, or the null string if none found"
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let* ((package-found (re-search-forward "^\s*package\s+\\(.+\\)" (point-max) t))
;; 	   (package-name (if package-found (match-string-no-properties 1) "")))
;;       package-name)))


;; (defun gjg/package-name-to-directory ()
;;   "Find the package statement in current source file, if any, and return the package name as a relative directory path.  For example, com.sun.java becomes com/sun/java.  Returns the null string if no package statement is found."
;;   (let* ((myret "")
;; 	 (package-name (gjg/package-name)))
;;     (when (> (length package-name) 0)
;;       (with-temp-buffer
;; 	(insert package-name)
;; 	(goto-char (point-min))
;; 	(while (search-forward "." (point-max) t)
;; 	  (replace-match "/" nil t))
;; 	(setq myret (concat (buffer-string) "/"))))
;;     myret))

;; snarfed from gmane.emacs.help
(defun my-dired-multi-occur (string)
  "Search string in files marked by dired."
  (interactive "MList lines matching regexp: ")
  (require 'dired)
  (multi-occur (mapcar 'find-file (dired-get-marked-files)) string))


;; (require 'xml)
;; (defun gjg/cheezget (&optional category pic)
;;   (interactive)
;;   (unless category (setq category "cats"))
;;   (setq pic (if pic "picture" "lol"))
;;   (message (format "this is waht you getz: %s" category))
;;   (let ((url (format "http://api.cheezburger.com/xml/category/%s/%s/random" category pic))
;; 	(imagekey (if (string= pic "lol") 'LolImageUrl 'PictureImageUrl)))
;;     (message (format "Url is %s" url))
;;     (set-buffer (url-retrieve-synchronously url))
;;     (goto-char (point-min))
;;     (setq xml (xml-parse-region (point-min) (point-max)))
;;     (kill-buffer)
;;     (message (format "%s" xml))
;;     (message (format "Image URL is %s" (nth 2 (assoc 'LolImageUrl (car xml)))))

;;     (set-buffer (get-buffer-create "adasdasd"))
;;     (delete-region (point-min) (point-max))
;;     (setq header-line-format (nth 2 (assoc 'Title (car xml))))
;;     (insert-image (gjg/cheez-fetch-image (nth 2 (assoc imagekey (car xml)))))
    
;;     ))

;; (defun gjg/cheez-fetch-image (url)
;;   (let* ((data (save-excursion
;; 		 (set-buffer (url-retrieve-synchronously url))
;; 		 (progn (goto-char (point-min))
;; 			(delete-region (point-min) (search-forward "\n\n"))
;; 			(buffer-substring (point-min) (point-max)))))
;; 	 (myimage (create-image data nil t)))
;;     myimage))

(defcustom gjg/audioplayer "mplayer"
  "Audio player for this machine"
  :type 'string
  :group 'gjg)


;; (defun djcb-popup (title msg &optional icon sound)
;;   "Show a popup if we're on X, or echo it otherwise; TITLE is the title
;; of the message, MSG is the context. Optionally, you can provide an ICON and
;; a sound to be played"
;;   ;; (interactive)
;;   (when sound (shell-command
;; 	       (concat gjg/audioplayer " -really-quiet \"" sound "\" 2> /dev/null &")))
;;   (cond ((eq window-system 'x)

;;       (shell-command (concat "notify-send "
;; 			     (if icon (concat "-i " icon) "")
;; 			     " '" title "' '" msg "'") nil nil))
;; 	((eq window-system 'w32)
;; 	 (call-process-shell-command gjg/visual-notify-command nil 0 nil (shell-quote-argument title) (shell-quote-argument msg) (shell-quote-argument icon)))
;; 	(t
;; 	 ;; text only version
;; 	 (message (concat title ": " msg)))))
;; ;; the appointment notification facility
;; (setq
;;  appt-message-warning-time 15 ;; warn 15 min in advance

;;  appt-display-mode-line t     ;; show in the modeline
;;  appt-display-format 'window) ;; use our func
;; (appt-activate 1)              ;; active appt (appointment notification)
;; (display-time)                 ;; time display is required for this...

;; ;; update appt each time agenda opened

;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defcustom gjg/appointment-icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
  "Icon to display when announcing an appointment"
  :type 'string
  :group 'gjg)
(defcustom gjg/appointment-sound "/usr/share/sounds/ubuntu/stereo/service-login.ogg"
  "Sound to play when announcing an appointment"
  :type 'string
  :group 'gjg)

;; ;; our little façade-function for djcb-popup
;; (defun djcb-appt-display (min-to-app new-time msg)
;;   (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg 
;; 	      gjg/appointment-icon gjg/appointment-sound))
;; ;; (setq appt-disp-window-function (function djcb-appt-display))
;; (setq appt-disp-window-function 'djcb-appt-display)


;; (defun gjg/bp-transform ()
;;   "Turn BP app emailed stats into an org-mode table"
;;   (interactive)
;;   (replace-regexp "\\([a-zA-Z]+\s+[0-9]+,\s+[0-9]+\\)\s+\\([0-9]+:[^:]+\\):\s*\\(.*\\)\n\s*blood pressure\s+\\([0-9]+\\)\s*/\s*\\([0-9]+\\)\s+mmHg\s*\n\s*pulse\s*\\(.+\\)\s+bpm\s*\n\n" "|\\1|\\2|\\4|\\5|\\6|\\3|\n")
;;   (goto-char (point-min))
;;   (insert "|Date|Time|Systolic|Diastolic|Pulse bpm|Comment|\n|-\n")
;;   (org-mode-restart)
;;   )

;; (defun gjg/import-mycal (&optional csvpath)
;;   "Import a csv calendar file, like Thunderbird3 Lightning Calendar exports"
;;   (interactive)
;;   (gjg/mycal
;;    (ido-read-file-name "Calendar CSV file: " (file-name-directory (or csvpath ".")) (file-name-nondirectory (or csvpath "Home.csv")))))

;; (defun gjg/mycal (csvfile)
;;   "Create org-mode event entries from CSV calendar file"
;; 					;  calOutlookCSVImportExport.js
;;   (require 'csv)
;;   (let ((calpath csvfile)
;; 	(tmpbuf (get-buffer-create " *ical parse*"))
;; 	(mybuf (get-buffer-create "*ICS Calendar Snarf*"))
;; 	(mycal)
;; 	)
;;     (set-buffer tmpbuf)
;;     (delete-region (point-min) (point-max))
;;     (insert-file-contents calpath)
;;     (while (search-forward "\r" nil t) (replace-match "" nil t))
;;     (setq mycal (csv-parse-buffer t tmpbuf))
;;     (set-buffer mybuf)
;;     (insert "* Calendar snarf from " (file-name-nondirectory calpath) "\n")
;;     (dolist (myevt mycal)
;;       (let* ((subject  (cdr (assoc "Subject" myevt)))
;; 	     (location (cdr (assoc "Location" myevt)))
;; 	     (desc     (cdr (assoc "Description" myevt)))
;; 	     (stime    (cdr (assoc "Start Time" myevt)))
;; 	     (sdate    (cdr (assoc "Start Date" myevt)))
;; 	     (sdatel   (split-string sdate "[/-]"))
;; 	     (edate    (cdr (assoc "End Date" myevt)))
;; 	     (edatel   (split-string edate "[/-]"))
;; 	     (etime    (cdr (assoc "End Time" myevt)))
;; 	     (sorg     (format-time-string "<%Y-%m-%d %a %H:%M>"
;; 					   (date-to-time
;; 							(shell-command-to-string (format "date --date='%s-%s-%s %s'"
;; 											 (nth 2 sdatel) (nth 0 sdatel) (nth 1 sdatel)
;; 											 stime)))))
;; 	     (eorg (format-time-string "<%Y-%m-%d %a %H:%M>"
;; 				       (date-to-time
;; 						    (shell-command-to-string (format "date --date='%s-%s-%s %s'"
;; 										     (nth 2 edatel) (nth 0 edatel) (nth 1 edatel)
;; 										     etime)))))
;; 	     )
;; 	(insert (format "** %s %s--%s\n\t:PROPERTIES:\n\t:LOCATION: %s\n\t:END:\n%s\n" subject sorg eorg location desc))
;; 	))
;;     ))

;; ;; pull in Dynapse calendar for use in org-mode
;; (defun gjg/calendar ()
;;   (interactive)
;;   (let ((mybuf (get-buffer-create "*Google Calendar Snarf*")))
;;     (set-buffer mybuf)
;;     (delete-region (point-min) (point-max))
;;     (insert "* Calendar snarf\n")
;;     (shell-command "~/bin/gcalcli --details --nc --cal=gregory@dynapse.com agenda 02/01/2010 03/31/2010" mybuf)
;;     (goto-char (point-min))
;;     ;; (replace-regexp "^gort:\s*\\(.\\|\n\\)+?Length: \\([^,\n]+\\)" "** \\1 MYLENGTH: \\2\n")
;;     (while (search-forward-regexp "^\\(gort:\\)\s*\\(<[^>]+>\\)\\(.\\|\n\\)+?Length: \\([^,\n]+\\)" (point-max) 42)
;;       (let ((duration (match-string 4))
;; 	    (start (match-string 2)))
;; 	(replace-match "** " t t nil 1)
;; 	(replace-match "HELLO \\&" t nil nil 3)
;; 	(message (format "Start is %s, Length is %s" start duration)))

;;       ;; (while (re-search-forward "^gort:\\(.\\|\n\\)+?Length: \\([^,\n]+\\)" (point-max) nil)
;;       ;; ;; (while (search-forward-regexp "^gort:" (point-max) 42)
;;       ;;   (replace-match "** " t t nil 1))
;;       )))

;; (defun gjg/openscan-cec ()
;;   "Generate the form for the Open Scan Client Engagement Checklist (CEC).  
;; Prepare the form for use by the Dynapse ReFormed Plugin"
;;   (interactive)
;;   (require 'csv)
;;   (let ((mycec (csv-parse-buffer t))
;; 	(mybuf (get-buffer-create "* CEC *"))
;; 	(list-name nil)
;; 	(list-idx 0))
;;     (set-buffer mybuf)
;;     (delete-region (point-min) (point-max))
;;     (dolist (myline mycec)
;;       (let* ((item (cdr (assoc "ITEM" myline)))
;; 	     (type (cdr (assoc "AnswerType" myline)))
;; 	     (level (string-to-number (cdr (assoc "LEVEL" myline))))
;; 	     (reqfield (cdr (assoc "REQUIRED" myline)))
;; 	     (required (if (string= "N" reqfield) nil t))
;; 	     (req-css-class (if required "class=\"required\"" ""))
;; 	     (id (cdr (assoc "ID" myline)))
;; 	     (title nil))
;; 	;; doc title: ID 0, type nil, required 0 
;; 	;; section title: ID nil, type 0, required 0
;; 	;; question: must have ID, type (required will default to true/Y)
;; 	(cond 
;; 	 ((and (not title) (= 1 level))
;; 	  (setq title item))
;; 	 ((= 2 level)
;; 	  (insert (format "</fieldset>\n\n<fieldset>\n<legend>%s</legend>\n" item)))
;; 	 (t
;; 	  ;; types of question: Attachment, List, Text, TextArea, Yes/No
;; 	  (when (= 4 level) ;; indented "sub-questions"
;; 	    (insert "<span class=\"sub-question\">\n" ))
;; 	  (unless (or (string= "Checkbox" type) (string= "Checkbox/Other" type))
;; 	    (insert (format "<label for=\"%s\" %s>%s</label>\n" id req-css-class item)))
;; 	  (cond
;; 	   ((string= "Text" type)
;; 	    (insert (format "<input type=\"text\" name=\"%s\" id=\"%s\" %s /><br/>\n"
;; 			    id id req-css-class)))
;; 	   ((string= "List" type)
;; 	    ;; list of choices to follow
;; 	    (setq list-name id
;; 		  list-idx 0)
;; 	    (insert "<br/>\n"))
;; 	   ((or (string= "Checkbox" type) (string= "Checkbox/Other" type))
;; 	    (insert (format "<input type=\"checkbox\" name=\"%s\" id=\"%s\" value=\"%s\" /> <span> %s</span><br/>\n"  ;; no "required" class at this level
;; 			    list-name list-name list-idx item))
;; 	    (setq list-idx (+ 1 list-idx)))
;; 	   ((string= "TextArea" type)
;; 	    (insert (format "<textarea name=\"%s\" id=\"%s\" %s></textarea>\n" id id req-css-class)))
;; 	   ((string= "Yes/No" type)
;; 	    (insert (format "<input type=\"radio\" name=\"%s\" value=\"yes\" %s/> Yes<br/>\n
;;                              <input type=\"radio\" name=\"%s\" value=\"no\"  %s/>  No<br/>\n"
;; 			    id req-css-class id req-css-class)))
;; 	   ((string= "Attachment" type) ;; never make this required
;; 	    (insert (format "<input type=\"file\" name=\"%s\" id=\"%s\"/>\n" id id))))
;; 	  (when (= 4 level) ;; indented "sub-questions"
;; 	    (insert "</span>\n")))
;; 	  )))
;;     (insert "</fieldset>\n" )))

(defun rgr/org-add-note-to-current-task ()
  (interactive)
  (save-window-excursion
    (if(org-clock-is-active)
	(org-clock-goto)
      (org-clock-goto t))
    (org-narrow-to-subtree)
    (org-add-note)))

(define-key global-map "\C-cn" 'rgr/org-add-note-to-current-task)

;; (defun gjg/org-table-export-to-buffer (&optional format)
;;   "Wrap function org-table-export to write a temporary file, then read that file into a buffer and display the buffer"
;;   (interactive)
  
;;   )

;; (defcustom gjg/agenda-file-sets '(("Open Scan"
;; 				   ("~/My Documents/My Dropbox/openscan/openscan.org" "~/My Documents/My Dropbox/openscan/howto.org" "~/My Documents/My Dropbox/openscan/openscan-cal.org" "~/projects/misc.org" "~/projects/fifa-worldcup-2010.org"))
;; 				  ("Home"
;; 				   ("~/projects/misc.org" "~/projects/notes.org" "~/projects/habits.org" "~/projects/next-work.org")))
;;   "A try at it"
;;   :group 'gjg
;;   :type '(alist :key-type string :value-type list))


(defun gjg/choose-agenda-file-set ()
  (interactive)
  (let* ((gort (completing-read "Choose an agenda file set: " (mapcar 'car gjg/agenda-file-sets)))
	 (gort2 (assoc gort gjg/agenda-file-sets)))
    (setq org-agenda-files (cadr gort2)
	  org-refile-targets '(( org-agenda-files :maxlevel . 3)))
    (message (format "org-agenda-files set to %s" org-agenda-files))))


;; (defun gjg/adhoc-xml-narrow ()
;;   "Try to create an indirect buffer and narrow it to a quoted XML string.  Usfeul for Open Scan DR config."
;;   (interactive)
;;   ;; make sure we are not already in an indirect buffer

;;   (cond ((buffer-base-buffer)
;; 	 (error "This is already an indirect buffer"))
;; 	(t     
;; 	 (message "Cool beans"))
;; 	))
(defun gjg/convert-teradata-sql ()
  "Generate a Syncsort DMExpress delimited record layout from SQL"
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (let ((mybuff (get-buffer-create "*DMX Delimited Record Layout*")))
    (copy-region-as-kill (point-min) (point-max))
    (set-buffer mybuff)
    (delete-region (point-min) (point-max))
    (yank)
    (goto-char (point-min))
    ;; (search-forward-regexp "CREATE\s+SET\s+TABLE\s+[A-Z]+\\.\\([A-Z0-9]+\\)" (point-max) 42)
    ;; (goto-char (point-min))
    (insert "/DELIMITEDRECORDLAYOUT\nI\"layout_TABLENAME\"\n{")
    ;; (insert (match-string 1))
    (goto-char (point-max))
    (insert "\n}\n")
    (goto-char (point-min))
    ;; translate NOT NULL
    (while (search-forward "NOT NULL" nil t)
      (replace-match "notnullable" t t))
    (goto-char (point-min))
    ;; be rid of things not needed for parsing
    (while (search-forward-regexp "\\(COMPRESS\\|DEFAULT CURRENT_TIMESTAMP([0-9]+)\\|TITLE\s+'[^\']+'\\)" (point-max) 42)
      (replace-match "" t t))
    ;; convert CHAR and VARCHAR
    (goto-char (point-min))
    (while (search-forward-regexp "\\(VAR\\)?CHAR\s*([0-9]+)\s+CHARACTER\s+SET\s+LATIN\s+NOT\s+CASESPECIFIC" (point-max) 42)
      (replace-match "character" t t))
    ;; convert DATE-only formats
    (goto-char (point-min))
    (while (search-forward "DATE FORMAT 'YYYY-MM-DD'" nil t)
      (replace-match "datetime (YEAR-MM0-DD0)" t t))
    ;; convert TIME-only formats 
    (goto-char (point-min))
    (while (search-forward "FORMAT '99:99:99'" nil t)
      (replace-match "datetime (HH0:MI0:SE0)" t t))
    ;; convert timestamps
    (goto-char (point-min))
    (while (search-forward-regexp "TIMESTAMP([0-9]+)" (point-max) 42)
      (replace-match "datetime (YEAR-MM0-DD0 HH0:MI0:SE0)" t t))
    ;; convert DECIMAL
    (goto-char (point-min))
    (while (search-forward-regexp "DECIMAL\s*([0-9 ,]+)" (point-max) 42)
      (replace-match "en" t t))
    ;; convert INTEGER
    (goto-char (point-min))
    (while (search-forward-regexp "INTEGER\\|SMALLINT" (point-max) 42)
      (replace-match "en" t t))

    ))

;; (defun gjg/gort ()
;;   "parse owens and minor thingie"
;;   (copy-region-as-kill (point-min) (point-max))
;;   (with-temp-buffer
;;     (yank)
;;     ;; trim whitespace at beginning and end of lines
;;     (replace-regexp "^\s+" "" nil (point-min) (point-max))
;;     (replace-regexp "\s+$" "" nil (point-min) (point-max))

;;     ;; keep only lines with key/value pairs
;;     (goto-char (point-min))
;;     (delete-non-matching-lines ":")

;;     ;; remove the page numbers
;;     (goto-char (point-min))
;;     (delete-matching-lines "page:")

;;     ;; trim whitespace following ':'
;;     (goto-char (point-min))
;;     (while (re-search-forward ":\s+" nil t)
;;       (replace-match ":" nil nil))

;;     ;; identify debit party fields
;;     (goto-char (point-min))
;;     (while (re-search-forward "\s\\{2,\\}\\(.+:\\)" nil t)
;;       (replace-match "\tDEBIT PARTY \\1" nil nil))

;;     ;; parse it
;;     (goto-char (point-min))
;;     (setq tempgort nil)
;;     (let  ((tempgort nil)
;; 	   (processing-details nil)
;; 	   (details nil)
;; 	   (current-line 0))
;;       (while (re-search-forward "\\([^:\t\n]+\\):\\([^\t\n]*\\)" nil t)
;; 	(when (string= "DETAILS" (match-string-no-properties 1)) (setq processing-details t))
;; 	(if (not processing-details)    ;; nab the check-level information
;; 	    (aput 'tempgort 'check
;; 		  (append (aget tempgort 'check)
;; 			  `(( ,(replace-regexp-in-string "\s+" "\s" (match-string-no-properties 1)) . ,(match-string-no-properties 2)))))
;; 	  (cond
;; 	   ((string= "LINE" (match-string-no-properties 1))
;; 	    (setq current-line (string-to-number (match-string-no-properties 2))))
;; 	   (t
;; 	    (aput 'details current-line
;; 		  (append (aget details current-line)
;; 			  `(( ,(replace-regexp-in-string "\s+" "\s" (match-string-no-properties 1)) . ,(match-string-no-properties 2)))))))))
;;       (aput 'tempgort 'remit details)
;;       (setq gort tempgort)
;;       (format "Nabbed %d payment fields and %d remittance fields" (length (aget gort 'check)) (if (sequencep (aget gort 'remit)) (length (aget gort 'remit)) 0)))))

;; (defun gjg/om-ach-check-remit-balances ()
;;   "Report on remittance total versus payment total."
;;   (interactive)
;;   (gjg/gort)
;;   (let ((amt 0)
;; 	(numlines 0))
;;     (dolist (myline (aget gort 'remit))
;;       (when (assoc "AMOUNT PAID" myline)
;; 	(progn
;; 	  (setq amt (+ amt (string-to-number (replace-regexp-in-string "[\\$,]" "" (cdr (assoc "AMOUNT PAID" myline))))))
;; 	  (setq numlines (+ 1 numlines)))))
;;     (message (format "Payment total is %s; Remit total is %.2f total in %d remittance lines" (replace-regexp-in-string "[\\$,]" "" (cdr (assoc "CREDIT" (aget gort 'check)))) amt numlines))))

;; (defun gjg/om-ach-make-mode-map ()
;;   "Create and return a mode map with Owens and Minor's ACH report mode key bindings/"
;;   (let ((keymap (make-sparse-keymap)))
;;     (define-key keymap "n" 'gjg/om-ach-mode-next-record)
;;     (define-key keymap "p" 'gjg/om-ach-mode-prev-record)
;;     keymap))

;; (define-derived-mode gjg/om-ach-mode fundamental-mode
;;   "Navigate the beach"
;;   (setq mode-name "Owens Minor ACH Report")
;;   (setq buffer-read-only t))

;; (setq gjg/om-ach-mode-map (gjg/om-ach-make-mode-map))

;; (defun gjg/om-ach-mode-narrow-to-record ()
;;   (beginning-of-line)
;;   (let ((beg (point))
;; 	(end (progn (forward-line) (re-search-forward "payment information:" nil t) (beginning-of-line) (point))))
;;     (when end
;;     ;; (message (format "Region is from %d to %d" beg end))
;;       (narrow-to-region beg end)
;;       (goto-char (point-min)))))


;; (defun gjg/om-ach-mode-next-record ()
;;   "Narrow buffer to the next record"
;;   (interactive)
;;   (let ((beg (point-min))
;; 	(end (point-max)))
;;     (widen)
;;     (end-of-line)
;;     (if (re-search-forward "payment information:" nil t)
;; 	(gjg/om-ach-mode-narrow-to-record)
;;       (progn (narrow-to-region beg end) (message "No next record.")))))

;; (defun gjg/om-ach-mode-prev-record ()
;;   "Narrow buffer to the previous record"
;;   (interactive)
;;   (goto-char (point-min)) ;; go to top of buffer in (hopefully narrowed) buffer
;;   (widen)
;;   (if (re-search-backward "payment information:" nil t)
;;       (gjg/om-ach-mode-narrow-to-record)
;;     (message "No previous record.")))


;; (defun gjg/om-ach-mode-narrow-to-credit ()
;;   "Narrow buffer to the credit section"
;;   (interactive)
;;     (goto-char (point-min))
;;     (let ((beg (point-min))
;; 	  (end (re-search-forward "=============================================================================" nil t)))
;;       (narrow-to-region beg end)
;; ))

(defvar gjg/lockbox-seq 1
  "The sequence number to use in writing lockbox index files")

;; (defun gjg/om-ach-mode-write-lockbox () 
;;   "Generate images for the credit and remittance sections of the current record and write SunTrust modified index file."
;;   (interactive)
;;   ;; first parse
;;   (gjg/gort)
;;   (let* ((credit (aget gort 'check))
;; 	 (remit  (aget gort 'remit))
;; 	 (tracenum (cdr (assoc "TRACE NUMBER" credit)))
;; 	 (lockbox "999999")
;; 	 (date "20100101")
;; 	 (batchnum "99")
;; 	 (checknum "999999999")
;; 	 (checkamt (replace-regexp-in-string "[$,]" "" (cdr (assoc "CREDIT" credit))))
;; 	 (checknum (cdr (assoc "TRACE NUMBER" credit))) ;; really the ACH number
;; 	 (routing (cdr (assoc "DEBIT PARTY ROUTING ID" credit)))
;; 	 (account (cdr (assoc "DEBIT PARTY DEMAND ACCT" credit)))
;; 	 (checkdate (cdr (assoc "EFFECTIVE DATE" credit)))
;; 	 (transnum (cdr (assoc "TRACE NUMBER" credit)))
;; 	 (importstring ""))
;;     ;; create the image subdirectory for this record
;;     (unless (file-exists-p (concat "images/" checknum))
;;       (make-directory (concat "images/" checknum) t))
;;     ;; generate the credit image
;;     (goto-char (point-min))
;;     (let ((beg (point-min))
;; 	  (end (re-search-forward "=============================================================================" nil t)))
;;       (shell-command-on-region beg end (concat "expand|convert -font Arial  -pointsize 12 -density 300x300 text:- -trim +repage images/" checknum "/credit.jpg")))
;;     (setq importstring (format "%s\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%s\"\n"
;; 			      importstring
;; 			      "" lockbox date batchnum gjg/lockbox-seq "" gjg/lockbox-seq "CK" checkamt "" checknum routing account (concat "images/" checknum "/credit.jpg") (replace-regexp-in-string "/" "" checkdate)))
;;     ;; generate remittance images (if any)
;;     (goto-char (point-min))
;;     ;; (if (re-search-forward "^\s*DETAILS:\s*$" nil t)
;;     (if (re-search-forward "^\s*REMITTANCE INFORMATION:\s*$" nil t)
;; 	(progn
;; 	  (beginning-of-line)
;; 	  (let ((beg (point))
;; 		;; (end (point-max)))
;; 		(end (progn (re-search-forward "^\s*DETAILS:\s*$" nil 42) (beginning-of-line) (point))))
;; 	    (shell-command-on-region beg end (concat "expand|convert -font Arial -pointsize 12 -density 300x300 text:- -page Letter  images/" checknum "/remits.jpg"))
;; 	    )))
;;     ;;   nab the remit image file names generated in the previous step
;;     (let ((remitimages (directory-files (concat "images/" checknum) nil "remits.*\.jpg" t)))
;;       (dolist (myremits remitimages)
;; 	(setq importstring (format "%s\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%s\"\n"
;; 				   importstring
;; 				   "" lockbox date batchnum gjg/lockbox-seq "" gjg/lockbox-seq "IN" checkamt "" checknum routing account (concat "images/" checknum "/" myremits) (replace-regexp-in-string "/" "" checkdate)))))
;;     (dolist (myremits remit)
;;       (let ((remitamt (or (replace-regexp-in-string "[$,]" "" (format "%s"(or (cdr (assoc "AMOUNT PAID" myremits)) 0)) 0)))
;; 	    (remitsearch1 (or nil (cdr (assoc "SELLER'S INVOICE NBR" myremits))))
;; 	    (remitsearch2 (or nil (cdr (assoc "INVOICE DATE" myremits)))))
;; 	(when (and remitamt (or remitsearch1 remitsearch2 ))
;; 	  (setq importstring (format "%s\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%s\"\n"
;; 				     importstring
;; 				     "" lockbox date batchnum gjg/lockbox-seq "" gjg/lockbox-seq "RM" remitamt (or remitsearch2 "") (or remitsearch1 "") routing account (concat "images/" checknum "/" "bogus.jpg") (replace-regexp-in-string "/" "" checkdate))))))
    

;;     ;; write the import index file
;;     (append-to-file importstring nil "summary-GORT.csv")

;;     ;; bump our sequence number
;;     (setq gjg/lockbox-seq (+ 1 gjg/lockbox-seq))
;;     ;; (with-temp-buffer
;;     ;;   (insert importstring)
;;     ;;   (write-file "summary-GORT.csv" t)
;;     ;;   )
;;     ))

;; (defun gjg/om-ach-mode-narrow-to-remittance ()
;;   "Narrow buffer to the remittance detail section"
;;   (interactive)
;; )

;; (defun gjg/om-wire-pdf ()
;;   "Convert and parse the Owens-Minor Wire Transfer PDF report"
;;   (interactive)
;;   (shell-command (concat "pdftotext -layout -nopgbrk " (shell-quote-argument "SunTrust Wire Report.pdf")))
;;   (with-temp-buffer
;;     (insert-file-contents "SunTrust Wire Report.txt")
;;     (delete-non-matching-lines "^[a-z]")
;;     (replace-regexp "^Credit\s" "\nCredit\s" nil (point-min) (point-max))
;;     (goto-char (point-min))
;;     (setq gort2 nil)
;;     (while (re-search-forward "\\([^ \n][^:\n]+\\):\s+\\(.*?\\)\\(\s\\{7\\}\\|$\\)" nil t)
;;       (setq gort2 (append gort2 (list (cons (replace-regexp-in-string "\s+" "\s" (match-string-no-properties 1)) (match-string-no-properties 2))))))
;;   ))

;; (defun gjg/check-bai2-receipt-totals ()
;;   "Sum up the check/payment/receipt amounts of a BAI2" 
;;   (interactive)
;;   (copy-region-as-kill (point-min) (point-max))
;;   (with-temp-buffer
;;     (yank)
;;     (goto-char (point-min))
;;     (let ((amt 0)
;; 	  (filetotal))
;;       ;; receipt amounts are in Record Type 6
;;       (while (re-search-forward "^6.\\{6\\}\\(.\\{10\\}\\)" nil t)
;; 	(setq amt (+ amt (string-to-number (match-string-no-properties 1))))
;; 	)
;;       (goto-char (point-min))
;;       (while (re-search-forward "^8.\\{34\\}\\(.\\{10\\}\\)" nil t)
;; 	(setq filetotal (string-to-number (match-string-no-properties 1))))
;;       (message "Total of all receipts in this file: %.0f; file total: %0.f" amt filetotal))
;;     )
;;   )

;; build-target '(id api-level name)
(defcustom gjg/android-targets
  '(("android-3" (1 3 "Android 1.5"))
    ("Google Inc.:Google APIs:3" (2 3 "Google APIs"))
    ("android-4" (3 4 "Android 1.6"))
    ("android-5" (5 5 "Android 2.0"))
    ("android-6" (7 6 "Android 2.0.1"))
    ("android-7" (9 7 "Android 2.1-update1"))
    ("android-8" (11 "Android 2.2")))
  "Android SDK build targets snarfed periodically from 'android list target'"
  :type '(alist :key-type string :value-type list)
  :group 'gjg
)

(defun gjg/create-android-project (project-name build-target application-name package-name activity min-sdk)
  ""
  (interactive)
  ;; (let* ((project-name (read-string "Project name: "))
  ;; 	 (build-target 
		       
);; gjg/create-android-project

;; bbdb parsing
;; 9 fields in a bbdb record
;; 1. first name 
;; 2. last name
;; 3. unknown (nil values)
;; 4. Company
;; 5. Phone numbers (list of vectors of 5 elements)
;;    1) Type (ie, Fax, Office, Home, etc.) (string)
;;    2-4) phone number elements
;;    5) extension (0 if no extension was specified)
;; 6. Addresses (list of vectors of 6 elements)
;;    1) Type (ie, Home, Office, etc)
;;    2) Street address (list of n strings)
;;    3) City
;;    4) State
;;    5) Zip
;;    6) Country
;; 7. Email addresses (list of strings)
;; 8. Annotation and metadata (list of cons) (keys: notes, creation-date, timestamp)
;; 9. unknown (nil values)

(defun grab-s3-bucket (url)
  (interactive "sURL for Amazon s3 bucket: ")
  (shell-command (format "curl -O %s" url) (get-buffer-create url)))

;; M-x grab-s3-bucket URL
;; You could write the results back with something like:

(defun write-s3-bucket (url)
  (interactive "sURL for Amazon s3 bucket: ")
  (shell-command-on-region (format "curl %s -T " url)))

(provide 'gjg-functions)
