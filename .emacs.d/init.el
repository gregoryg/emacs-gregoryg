(defvar gjg/check-packages-on-startup nil)

(add-to-list 'load-path "~/emacs")
(require 'cl)
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)


(require 'package)
;; (autoload 'package "package" "Elpa and similar package manager")
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  ;; for important compatibility packages like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; try to get control of required packages across different emacs environments
(defvar gjg/required-packages 
  '(auctex
    auto-complete
    cider
    clojure-mode
    dash
    dumb-jump
    edit-server
    elpy
    ensime
    ess
    ess-R-data-view
    exec-path-from-shell
    flx-ido
    hl-line+
    htmlize
    ido-completing-read+
    ido-ubiquitous
    js2-mode
    json-mode
    json-reformat
    magit
    markdown-mode
    material-theme
    multi-term
    multiple-cursors
    nodejs-repl
    org
    org-plus-contrib
    pig-mode
    pretty-lambdada
    python-mode
    pyvenv
    queue
    rainbow-delimiters
    recursive-narrow
    redo+
    s
    scala-mode
    smart-mode-line
    smartparens
    spaceline
    sql-indent
    uuid
    web-mode
    yafolding
    yaml-mode
    yasnippet
    zencoding-mode
    )
)
(defun gjg/packages-installed-p ()
  (loop for p in gjg/required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, install 'em
(when gjg/check-packages-on-startup
  (unless (gjg/packages-installed-p)
    ;; check for new package versions
    (message "%s" "This copy of Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" "  done.")
    ;; install the missing packages
    (dolist (p gjg/required-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

;; OS X / Mac specific things
(setq ns-command-modifier (quote meta))
;; deal with bad behavior on OSX (PATH does not export for remote shells)
(when (and (memq window-system '(mac ns)) (fboundp 'exec-path-from-shell-initialize))
  (exec-path-from-shell-initialize))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; add in local user's bin/
(when (file-exists-p "~/bin/")
  (add-to-list 'exec-path "~/bin")
  )

;; Spacemacs theme stuff
(load-theme 'material t)
;; (load-theme 'spacemacs-dark t)
;; (require 'spaceline-config)
;; (spaceline-emacs-theme)

 ;; fancy git icon
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((gitlogo (replace-regexp-in-string "^ Git." "  " vc-mode)))
        (setq vc-mode gitlogo))))


;;* Font selection

;; the (good) bits that were in emacs-config.el
;; ** Global identity (ie, same on all machines)
(setq user-full-name "Gregory Grubbs"
      user-mail-address "gregory@dynapse.com"
      w3m-home-page "http://www.google.com"
      w3m-use-cookies t)
(setq confirm-kill-emacs 'y-or-n-p) ;; confirm to exit emacs
;; dippy bird FTW
(fset 'yes-or-no-p 'y-or-n-p)
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)
(setq explicit-bash-args '("--noediting" "-i"))
(add-hook 'shell-mode-hook
	  (lambda ()
	    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
	    (shell-command "export PS1='\\[\\e[32m\\]\\u@\\h \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$ '")
	    ))
(add-to-list 'Info-default-directory-list "~/emacs/org-mode/doc")


(autoload 'hl-line+ "hl-line+" "Extensions to hl-line")
(eval-after-load "hl-line+"
  '(global-hl-line-mode t))

;; windmove
(require 'windmove)
(eval-after-load "windmove"
  '(progn
     (windmove-default-keybindings)
     ;; Make windmove work in org-mode:
     (setq org-replace-disputed-keys t)))
     ;; (add-hook 'org-shiftup-final-hook 'windmove-up)
     ;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
     ;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
     ;; (add-hook 'org-shiftright-final-hook 'windmove-right)))



;; (add-to-list 'load-path "~/emacs/org-8.2.10/lisp")
;; (add-to-list 'load-path "~/emacs/org-8.2.10/contrib/lisp")
(add-to-list 'load-path "~/emacs/org-reveal")
(add-to-list 'load-path "~/emacs/dtl-mode")
;; backup files
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 1
 kept-old-versions 1
 version-control 'never)       ; use versioned backups

;; rainbow delimiters ; make much stronger (more saturated) colors
(autoload 'rainbow-delimiters "rainbow-delimiters" "Highlight brackets according to their depth")

(eval-after-load "rainbow-delimiters"
  '(progn
     (require 'cl-lib)
     (require 'color)
     (cl-loop
      for index from 1 to rainbow-delimiters-max-face-count
      do
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	(cl-callf color-saturate-name (face-foreground face) 30)))))

(add-hook 'vc-before-checkin-hook
	  #'(lambda ()
	      (set-window-buffer
	       (split-window-vertically)
	       (get-buffer-create "*VC-log*"))))

;; try global find-file-at-point smarts
;; (when (require 'ffap nil t)
;;   (ffap-bindings))
;; Syncsort DTL mode
(autoload 'ebnf-mode "ebnf-mode" "A/E/BNF Grammar Mode" t)
(add-to-list 'auto-mode-alist '("bnf$" . ebnf-mode))
(autoload 'dtl-mode "dtl-mode" "Syncsort DTL" t)
(add-to-list 'auto-mode-alist '("\\.dtl$" . dtl-mode))
(add-hook 'dtl-mode-hook 
	  (lambda ()
	    (hl-line-mode 1)
	    (when (and (boundp 'dtl-ac-dict-file) (file-exists-p dtl-ac-dict-file) (require 'auto-complete nil t))
	      (add-to-list 'ac-dictionary-files dtl-ac-dict-file)
	      (ac-config-default)
	      (auto-complete-mode 1))
	    ;; (when (require 'yasnippet nil t)
	    ;;   (yas-minor-mode 1))
	    ))

;; web-dev
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.repo\\'" . conf-mode))
(require 'web-mode)
;; (eval-after-load "web-mode"
;;   (progn
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")
        ("jinja"  . "\\.html")
        ))
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-sql-indent-offset 2)
  ;;   )
  ;; )
(define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)

;;    (local-set-key (kbd "C-c C-v") 'browse-url-of-file)

    

;; Python
(require 'ob-python)
;; Latex
(require 'ob-latex)
;; SQL
(require 'sql)
;; (add-hook 'sql-mode-hook (lambda
(sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|[Mm][Yy][Ss][Qq][Ll]\\) *\\[?[_a-zA-Z0-9()]*\\]?> ")
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
;; ))
;; Clojure
(require 'ob-clojure) ;; org-babel code evaluation

;; (add-to-list 'load-path "~/emacs/cider")
(autoload 'cider "cider" "Cider for Clojure")
(require 'cider)
(eval-after-load "cider"
  '(progn
     ;; (require 'cider)
     ;; (defadvice cider--lein-present-p (around gjg-find-the-damn-script activate)
     ;;   "Lein shell script is not detected on Windows as executable"
     ;;   (if (eq window-system 'w32)
     ;;       (setq ad-return-value (or (file-remote-p default-directory)
     ;;    			     (locate-file "lein" exec-path nil 'exists)))
     ;;     (ad-do-it)))
     (add-hook 'cider-repl-mode-hook 'company-mode)
     (add-hook 'cider-mode-hook 'company-mode)))

(autoload 'smartparens-config "smartparens-config" "Default configuration for smartparens package")
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(require 'smartparens)
;; (eval-after-load "smartparens-config"
;;   '(progn 
;; (sp-local-pair 'javascript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js2-mode        "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js2-mode        "[" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'javascript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;; ))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'smartparens-mode)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)

;; R
(setq ess-sas-edit-keys-toggle nil)     ;;; turn on automatic indentation
(require 'ess-site)
(when (require 'polymode nil t)
  (progn
    ;;; R modes
    (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))))

;; be sure to load org-mode after windmove (ref: org-disputed-keys)
(load-library "gjg-org")
;; (with-eval-after-load 'tramp'
;;   (setq tramp-default-method "rsync"))

;; ** Do special things for special OS
(setq w32-get-true-file-attributes nil)

;; **** Windows printing that relies on ghostscript and gsview

(defcustom gjg/gsprint "c:/Program Files/Ghostgum/gsview/gsprint.exe"
  "Path to gsprint.exe: usually in Program Files or Program Files (x86)")

(when (and (string= (window-system) "w32") (file-exists-p gjg/gsprint))
  (progn
    ;;  Windows printer
    (setq-default ps-lpr-command (expand-file-name gjg/gsprint))
    (setq-default ps-printer-name t)
    (setq-default ps-printer-name-option nil)
    (setq ps-lpr-switches '("-query")) ; show printer dialog
    (setq ps-right-header '("/pagenumberstring load" ps-time-stamp-mon-dd-yyyy))))

(temp-buffer-resize-mode 1)  ;; crazy cool
;; very important: keep a long list of yow lines
;;   in emacs 24.4 yow.el is obsolete; use cookie instead
(when (featurep 'yow)
  (unload-feature 'yow))


(require 'cookie1)
(eval-after-load "cookie1"
  '(progn
     (setq yow-file "~/emacs/yow.lines")
     (defun yow ()
       (interactive)
       (let ((suggest-key-bindings nil))
	 (message (cookie yow-file nil nil))))))
;; almost as important: keep track of those tetris scores
(setq tetris-score-file "~/.emacs.d/tetris.score")
(global-set-key [home] (lambda () (interactive) (goto-char (point-min))))
(global-set-key [end] (quote end-of-buffer))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(setq fill-column 90); default for wider screens

;; ensure that the default for searches is case-insensitive
(setq case-fold-search t)

;; highlight/colorize everything that can be colorized!
(global-font-lock-mode 1)

;; When I want transient mark mode, I'll beat it out of you
(setq transient-mark-mode nil)

;; display time and battery status in the mode line
(display-time-mode 1)
;; ;; unbelievably weird thing I have to do to get the right time in emacs on my Windows laptop
;; (when (eq (window-system) 'w32)
;;   (progn
;;     (set-time-zone-rule "MDT+6")
;;     (display-time)))
(display-battery-mode 0)
;; turn off that stupid toolbar
(tool-bar-mode -1)
;; scroll bars?
(scroll-bar-mode -1)

;; don't do that stoopid splash screen
(setq inhibit-splash-screen t)

;; never split windows horizontally by default
(setq split-width-threshold nil)
;; Here's a choice one: stay in the same column while scrolling!
(setq scroll-preserve-screen-position t)
;; in Emacs23+, make line-move move by a line, ignoring wrapping
(setq line-move-visual nil)
					;
;; enable hugely useful things that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; echo quickly
(setq echo-keystrokes 0.1)
;; show column number in modeline
(column-number-mode t)
;; show them parens
(show-paren-mode 1)

(ansi-color-for-comint-mode-on)
;; keep the screen from jumping wildly as I cursor down/up
(setq scroll-conservatively 5)
;; remember minibuffer history between sessions
(savehist-mode t)
(setq frame-title-format (concat "emacs@" (system-name) " - %f; %b"))
;; if point is at end of line, keep it there for vertical movement
(setq track-eol t)
;; Dynamic abbrevs should expand both words and symbols
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
;; and now set up hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") (lambda () (interactive) (ibuffer)))
(setq ibuffer-expert t)

(autoload 'uniquify "uniquify" "unique buffer names dependent on file name")
(eval-after-load "uniquify"
  '(progn
     (setq uniquify-buffer-name-style 'reverse)
     (setq uniquify-separator "/")
     (setq uniquify-after-kill-buffer-p t)
     (setq uniquify-ignore-buffers-re "^\\*")))

(setq align-to-tab-stop nil) ;; do not use tabs for align and align-regexp

(defun indent-buffer ()
  "Fontify and indent buffer using current mode."
  (interactive)
  (font-lock-fontify-buffer)
  (indent-region (point-min) (point-max) nil)
  )

(defun revert-dammit () ; revert buffer without prompting
  (interactive)
  ;; revert buffer, don't use auto-save, preserve modes
  (revert-buffer t t t))
(defalias 'rd 'revert-dammit)
(defun gjg/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer))
  (message "Switched to other-buffer"))
(defun gjg/other-window-or-split ()
  "If one window is displayed in current frame, split vertically and go to other window, else go to other window without splitting"
  (interactive)
  (if (one-window-p t)
      (progn
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer (other-buffer)))
    (other-window 1)))
(defun gjg/widen-ask-if-indirect ()
  "If buffer is indirect, ask before widening"
  (interactive)
  (if (buffer-base-buffer)
      (when (yes-or-no-p "Buffer is indirect; really widen? ")
	(widen))
    (widen)))
(defun toggle-transparency (alpha-level)
  (interactive "p")
  (message (format "%s" alpha-level))
  (if (< alpha-level 50) (setq alpha-level 75))
  (let ((myalpha (frame-parameter nil 'alpha)))
    (if (or (not myalpha)
            (= myalpha 100))
        (set-frame-parameter nil 'alpha alpha-level)
      (set-frame-parameter nil 'alpha 100))
    )
  (message (format "Alpha level is %d" (frame-parameter nil 'alpha)))
  )

(defun set-transparency (alpha-level)
  (interactive "p")
  (message (format "Alpha level passed in: %s" alpha-level))
  (let ((alpha-level (if (< alpha-level 2)
			 (read-number "Opacity percentage: " 75)
		       alpha-level))
	(myalpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha alpha-level))
  (message (format "Alpha level is %d" (frame-parameter nil 'alpha))))
(defalias 'set-opacity 'set-transparency )

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)
;; *** Full-screen frame defuns

(defun switch-full-screen ()
  (interactive)
  (shell-command (concat "/usr/bin/wmctrl -i -r " (frame-parameter nil 'outer-window-id) " -btoggle,fullscreen")))

;; (defun toggle-fullscreen ()
;;   "toggles whether the currently selected frame consumes the entire display or is decorated with a window border"
;;   (interactive)
;;   (let ((f (selected-frame)))
;;     (modify-frame-parameters f `((fullscreen . ,(if (eq nil (frame-parameter f 'fullscreen)) 'fullboth nil))))))

;; (defun mac-toggle-max-window ()
;;   "Toggle full-screen frame on Linux and OS X - use maxframe.el for Windows"
;;   (interactive)
;;   (if (frame-parameter nil 'fullscreen)
;;       (set-frame-parameter nil 'fullscreen nil)
;;     (set-frame-parameter nil 'fullscreen 'fullboth)))
;; (defun toggle-full-screen ()
;;   "Toggle between full screen and partial screen display on X11;
;;     courtesy of http://www.emacswiki.org/cgi-bin/wiki/FullScreen"
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; Recursive narrowing!
(require 'recursive-narrow)
(global-set-key (kbd "C-x n n") 'recursive-narrow-or-widen-dwim)
(global-set-key (kbd "C-x n w") 'recursive-widen)
;; (global-set-key (kbd "C-x n n") 'narrow-to-region)
;; (global-set-key (kbd "C-x n w") 'widen)

;; ;; add toggle for maxframe.el (works on Windows)
(when (eq window-system 'w32)
  (require 'maxframe)
  (modify-frame-parameters nil '((gjg/frame-maxp . nil))))
;; (modify-frame-parameters nil '((gjg/frame-maxp . nil)))
;; ;; (defvar gjg/frame-maxp nil "Store whether frame is maximized using maxframe.el")

(require 'dash)
(defun gjg/switch-buffer-by-mode ()
  "Switch to a buffer after choosing a mode."
  (interactive)
  (let* ((active-buffers-with-mode (mapcar '(lambda (x) (with-current-buffer x (cons (buffer-name) (symbol-name major-mode)))) (buffer-list)))
	 (distinct-modes (-distinct (mapcar #'cdr active-buffers-with-mode)))
	 (selected-mode (ido-completing-read "Mode: " distinct-modes))
	 (candidate-buffers (mapcar #'car (remove-if-not '(lambda (x) (string-equal selected-mode (cdr x))) active-buffers-with-mode)))
	 (selected-buffer (ido-completing-read "Buffer: " candidate-buffers)))
    (switch-to-buffer selected-buffer)))
(global-set-key (kbd "C-c s") 'gjg/switch-buffer-by-mode)

(defun gjg/max-frame ()
  "Maximize the current frame and toggle gjg/frame-maxp"
  (interactive)
  (cond ((eq window-system 'w32)
         (maximize-frame))
	((eq window-system 'x)
	 (switch-full-screen))
        (t
         (mac-toggle-max-window)))
  (modify-frame-parameters nil '((gjg/frame-maxp . t))))

(defun gjg/restore-frame ()
  "Restore the current frame to its previous size and toggle gjg/frame-maxp"
  (interactive)
  (cond ((eq window-system 'w32)
         (restore-frame))
	((eq window-system 'x)
	 (switch-full-screen))
        (t
	 (mac-toggle-max-window)))
  (modify-frame-parameters nil '((gjg/frame-maxp . nil))))

(defun gjg/toggle-max-frame ()
  "Check the status of gjg/max-framep and change to whichever mode we're not in now."
  (interactive)
  (cond ((eq window-system 'ns)
         (toggle-frame-maximized))
        (t
         (if (eq (frame-parameter nil 'gjg/frame-maxp) nil) (gjg/max-frame) (gjg/restore-frame)))))

;; take back my C-o binding, emacs25!
(global-set-key (kbd "C-o") 'open-line)
;; redefine the obsolete spell-word
(defalias 'spell-word  'ispell-word)

(defalias 'spell-buffer 'ispell-buffer)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'gjg/switch-to-other-buffer)
(global-set-key [f3] 'gjg/other-window-or-split)
(global-set-key [f4] 'narrow-to-defun)

(global-set-key [f5] 'gjg/widen-ask-if-indirect)
(global-set-key [f6] 'toggle-read-only)
(global-set-key [f7] 'hl-line-mode) ;; toggle hl-line-mode for this window only
(global-set-key [f8] 'bury-buffer)

(global-set-key [f9] 'bury-buffer)
(global-set-key [f10] 'dired-omit-mode)
;; (global-set-key [f11] 'mac-toggle-max-window)
(global-set-key [f11] 'gjg/toggle-max-frame)
(global-set-key [M-f11] 'gjg/toggle-max-frame)
;; (global-set-key [f11] 'toggle-fullscreen)
;; (global-set-key [f11] 'switch-full-screen)
;; (global-set-key (kbd "C-<f11>") 'gjg/emacs-max-coolness)
(global-set-key [f12] (lambda () (interactive) (message (or (buffer-file-name) "No file associated with this buffer."))))
(global-set-key (kbd "<C-f12>") 'yow)
(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)
(global-set-key [S-wheel-up] 'inc-font-size)
(global-set-key [S-wheel-down] 'dec-font-size)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "C-c C-v") 'browse-url-at-point)
;; EXPERIMENTAL: unbind SPACE and ? in minibuffer, to allow typing in completions with those chars
(add-hook 'minibuffer-setup-hook (lambda () 
                                   (define-key minibuffer-local-completion-map " " nil)
                                   (define-key minibuffer-local-must-match-map " " nil)
                                   (define-key minibuffer-local-completion-map "?" nil)
                                   (define-key minibuffer-local-must-match-map "?" nil)))

(server-start)


;; (if (window-system)
;;     ;; (load-theme 'noctilux t)
;;     ;; (load-theme 'grandshell t)
;;     ;; (load-theme 'gjg-twilight t)
;;   ;;    (load-theme 'deeper-blue t)
;;   ;; (load-theme 'solarized-dark t)
;;   )

(cond ((or (eq window-system 'mac) (eq window-system 'ns))
       (set-frame-font "Source Code Pro-17"))
      ((eq window-system 'w32)
       ;; (set-face-font 'default '"-outline-Inconsolata-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
       (set-face-font 'default '"-outline-Source Code Pro-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"))
      ((and (eq window-system 'x) (eq emacs-major-version 23))
       (set-face-font 'default '"-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
       (add-to-list 'default-frame-alist '(font . "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
       )
      ((and (eq window-system 'x) (eq emacs-major-version 24))
       ;; (set-frame-font "Inconsolata-16")
       (set-frame-font "Source Code Pro-16"))
      ((eq window-system 'x)
       (set-frame-font "Inconsolata-21")
                                        ;(set-face-font 'default '"10x20")
       ))
(require 'fontize)
(global-set-key [C-kp-subtract] 'dec-font-size)
(global-set-key [C-kp-add] 'inc-font-size)

;; *** Desktop save
(desktop-save-mode 1)
(setq desktop-restore-eager 5)
;; GJG TODO: does not work in emacs 25.x
(defun add-ssh-agent-to-tramp ()
  (cl-pushnew '("-A")
              (cadr (assoc 'tramp-login-args
                           (assoc "ssh" tramp-methods)))
              :test #'equal))
(add-ssh-agent-to-tramp)

(defun tv-list-tramp-buffer-file-name ()
  "Return a list of buffers that I do not want automatically restored by desktop.el in the next emacs session: TRAMP dirs and files, Info buffer, and Dired buffers"
  (let* ((desktop-info-list (mapcar #'desktop-buffer-info (buffer-list)))
         (tramp-buf-list (loop for i in desktop-info-list
			       if (and (listp i)
				       (stringp (car (nth 8 i)))
				       (or
					(string= "dired-mode" (nth 3 i))
					(string= "Info-mode" (nth 3 i))
					(string-match "^/su:.*\\|^/sudo:.*\\|^/ssh:.*\\|^/scp[^:]*:.*\\|^/smb:.*" (car (nth 8 i)))))
			       collect (nth 2 i))))
    tramp-buf-list))

;; (add-hook 'desktop-save-hook #'(lambda ()
;;                                  (let ((del-buf-list
;;                                         (tv-list-tramp-buffer-file-name)))
;;                                    (dolist (i del-buf-list)
;;                                      (if (get-buffer i) (kill-buffer i))))))
;;(setq desktop-buffers-not-to-save
;;      (concat "\\` "
;;	      (tv-list-tramp-buffer-file-name)))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; ** IDO


(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode t) ;; from ido-ubiquitous package, not ido package!!
(setq ido-auto-merge-delay-time 5.0)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
;; (setq ido-use-filename-at-point 'guess) ;; prefer file names near point
(setq ido-use-filename-at-point nil) ;; prefer file names near point

;; **** Dired

(declare (special dired-x-hands-off-my-keys
		  dired-bind-vm
		  dired-omit-files-p))
(add-hook 'dired-load-hook
	  (function
	   (lambda ()
	     (declare (special dired-x-hands-off-my-keys
			       dired-bind-vm
			       dired-omit-files-p))
	     (load-library "dired-x")
	     (setq dired-x-hands-off-my-keys nil
		   dired-bind-vm t))))

(add-hook 'dired-mode-hook
	  (function (lambda ()
                      (auto-revert-mode))))
;; 		      ;; Set dired-x buffer-local variables here.  For example:
;; 		      (setq dired-omit-mode t)
;; 		      (setq dired-omit-files "^\\..*")
;; 		      )))
(setq dired-dwim-target t)
(require 'dired-x)

;; **** Occur

;; **** yafolding
(global-set-key (kbd "C-c -") 'yafolding-hide-element)
(global-set-key (kbd "C-c =") 'yafolding-show-element)

;;     Some simple sex-ups for occur-mode

(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)
(defadvice occur-next (after gjg/occur-navigation-other-window)
  "Show match in other window as you navigate, as in rgrep"
  (occur-mode-display-occurrence))
(ad-activate 'occur-next)
(defadvice occur-prev (after gjg/occur-navigation-other-window)
  "Show match in other window as you navigate, as in rgrep"
  (occur-mode-display-occurrence))
(ad-activate 'occur-prev)

;; **** Help mode


(add-hook 'help-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-p") 'help-go-back)
	    (local-set-key (kbd "M-n") 'help-go-forward)
	    ))

;; EXPERIMENTAL: unbind SPACE and ? in minibuffer, to allow typing in completions with those chars
(add-hook 'minibuffer-setup-hook (lambda () 
                                   (define-key minibuffer-local-completion-map " " nil)
                                   (define-key minibuffer-local-must-match-map " " nil)
                                   (define-key minibuffer-local-completion-map "?" nil)
                                   (define-key minibuffer-local-must-match-map "?" nil)))

(require 'gjg-functions)
;; * Near end of inits, make Info work on Windows
(when  (eq window-system 'w32)
  (progn
    (setq Info-directory-list Info-default-directory-list)
    (setq Info-additional-directory-list '("c:/emacs/share/info"))))

;; Try to improve file open speeds on Windows
(when (eq window-system 'w32)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

;; (defadvice show-paren-function (after my-echo-paren-matching-line activate)
;;   "If a matching paren is off-screen, echo the matching line."
;;   (when (char-equal (char-syntax (char-before (point))) ?\))
;;     (let ((matching-text (blink-matching-open)))
;;       (when matching-text
;;         (message matching-text)))))
;; open a nice local shell
(shell (get-buffer-create "sh1"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(auto-revert-verbose nil)
 '(compilation-message-face (quote default))
 '(csv-separators (quote (",")))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "ffc01b1b3a7cc43c6d0f25ff5573c21fe6cdf2e4e6ab0e4667856f1a90b98c60" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "0cebcfb34ef4f79b8ed16520d199ae323290052e2a1cd0aab9d0a1dcce98d7a8" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(dired-auto-revert-buffer t)
 '(dired-omit-files "^\\.?#\\|^\\..*")
 '(dumb-jump-project-denoters
   (quote
    (".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el" "package.json")))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#373b41")
 '(font-lock-verbose t)
 '(gjg/os-open "exo-open")
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(grep-command "grep --color=auto -nH -e ")
 '(grep-find-command
   (quote
    ("find . -type f -exec grep --color=auto -nH -e  {} +" . 34)))
 '(grep-find-template
   "find . <X> -type f <F> -exec grep --color=auto <C> -nH -e <R> {} +")
 '(grep-highlight-matches (quote always))
 '(grep-template "grep --color=auto <X> <C> -nH -e <R> <F>")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#1c1f26")
 '(ido-everywhere t)
 '(ido-ubiquitous-mode t)
 '(ido-use-virtual-buffers t)
 '(ispell-program-name "aspell")
 '(ls-lisp-verbosity nil)
 '(magit-diff-use-overlays nil)
 '(magit-revert-buffers nil t)
 '(magit-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(ns-command-modifier (quote meta))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files
   (quote
    ("~/Google Drive/cloudera.org" "~/Google Drive/Customers and Prospects/Simic/Scopely/scopely.org")))
 '(org-babel-load-languages
   (quote
    ((R . t)
     (awk . t)
     (emacs-lisp . t)
     (shell . t)
     (sql . t))))
 '(org-directory "~/Google Drive/projects/")
 '(org-drill-optimal-factor-matrix
   (quote
    ((2
      (1.8000000000000003 . 2.226)
      (2.04 . 2.243)
      (2.2199999999999998 . 2.328)
      (2.1799999999999997 . 2.325)
      (1.96 . 2.238)
      (2.36 . 2.412)
      (2.6 . 2.588)
      (1.7000000000000002 . 2.15)
      (2.7 . 2.679)
      (2.5 . 2.5)
      (2.46 . 2.496))
     (1
      (1.96 . 3.58)
      (1.7000000000000002 . 3.44)
      (2.36 . 3.86)
      (2.5 . 4.0)
      (2.1799999999999997 . 3.72)
      (2.6 . 4.14)))))
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope (quote tree))
 '(org-export-backends (quote (ascii html icalendar latex md odt confluence)))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-html-htmlize-output-type (quote css))
 '(org-html-postamble t)
 '(org-html-postamble-format
   (quote
    (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill org-learn)))
 '(org-src-fontify-natively t)
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded (quote content))
 '(org-use-sub-superscripts (quote {}))
 '(org-yank-folded-subtrees t)
 '(package-selected-packages
   (quote
    (pretty-lambdada prettify-greek ob-ipython yafolding dart-mode ac-js2 inf-clojure ein ac-cider cider material-theme hc-zenburn-theme afternoon-theme molokai-theme rainbow-identifiers labburn-theme scala-mode ensime spaceline airline-themes spacemacs-theme zencoding-mode yaml-mode web-mode uuid tramp-hdfs sql-indent smartparens smart-mode-line-powerline-theme redo+ recursive-narrow rainbow-delimiters queue python-mode projectile-speedbar pig-mode org-plus-contrib ocodo-svg-modelines nodejs-repl multiple-cursors multi-term monokai-theme markdown-mode magit json-mode js2-mode ido-ubiquitous htmlize hl-line+ flx-ido exec-path-from-shell ess-R-object-popup ess-R-data-view epresent elpy edit-server deft color-theme-sanityinc-tomorrow clojure-mode auto-complete auctex)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-interpreter-args "--simple-prompt --pprint")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (sh-indent-comment . t)
     (eval when
           (fboundp
            (quote aggressive-indent-mode))
           (aggressive-indent-mode -1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (outline-minor-mode)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (buffer-read-only . true)
     (emacs-lisp-docstring-fill-column . 75))))
 '(scroll-all-mode nil)
 '(send-mail-function (quote mailclient-send-it))
 '(server-port "9999")
 '(server-use-tcp nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sp-hybrid-kill-excessive-whitespace t)
 '(spaceline-info-mode t)
 '(sql-postgres-login-params
   (quote
    ((user :default "ggrubbs")
     password server
     (database :default "ggrubbs")
     port)))
 '(switch-to-buffer-in-dedicated-window (quote prompt))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-default-method "rsync")
 '(tramp-inline-compress-start-size nil)
 '(tramp-ssh-controlmaster-options
   "-o ControlPath='/Users/gregoryg/.ssh/sockets/%%r@%%h-%%p' -o ControlMaster=auto -o ControlPersist=yes" t)
 '(tramp-use-ssh-controlmaster-options nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(version-control t)
 '(web-mode-enable-engine-detection t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(winner-mode t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-x-prompt yas-completing-prompt yas-dropdown-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-html-tag-face ((t (:foreground "DodgerBlue")))))
(put 'dired-find-alternate-file 'disabled nil)
