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
  '(ag
    auctex
    auto-complete
    bash-completion
    cider
    clojure-mode
    csv-mode
    dash
    docker-tramp
    dumb-jump
    edit-indirect
    edit-server
    elpy
    ensime
    ess
    ess-R-data-view
    exec-path-from-shell
    flx
    go-mode
    htmlize
    js2-mode
    json-mode
    json-reformat
    lsp-mode
    magit
    magit-todos
    markdown-mode
    material-theme
    multi-term
    multiple-cursors
    nodejs-repl
    org
    org-plus-contrib
    pandoc-mode
    prettify-greek
    python-mode
    pyvenv
    queue
    rainbow-delimiters
    recursive-narrow
    ;; redo+
    s
    scala-mode
    smart-mode-line
    smartparens
    spaceline
    sql-indent
    terraform-mode
    use-package
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

;; let's get encryption established
(require 'epa-file)
(epa-file-enable)
(require 'org-crypt)
(org-crypt-use-before-save-magic)
;(setq org-tags-exclude-from-inheritance (quote ("crypt"))) 
;;  set to nil to use symmetric encryption.
;; (setq org-crypt-key nil)


;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

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

;; For gpg, disable external pinentry - let emacs handle pass phrase
(setenv "GPG_AGENT_INFO" nil)

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


;; (autoload 'hl-line+ "hl-line+" "Extensions to hl-line")
;; (eval-after-load "hl-line+"
;;   '(global-hl-line-mode t))
(global-hl-line-mode t)

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
 kept-new-versions 5
 kept-old-versions 5
 version-control nil)

;; Moos and MUDs - NOT legacy stuff - added 2019!
(add-to-list 'load-path "~/projects/rmoo")
(require 'rmoo-autoload)
(require 'moocode-mode)
;; (global-set-key (kbd "C-c C-r") 'rmoo)
(add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))
(add-hook 'rmoo-interactive-mode-hooks
          (lambda ()
            (linum-mode -1)                  ;; ... no line numbers
            (goto-address-mode t)))          ;; ... clickable links

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
;; deal with the horrific ANSI codes in latest ipython
(setq ansi-color-drop-regexp
  "\033\\[\\([ABCDsuK]\\|[12][JK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\|\\?[0-9]+[hl]\\|[0-9]+[CD]\\|J\\|6n\\)")

(require 'elpy)
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")


(require 'ob-python)
(require 'prettify-greek)
(add-hook 'python-mode-hook
          (lambda ()
            (setq prettify-symbols-alist prettify-greek-lower)
            (prettify-symbols-mode t)))


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
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  (setq lsp-enable-indentation nil)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

;; ;; (add-to-list 'load-path "~/emacs/cider")
;; (autoload 'cider "cider" "Cider for Clojure")
;; (require 'cider)
;; (eval-after-load "cider"
;;   '(progn
;;      ;; (require 'cider)
;;      ;; (defadvice cider--lein-present-p (around gjg-find-the-damn-script activate)
;;      ;;   "Lein shell script is not detected on Windows as executable"
;;      ;;   (if (eq window-system 'w32)
;;      ;;       (setq ad-return-value (or (file-remote-p default-directory)
;;      ;;    			     (locate-file "lein" exec-path nil 'exists)))
;;      ;;     (ad-do-it)))
;;      (add-hook 'cider-repl-mode-hook 'company-mode)
;;      (add-hook 'cider-mode-hook 'company-mode)))

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

;; Go
(add-hook 'go-mode-hook 'smartparens-mode)
(add-hook 'go-mode-hook 'rainbow-delimiters-mode)


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
(require 'org-tempo)
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
;; M-left and M-right on Pixelbook
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-set-key (kbd "<M-left>") (lambda () (interactive) (goto-char (point-min))))
(global-set-key (kbd "<M-right>") 'end-of-buffer)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "M-]")) ; unset one next-buffer binding
(global-unset-key (kbd "M-[")) ; unset one previous-buffer binding
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
(global-set-key [f6] 'read-only-mode)
(global-set-key [f7] 'hl-line-mode) ;; toggle hl-line-mode for this window only
;; (global-set-key [f8] 'bury-buffer)

;; (global-set-key [f9] 'bury-buffer)
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
       (set-frame-font "Source Code Pro-17")
       (setq gjg/os-open "open"))
      ((eq window-system 'w32)
       ;; (set-face-font 'default '"-outline-Inconsolata-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
       (set-face-font 'default '"-outline-Source Code Pro-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1")
       (setq gjg/os-open "cygpath"))
      ((and (eq window-system 'x) (eq emacs-major-version 23))
       (set-face-font 'default '"-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
       (add-to-list 'default-frame-alist '(font . "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
       )
      ((and (eq window-system 'x) (eq emacs-major-version 24))
       ;; (set-frame-font "Inconsolata-16")
       (set-frame-font "Source Code Pro-16"))
      ((eq window-system 'x)
       (set-frame-font "Inconsolata-16")
       (setq gjg/os-open "xdg-open")
                                      ;(set-face-font 'default '"10x20")
       ))
(require 'fontize)
(global-set-key [C-kp-subtract] 'dec-font-size)
(global-set-key [C-kp-add] 'inc-font-size)

;; *** Desktop save
(desktop-save-mode 1)
(setq desktop-restore-eager 5)

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
					(string-match "^/su:.*\\|^/sudo:.*\\|^/ssh:.*\\|^/scp[^:]*:.*\\|^/smb:.*\\|^/docker:.*" (car (nth 8 i)))))
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
;; ;; ** Ivy
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

;; ** IDO


(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
;; (ido-ubiquitous-mode t) ;; from ido-ubiquitous package, not ido package!!
;; (setq ido-auto-merge-delay-time 5.0)
;; (setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
;; (setq ido-enable-tramp-completion nil)
;; (setq ido-enable-last-directory-history nil)
;; (setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
;; (setq ido-show-dot-for-dired t) ;; put . as the first item
;; ;; (setq ido-use-filename-at-point 'guess) ;; prefer file names near point
;; (setq ido-use-filename-at-point nil) ;; prefer file names near point

;; **** Dired
(require 'dired)
(require 'dired-x)
(setq dired-dwim-target t)
(setq dired-omit-files "^\\.?#\\|^\\..*")
;; (declare (special dired-x-hands-off-my-keys
;; 		  dired-bind-vm
;; 		  dired-omit-files-p))
;; (add-hook 'dired-load-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (declare (special dired-x-hands-off-my-keys
;; 			       dired-bind-vm))
;; 	     (load-library "dired-x")
;; 	     (setq dired-x-hands-off-my-keys nil
;; 		   dired-bind-vm t))))

(add-hook 'dired-mode-hook
	  (function (lambda ()
                      (auto-revert-mode))))
;; 		      ;; Set dired-x buffer-local variables here.  For example:
;; 		      (setq dired-omit-mode t)
;; 		      (setq dired-omit-files "^\\..*")
;; 		      )))

;; *** Ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


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
(defalias 'ss 'gjg/open-remote-shell)
(shell (get-buffer-create "sh1"))
(defalias 'rb 'rename-buffer)

(put 'dired-find-alternate-file 'disabled nil)

;; keep customized variables in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


