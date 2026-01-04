;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent "Flash of Unstyled Content" by disabling UI elements early
(setq package-enable-at-startup nil)
(setq site-run-file nil)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; On MacOS, define the meta key up front
(setq frame-inhibit-implied-resize t)
(setq ns-command-modifier (quote meta))

(provide 'early-init)
