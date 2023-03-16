;;; early-init.el -*- no-byte-compile: t -*-

;; Increase number of bytes of consing between garbage collections. This appears
;; to increase performance at the cost of some memory increase. This is done
;; first to decrease start-up time.
(setq gc-cons-threshold (* 24 1024 1024))

;; Disable implied resizing of frames when display settings change. This speeds
;; up startup by skipping frame resizing when e.g. font settings change.
(setq frame-inhibit-implied-resize t)

;; Disable tool bars and scroll bar. Doing this here in early-init.el means they
;; won't appear during the startup process.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)

;; Prevent stale Elisp bytecode file from shadowing more up-to-date source
;; files.
(setq load-prefer-newer t)

;; Enable the auto-compile package.
(add-to-list 'load-path
             (expand-file-name (concat user-emacs-directory
                                       "site-lisp/auto-compile")))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Disable package.el, as I don't use it.
(setq package-enable-at-startup nil)
