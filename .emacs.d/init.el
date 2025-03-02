;;; init.el -- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Main Emacs configuration file.

;;; Code:

;; Reduce `gc-cons-threshold' to my default, as it was changed to be as large as
;; possible in `early-init.el'.
(setq gc-cons-threshold (eval-when-compile (* 32 1024 1024)))

;; Set personal information
(setopt user-full-name "Björn Lindström"
        user-mail-address "bkhl@elektrubadur.se")

;; Reduce `gc-cons-threshold' to the value I normally want while using Emacs, as
;; it was changed to be as large as possible in `early-init.el'.
(setq gc-cons-threshold (eval-when-compile (* 32 1024 1024)))

;; Load configuration files from `.emacs.d/lisp'.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Startup configuration.
(require 'init-startup)

;; Variables defined in `C source code'
(require 'init-core)

;; Initialize use-package, used for most configuration
(require 'init-use-package)

;; Packages bundled with Emacs.
(require 'init-bookmark)
(require 'init-calendar)
(require 'init-custom)
(require 'init-files)
(require 'init-frame)
(require 'init-paren)
(require 'init-pixel-scroll)
(require 'init-savehist)
(require 'init-saveplace)
(require 'init-server)
(require 'init-simple)
(require 'init-windmove)
(require 'init-window)
(require 'init-winner)

;; Add-on packages

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
