;;; init-frame.el -- configuration of frame management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package frame
  :config
  ;; Function to enable emoji font. This is a test emoji: ☃
  (defun my/set-fontset-fonts (frame)
    (when (display-graphic-p frame)
      (dolist (font-spec '((#x2600 . #x26ff)
                           emoji))
        (set-fontset-font t font-spec
                          "Noto Color Emoji"))
      (remove-hook 'after-make-frame-functions
                   'my/set-fontset-fonts)))

  ;; This is run as a hook after the first graphical frame is created, as this
  ;; will otherwise not work when Emacs is started in daemon mode, or by
  ;; `emacsclient'.
  (add-hook 'after-make-frame-functions #'my/set-fontset-fonts)

  ;; Also run immediately for when we don't use `emacsclient'.
  (my/set-fontset-fonts (selected-frame)))

(provide 'init-frame)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-frame.el ends here
