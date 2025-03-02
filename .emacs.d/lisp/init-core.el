;;; init-core.el -- basic Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This is configuration of functionality defined in `C source code'.

;;; Code:

;; Disable GC while minibuffer is open, and enabled again when it is closed.
;; This helps prevent hanging while working in the minibuffer.
(add-hook 'minibuffer-setup-hook
          (defun my/disable-gc ()
            (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (defun my/default-gc ()
            (setq gc-cons-threshold my/default-gc-cons-threshold)))

;; Start with an empty scratch buffer.
(setopt inhibit-startup-screen t
        initial-scratch-message nil)

;; Disable warning bell, both the default audio one and the visual one.
(setopt ring-bell-function 'ignore)

;; Set window title including current buffer or filename, along with system name.
;; Use a straight or squiggly line to show if the buffer has modifications.
(setq frame-title-format
      '(
        "%b"
        (:eval (if (buffer-modified-p) " ⁓ " " — "))
        (:eval (system-name))))

;; Enable restoring exact window size, rather than rounding to an exact number
;; of lines or columns. This is needed to be able to restore back from
;; fullscreen to original frame size in Gnome.
(setopt frame-resize-pixelwise t)

;; Scrolling behaviour when moving cursor: When the cursor moves close to the
;; edge of the screen, scroll only one line at time, but try to keep 5 rows
;; within view.
(setopt scroll-conservatively 101
        scroll-margin 5)

;; Use bar cursor
(setopt cursor-type 'bar)

;; Make yes/no prompts shorter.
(setopt use-short-answers t)

;; Change the default keybinding for killing a buffer, `C-x k', so that it kills
;; the current buffer rather than prompting for a buffer. Instead `C-x K' is
;; used for the previous default.
(global-set-key
 (kbd "C-x k")
 (defun my/kill-this-buffer ()
   "Kill current buffer, prompting if there are unsaved changes."
   (interactive)
   (kill-buffer)))
(global-set-key (kbd "C-x K") #'kill-buffer)

(provide 'init-core)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-core.el ends here
