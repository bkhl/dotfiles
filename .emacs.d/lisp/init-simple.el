;;; init-simple.el -- configuration of basic editing commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package simple
  :custom
  ;; Highlight currently visited messages in `next-error' buffers.
  (next-error-message-highlight t)

  :config
  ;; Show line and column number in mode line
  (line-number-mode)
  (column-number-mode)

  ;; Highlight the region when the mark is active.
  (transient-mark-mode)

  ;; If a selection is active, typed text will replace the selection.
  (delete-selection-mode)

  ;; Disable indentation using tabs.
  (indent-tabs-mode nil)

  :bind
  ;; Make `Home'/`End' move to start/end of line.
  ("<home>" . move-beginning-of-line)
  ("<end>" . move-end-of-line)

  ;; This edits whitespace around point by cycling between leaving only one
  ;; space, deleting the space, and going back to what was there before.
  ("M-S-SPC" . cycle-spacing))

(provide 'init-simple)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-simple.el ends here
