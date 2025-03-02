;;; init-calendar.el -- configuration of calendar functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package calendar
  :config
  ;; Set preferred dateformat.
  (calendar-set-date-style 'iso))

(provide 'init-calendar)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-calendar.el ends here
