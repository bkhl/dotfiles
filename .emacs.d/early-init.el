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
