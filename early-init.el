;;; early-init.el --- Emacs-Solo (no external packages) Configuration  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Early init configuration for Emacs-Solo
;;
;;; Code:

;; Hack to avoid being flashbanged
(defun emacs-solo/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'default nil :background "#0d0e1b"))

(emacs-solo/avoid-initial-flash-of-light)

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
