;;; ui.el -- UI customizations

;;; Commentary:

;;; Code:

;; Use Y/N intead of Yes/No
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep a couple of lines of screen context when scrolling
(setq scroll-margin 3 scroll-preserve-screen-position 1)

;; Customize line spacing
(setq-default line-spacing 1)

;; Use 4 columns for line numbers and then a space
(setq linum-format "%4d ")

;; When using GUI, do not open new frames but re-use existing frames
;; when opening new files.
(setq ns-pop-up-frames nil)

;; Always show line and column numbers
(global-linum-mode 1)
(column-number-mode 1)

;; Don't show startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Hide unnecessary GUI chrome
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(show-paren-mode t)

(setq ns-function-modifier 'hyper)
(menu-bar-mode +1)

;; do not blink
(blink-cursor-mode -1)

;;; ui.el ends here
