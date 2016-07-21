(tool-bar-mode -1)
(setq-default line-spacing 1)
(setq linum-format "%4d ")
(show-paren-mode t)

;; Navigate windows
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Disable annoying UI features
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;; Sane scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Always show our location in the mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Hide scroll bar
(scroll-bar-mode -1)

;; Use simpler Y/N prompt
(fset 'yes-or-no-p 'y-or-n-p)

(defun avdg-toggle-line-numbers ()
  "Turn line numbers on or off."
  (interactive)
  (if (and (boundp 'linum-mode) linum-mode)
      (linum-mode 0)
      (linum-mode 1)))
(global-set-key (kbd "C-c l") 'avdg-toggle-line-numbers)

(defun avdg-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Toggle between the two most recently open buffers on repeated invocations."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'avdg-switch-to-previous-buffer)

(provide 'ag-ui)
