(setq ns-function-modifier 'hyper)
(menu-bar-mode +1)

;; Re-use existing frames to open new buffers
(setq ns-pop-up-frames nil)

;; On OSX, we delete files (in dired mode) by moving them to the trash.
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/")

;; Sane scrolling with the mouse
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

;; Fix emoji display
(defun ag-set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend))

(when (fboundp 'set-fontset-font)
  (ag-set-emoji-font nil)
  (add-hook 'after-make-frame-functions 'ag-set-emoji-font))


;; Some sane keyboard shortcuts for OSX conventions
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-˙") 'ns-do-hide-others)

(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(provide 'ag-osx)
