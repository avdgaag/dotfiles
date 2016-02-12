;;; avy.el --- configure avy package

;;; Commentary:

;;; Code:

(setq avy-background t)
(setq avy-style 'at-full)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

;;; avy.el ends here
