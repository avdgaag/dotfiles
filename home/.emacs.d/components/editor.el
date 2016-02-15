;;; editor.el -- configure general editor settings

;;; Commentary:

;;; Code:

;; No need for backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Default tab width and usage
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set a default line width
(setq-default fill-column 80)

;; Newline at end of file
(setq require-final-newline t)

;; Delete selections with a keypress
(delete-selection-mode t)

;; Revert buffers automatically
(global-auto-revert-mode t)

;; Enable clipboard copy/paste
(setq x-select-enable-clipboard t)

;; Always use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Automatically make scripts exectable if they contain a shebang line
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Auto-pair parentheses
(electric-pair-mode +1)

;; Enable subword-mode in all programming modes to allow navigation
;; over camelCased words
(add-hook 'prog-mode-hook 'subword-mode)

;;; editor.el ends here
