(delete-selection-mode t)
(global-auto-revert-mode t)
(setq tab-always-indent 'complete)
(global-hl-line-mode +1)
(add-hook 'prog-mode-hook 'subword-mode)
(setq x-select-enable-clipboard t)

;; Sane whitespace handling
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 80)
(setq require-final-newline t)

;; Store backup files in tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Dired
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(require 'dired-x)

;; Whitespace
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; Make files with a shebang executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Sane compilation mode
(require 'compile)
(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(setq compilation-scroll-output 'first-error)

;; Enable some disabled-by-default commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'ag-editor)
