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
(setq whitespace-style '(
                         face
                         tabs
                         empty
                         trailing
                         lines-tail
                         space-before-tab
                         space-after-tab
                         tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

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

(defun avdg-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (avdg-indent-buffer)
        (message "Indented buffer.")))))
(global-set-key (kbd "C-M-\\") 'avdg-indent-region-or-buffer)

(defun avdg-duplicate-line()
  "Duplicate the current line by killing it and than yanking it tiwce."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))
(global-set-key (kbd "s-d") 'avdg-duplicate-line)

(provide 'ag-editor)
