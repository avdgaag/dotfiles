;;; helm.el --- configuration for Helm

;;; Commentary:

;;; Code:

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)

(setq helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;;; helm.el ends here
