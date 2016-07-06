(require 'use-package)

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm")))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  :config
  (require 'helm-config)
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h f" . helm-apropos)
   ("C-h r" . helm-info-emacs)
   ("C-h C-l" . helm-locate-library)))

(provide 'ag-helm)
