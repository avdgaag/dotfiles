(require 'use-package)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

(provide 'ag-yasnippet)
