(require 'use-package)

(use-package smartparens
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config))

(provide 'ag-smartparens)
