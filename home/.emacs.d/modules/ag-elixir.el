(require 'use-package)

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package flycheck-credo
  :ensure t
  :defer t
  :init
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup))
(provide 'ag-elixir)
