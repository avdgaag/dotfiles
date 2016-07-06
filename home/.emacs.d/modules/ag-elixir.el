(require 'use-package)

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(provide 'ag-elixir)
