(use-package elixir-mode
  :commands elixir-mode
  :ensure t)

(use-package alchemist
  :commands alchemist-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))
