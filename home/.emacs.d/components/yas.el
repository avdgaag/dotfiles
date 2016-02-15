(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate t))))
