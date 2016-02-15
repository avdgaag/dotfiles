(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  (setq avy-style 'at-full)
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-word-or-subword-1)))
