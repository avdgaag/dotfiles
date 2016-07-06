(require 'use-package)

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.eex\\'" "\\.erb\\'")
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2))

(provide 'ag-web-mode)
