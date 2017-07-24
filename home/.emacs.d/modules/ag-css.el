(require 'use-package)

(setq css-indent-offset 2)

(use-package scss-mode
  :ensure t
  :defer t)

(use-package sass-mode
  :ensure t
  :defer t)

(use-package stylefmt
  :ensure t
  :init
  (stylefmt-enable-on-save))

(provide 'ag-css)
