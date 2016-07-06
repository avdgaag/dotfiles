(require 'use-package)

(use-package company
  :ensure t
  :diminish company-mode
  :defer t
  :init
  (setq company-backends
        (quote
         (company-elisp
          company-emoji
          company-css
          company-semantic
          company-etags
          company-files)))
  (setq completion-at-point-functions '(company-complete-common))
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (require 'company-emoji))

(use-package company-emoji
  :ensure t
  :defer t)

(provide 'ag-company)
