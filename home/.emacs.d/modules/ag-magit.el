(require 'use-package)

(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c G" . magit-file-popup)
   ("C-c M-g" . magit-dispatch-popup))
  :init
  (setq magit-fetch-arguments (quote ("--prune")))
  (setq magit-merge-arguments (quote ("--no-ff")))
  (setq magit-pull-arguments (quote ("--rebase")))
  (setq magit-push-arguments (quote ("--set-upstream")))
  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))))

(provide 'ag-magit)
