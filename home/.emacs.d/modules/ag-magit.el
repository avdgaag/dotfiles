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
  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
  :config
  (defun ag-visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote" "origin" "url"))
             (magit-get-current-branch))))

  (eval-after-load 'magit
    '(define-key magit-mode-map "v"
       #'ag-visit-pull-request-url)))

(provide 'ag-magit)
