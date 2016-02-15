(use-package magit
  :ensure t
  :init
  (setq magit-fetch-arguments (quote ("--prune")))
  (setq magit-merge-arguments (quote ("--no-ff")))
  (setq magit-pull-arguments (quote ("--rebase")))
  (setq magit-push-arguments (quote ("--set-upstream")))
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
