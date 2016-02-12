;;; magit.el --- configure magit

;;; Commentary:

;;; Code:

(setq magit-fetch-arguments (quote ("--prune")))
(setq magit-merge-arguments (quote ("--no-ff")))
(setq magit-pull-arguments (quote ("--rebase")))
(setq magit-push-arguments (quote ("--set-upstream")))

;;; magit.el ends here
