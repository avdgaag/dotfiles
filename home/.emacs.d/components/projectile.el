;;; projectile.el --- configure projectile

;;; Commentary:

;;; Code:

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-tags-command "ctags -Re -f \"%s\" %s")
(setq projectile-tags-file-name "tags")
(helm-projectile-on)

(defun my-expand-completion-table (orig-fun &rest args)
  "Extract all symbols from COMPLETION-TABLE before calling projectile--tags."
  (let ((completion-table (all-completions "" (car args))))
    (funcall orig-fun completion-table)))

(advice-add 'projectile--tags :around #'my-expand-completion-table)

;;; projectile.el ends here
