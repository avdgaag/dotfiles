(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-tags-command "ctags -Re -f \"%s\" %s")
  (setq projectile-tags-file-name "tags")
  :config
  (projectile-global-mode)

  (defun ag-expand-completion-table (orig-fun &rest args)
    "Extract all symbols from COMPLETION-TABLE before calling projectile--tags."
    (let ((completion-table (all-completions "" (car args))))
      (funcall orig-fun completion-table)))

  (advice-add 'projectile--tags :around #'ag-expand-completion-table))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package ggtags
  :ensure t)
