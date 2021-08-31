(org-babel-load-file (concat user-emacs-directory "config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode emmet-mode web-mode ob-restclient restclient sqlformat company-box org-tempo treemacs-magit treemacs-projectile treemacs-evil treemacs org-superstar olivetti htmlize ox-pandoc org-roam vterm-toggle vterm lsp-ivy lsp-ui lsp-mode rg avy flycheck doom-modeline yasnippet ibuffer-projectile counsel ivy-rich ivy which-key general forge magit projectile-rails minitest rspec-mode rake bundler robe rubocop inf-ruby yard-mode projectile ws-butler ws-bulter diminish f fd-dired dired-x diredfl dired doom-themes use-package))
 '(safe-local-variable-values
   '((sql-postgres-login-params
      '((user :default
              (getenv "PGUSER"))
        (database :default
                  (getenv "PGDATABASE"))
        (server :default
                (getenv "PGHOST"))
        (port :default 5432)))
     (rspec-use-bundler-when-possible))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
