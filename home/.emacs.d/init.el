(load-library "~/.emacs.d/components/packages")
(load-library "~/.emacs.d/components/ui")
(load-library "~/.emacs.d/components/projectile")
(load-library "~/.emacs.d/components/keys")
(load-library "~/.emacs.d/components/ruby")

;; Ensure we can load the actual binaries we want to use
(push "/usr/local/bin" exec-path)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f4020e085253f630b9dedb0bb2bea7dc574100b7993cac011f94097c4f92fd13" default)))

 ;; Configuration for projectile to not look for `TAGS` file but a `tags` file.
 '(projectile-tags-file-name "tags")

 ;; Configure rspec-mode to not use `bundle exec` or `rake` but just plain `rspec`
 ;; since we'll be using binstubs anyway.
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-rake-when-possible nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#F7F7F7" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Source Code Pro for Powerline")))))

;; Use the Smyx color scheme
(load-theme 'smyx)

;; No need for backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Default tab width and usage
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set a default line width
(setq-default fill-column 80)

;; Enable clipboard copy/paste
(setq x-select-enable-clipboard t)

;; Always use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; On Mac OSX, delete files by moving them to ~/.Tash
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")))

(put 'upcase-region 'disabled nil)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(setq whitespace-style '(
                         face
                         tabs
                         trailing
                         lines-tail
                         space-before-tab
                         newline
                         empty
                         space-after-tab
                         tab-mark
                         newline-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
