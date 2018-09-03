;; Packages
(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq user-full-name "Arjan van der Gaag"
      user-mail-address "arjan.vandergaag@gmail.com")

;; General settings
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Editor settings
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq tab-always-indent 'complete)
;; (global-hl-line-mode +1)
(add-hook 'prog-mode-hook 'subword-mode)
(setq x-select-enable-clipboard t)

;; Sane whitespace handling
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 80)
(setq require-final-newline t)

;; Store backup files in tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Dired
(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (require 'dired-x))

(delete-selection-mode t)
(global-auto-revert-mode t)
(setq tab-always-indent 'complete)
(global-hl-line-mode +1)
(add-hook 'prog-mode-hook 'subword-mode)
(setq x-select-enable-clipboard t)

;; UI settings
(tool-bar-mode -1)
(setq-default line-spacing 3)
(setq linum-format "%4d ")
(show-paren-mode t)

;; Navigate windows
(use-package windmove
  :config
  (windmove-default-keybindings))

;; Disable annoying UI features
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;; Sane scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Always show our location in the mode line
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Hide scroll bar
(scroll-bar-mode -1)

;; Use simpler Y/N prompt
(fset 'yes-or-no-p 'y-or-n-p)

(defun avdg-toggle-line-numbers ()
  "Turn line numbers on or off."
  (interactive)
  (if (and (boundp 'linum-mode) linum-mode)
      (linum-mode 0)
      (linum-mode 1)))
(global-set-key (kbd "C-c l") 'avdg-toggle-line-numbers)

(defun avdg-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Toggle between the two most recently open buffers on repeated invocations."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'avdg-switch-to-previous-buffer)

;; Custom functions
(defun avdg-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun avdg-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (avdg-indent-buffer)
        (message "Indented buffer.")))))

(defun avdg-duplicate-line()
  "Duplicate the current line by killing it and than yanking it tiwce."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun avdg-smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun avdg-toggle-eshell-buffer ()
  "Create and/or siwtch to a Eshell buffer, or kill it."
  (interactive)
  (let ((shell-window (get-buffer-window "*eshell*"))
        (shell-buffer (get-buffer "*eshell")))
    (if shell-window
        (progn
          (bury-buffer shell-buffer)
          (delete-window shell-window))
      (progn
        (select-window (split-window-sensibly))
        (if shell-buffer
            (switch-to-buffer shell-buffer)
          (eshell))))))

(defun avdg-open-newline-above (arg)
  "Move to the previous line and then open ARG new lines below."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun avdg-open-newline-below (arg)
  "Open ARG new lines below the current line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (indent-according-to-mode))

(defun avdg-move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line."
  (let ((remember-point (point)))
    (goto-char (point-max))
    (if (not (bolp)) (newline))
    (goto-char remember-point)
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark))
               (exchange-point-and-mark))
           (let ((column (current-column))
                 (text (delete-and-extract-region (point) (mark))))
             (forward-line arg)
             (move-to-column column t)
             (set-mark (point))
             (insert text)
             (exchange-point-and-mark)
             (setq deactivate-mark nil)))
          (t (let ((column (current-column)))
               (beginning-of-line)
               (when (or (> arg 0) (not (bobp)))
                 (forward-line 1)
                 (when (or (< arg 0) (not (eobp)))
                   (transpose-lines arg))
                 (forward-line -1))
               (move-to-column column t))
             ))))

(defun avdg-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (avdg-move-text-internal (- arg)))

(defun avdg-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (avdg-move-text-internal arg))

;; Keymaps
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-s-∑") 'toggle-truncate-lines)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-M-\\") 'avdg-indent-region-or-buffer)
(global-set-key (kbd "s-d") 'avdg-duplicate-line)
(global-set-key [remap move-beginning-of-line] 'avdg-smarter-move-beginning-of-line)
(global-set-key (kbd "s-§") 'avdg-toggle-eshell-buffer)
(global-set-key (kbd "M-o") 'avdg-open-newline-below)
(global-set-key (kbd "M-O") 'avdg-open-newline-above)
(global-set-key (kbd "s-<up>") 'avdg-move-text-up)
(global-set-key (kbd "s-<down>") 'avdg-move-text-down)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; OSX settings
(setq ns-function-modifier 'hyper)
(menu-bar-mode +1)

;; Re-use existing frames to open new buffers
(setq ns-pop-up-frames nil)

;; On OSX, we delete files (in dired mode) by moving them to the trash.
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/")

;; Sane scrolling with the mouse
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-˙") 'ns-do-hide-others)

;; Use Emacs server mode for quick init
(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/.emacs.d/")
(load-theme 'tomorrow-night)

(diminish 'whitespace-mode)

(require 'use-package)

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package smartparens
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-format-hook
            (lambda () (if (projectile-project-p)
                           (setq elixir-format-arguments
                                 (list "--dot-formatter"
                                       (concat (locate-dominating-file
                                                buffer-file-name
                                                ".formatter.exs")
                                               ".formatter.exs")))
                         (setq elixir-format-arguments nil)))))

(use-package alchemist
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(setq css-indent-offset 2)

(use-package scss-mode
  :ensure t
  :defer t)

(use-package sass-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (setq js2-mode-assume-strict t)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  :mode "\\.js\\'")

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

(use-package yaml-mode
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm")))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  :config
  (require 'helm-config)
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h f" . helm-apropos)
   ("C-h r" . helm-info-emacs)
   ("C-h C-l" . helm-locate-library)))

(use-package helm-projectile :ensure t :defer t)

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode +1)
  (helm-projectile-on))

(use-package ag :ensure t :defer t)

(use-package helm-ag :ensure t :defer t)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-backends
        (quote
         (company-elisp
          company-css
          company-semantic
          company-etags
          company-files)))
  (setq completion-at-point-functions '(company-complete-common))
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package enh-ruby-mode
  :ensure t
  :interpreter "ruby"
  :mode ("\\.rb$"
         "\\.ruby$"
         "\\.rake$"
         "Rakefile$"
         "\\.gemspec$"
         "\\.ru$"
         "Gemfile$"
         "Vagrantfile$"
         ".simplecov$")
  :init
  (setq enh-ruby-deep-indent-paren nil))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.eex\\'" "\\.erb\\'")
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elm-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (setq elm-format-on-save t)
  (setq elm-indent-offset 2)
  (setq elm-tags-on-save t)
  (setq elm-sort-imports-on-save t)
  :config
  (add-to-list 'company-backends 'company-elm))

(use-package avy
  :ensure t
  :bind ("s-." . avy-goto-word-or-subword-1))

(use-package erlang
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode))

(use-package flycheck-elm
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(use-package markdown-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C-c M" . mc/edit-ends-of-lines)
         ("s-]" . mc/mark-next-like-this)
         ("s-[" . mc/mark-previous-like-this)
         ("s-}" . mc/mark-all-like-this)
         ("s-{" . mc/mark-all-dwim)
         )
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package haskell-mode
  :ensure t
  :bind (("<f8>" . 'haskell-navigate-imports)
         ("C-c C-l" . 'haskell-process-load-or-reload)
         ("C-c C-z" . 'haskell-interactive-switch)
         ("C-c C-n C-t" . 'haskell-process-do-type)
         ("C-c C-n C-i" . 'haskell-process-do-info)
         ("C-c C-n C-c" . 'haskell-process-cabal-build)
         ("C-c C-n c" . 'haskell-process-cabal)
         ("C-c C-o" . 'haskell-compile))
  :custom
  (haskell-tags-on-save t)
  (haskell-process-type 'stack-ghci)
  (haskell-process-suggest-remove-import t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-stylish-on-save t)
  :config
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hindent
  :ensure t)

(use-package company-ghc
  :ensure t
  :config
  (custom-set-variables '(company-ghc-show-info t))
  (add-to-list 'company-backends 'company-ghc))

(use-package diminish
  :ensure t)
