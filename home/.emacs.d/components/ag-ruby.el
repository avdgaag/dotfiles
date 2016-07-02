;;; ag-ruby.el --- TODO
;;
;; Author: Arjan van der Gaag <arjan@arjanvandergaag.nl>
;; URL: http://arjanvandergaag.nl
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(use-package enh-ruby-mode
  :commands enh-ruby-mode
  :diminish abbrev-mode
  :ensure t
  :interpreter "ruby"
  :mode ("\\.rb$"
         "\\.rake$"
         "Rakefile$"
         "\\.gemspec$"
         "\\.ru$"
         "Gemfile$"
         "Vagrantfile$"
         ".simplecov$")
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-hanging-paren-deep-indent-level 2))

(defun projectile-rails-or-hanami-on ()
  "Activate either projectile-rails or projectile-hanami."
  (if (projectile-hanami-applicable-p)
      (projectile-hanami-mode +1)
    (projectile-rails-on)))

(use-package projectile-rails
  :ensure t
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-or-hanami-on))

(use-package rbenv
  :diminish rbenv-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'global-rbenv-mode)
  (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding))

(use-package robe
  :diminish robe-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package yard-mode
  :diminish yard-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package rspec-mode
  :commands rspec-mode
  :ensure t
  :init
  (setq rspec-use-bundler-when-possible nil)
  (setq rspec-use-rake-when-possible nil))

(use-package bundler
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package ruby-refactor
  :ensure t)

(use-package rubocop
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :commands haml-mode
  :ensure t)

(use-package slim-mode
  :commands slim-mode
  :ensure t)

(use-package minitest
  :ensure t)

(require 'rails-sql-mode)
(add-hook 'projectile-rails-mode 'rails-sql-mode)

(provide 'ag-ruby)
;;; ag-ruby.el ends here
