;;; ag-helm.el --- TODO
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
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-file-cache-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ff-file-name-history-use-recentf t)
  :config
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (helm-mode 1)
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)))

(use-package helm-ag
  :ensure t)

(provide 'ag-helm)
;;; ag-helm.el ends here
