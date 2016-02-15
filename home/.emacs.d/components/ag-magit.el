;;; ag-magit.el --- TODO
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

(provide 'ag-magit)
;;; ag-magit.el ends here
