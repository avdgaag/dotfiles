;;; ag-keys.el --- define all custom keybindings
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

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c I") 'avdg-find-user-init-file)

;; Use C-c [arrow] to navigate windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "s-d") 'avdg-duplicate-line)

;; Define C-S-F to full screen window
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)


(global-set-key (kbd "<C-S-down>") 'avdg-move-line-down)
(global-set-key (kbd "<C-S-up>") 'avdg-move-line-up)

(global-set-key (kbd "<C-return>") 'avdg-open-line-below)
(global-set-key (kbd "<C-S-return>") 'avdg-open-line-above)

;; Code alignment
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

(global-set-key (kbd "M-j") 'avdg-join-with-next-line)

(global-set-key (kbd "C-M-\\") 'avdg-indent-region-or-buffer)

(global-set-key (kbd "C-c b") 'avdg-switch-to-previous-buffer)

(provide 'ag-keys)
;;; ag-keys.el ends here
