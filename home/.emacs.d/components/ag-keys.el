;;; ag-keys.el --- TODO
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
;;; keys.el --- custom keybindings and functions

;;; Commentary:

;;; Code:

;; Define key to open Magit status
(global-set-key (kbd "C-c g") 'magit-status)

;; Define key to quickly open init.el in another window
(defun find-user-init-file ()
  "Edit the `init.el` file in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Use C-c [arrow] to navigate windows
;; (windmove-default-keybindings)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(defun duplicate-line()
  "Duplicate the current line by killing it and than yanking it tiwce."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))
(global-set-key (kbd "s-d") 'duplicate-line)

;; Define C-S-F to full screen window
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)

(defun ag-move-line-down ()
  "Move the current line one line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun ag-move-line-up ()
  "Move the current line one line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'ag-move-line-down)
(global-set-key (kbd "<C-S-up>") 'ag-move-line-up)

(defun ag-open-line-below ()
  "Insert a new line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun ag-open-line-above ()
  "Insert a new line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'ag-open-line-below)
(global-set-key (kbd "<C-S-return>") 'ag-open-line-above)

;; Code alignment
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

(defun ag-join-with-next-line ()
  "Like C-^ but join with next instead of previous line."
  (join-line -1))

(global-set-key (kbd "M-j") 'ag-join-with-next-line)

(defun ag-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ag-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (ag-indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'ag-indent-region-or-buffer)

(defun ag-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Toggle between the two most recently open buffers on repeated invocations."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'ag-switch-to-previous-buffer)
;;; keys.el ends here

(provide 'ag-keys)
;;; ag-keys.el ends here
