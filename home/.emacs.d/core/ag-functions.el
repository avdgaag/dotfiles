;;; ag-functions.el --- define all custom elisp functions
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

(defun avdg-find-user-init-file ()
  "Edit the `init.el` file in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun avdg-duplicate-line()
  "Duplicate the current line by killing it and than yanking it tiwce."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun avdg-move-line-down ()
  "Move the current line one line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun avdg-move-line-up ()
  "Move the current line one line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(defun avdg-open-line-below ()
  "Insert a new line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun avdg-open-line-above ()
  "Insert a new line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun avdg-join-with-next-line ()
  "Like C-^ but join with next instead of previous line."
  (join-line -1))

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

(defun avdg-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Toggle between the two most recently open buffers on repeated invocations."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun avdg-beginning-of-string ()
  "Move to the beginning of a syntactic string."
  (interactive)
  (unless (in-string-p)
    (error "You must be in a string for this command to work"))
  (while (in-string-p)
    (forward-char -1))
  (point))

(defun avdg-end-of-string ()
  "Move to the end of a syntactic string."
  (interactive)
  (avdg-beginning-of-string)
  (forward-sexp)
  (point))

(defun avdg-cycle-quotes ()
  "Toggle single or double quotes in a syntactic string."
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (avdg-beginning-of-string)))
          (eos (save-excursion
                 (avdg-end-of-string)))
          (replacement-char ?\'))
      (goto-char bos)
      (when (eq (following-char) ?\')
        (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

(provide 'ag-functions)
;;; ag-functions.el ends here
