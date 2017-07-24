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

(provide 'ag-functions)
