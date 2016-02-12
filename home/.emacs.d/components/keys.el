;; Define key to open Magit status
(global-set-key (kbd "C-c g") 'magit-status)

;; Define key to quickly open init.el in another window
(defun find-user-init-file ()
  "Edit the `init.el` file in another window"
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Use C-c [arrow] to navigate windows
;; (windmove-default-keybindings)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Set up multiple cursors keybindings
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-c M") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)

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
