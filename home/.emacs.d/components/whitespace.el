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
