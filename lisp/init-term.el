;;; Eshell
(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '()))

(provide 'init-term)
