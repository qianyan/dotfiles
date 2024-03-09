
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)  
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :requires (nerd-icons)
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'init-icons)
