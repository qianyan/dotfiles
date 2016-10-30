(use-package haskell-mode
  :ensure t
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
 (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-compile) 
 (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-compile)
  )

(provide 'init-haskell)
