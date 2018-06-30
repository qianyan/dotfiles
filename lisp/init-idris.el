(use-package idris-mode
  :mode (("\\.idr$" . idris-mode)
         ("\\.lidr$" . idris-mode))
  :ensure t 
  :defer t)

(provide 'init-idris)
