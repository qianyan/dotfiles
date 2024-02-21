(use-package company
             :ensure t
             :defer t
             :config
             (global-company-mode)
             (setq company-idle-delay 0)
             (setq company-show-numbers t))

(use-package company-tabnine
  :requires (company)
  :ensure t)

(add-to-list 'company-backends #'company-tabnine)

(provide 'init-ai-assistant)
