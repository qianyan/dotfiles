(require 'init-clojure)
(require 'company)

(use-package cider
             :ensure t
             :config
             (setq nrepl-popup-stacktraces nil)
             (add-hook 'cider-mode-hook 'eldoc-mode)
             (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
             ;; Replace return key with newline-and-indent when in cider mode.
             (add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
             (add-hook 'cider-mode-hook #'company-mode)
             (add-hook 'cider-repl-mode-hook 'subword-mode)
             (add-hook 'cider-repl-mode-hook 'paredit-mode)
             (add-hook 'cider-repl-mode-hook #'company-mode)
             (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(provide 'init-clojure-cider)
