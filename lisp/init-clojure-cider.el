(require 'init-clojure)
(require 'company)

(use-package cider
             :ensure t
             :config
             (setq nrepl-popup-stacktraces nil)
             (add-hook 'cider-mode-hook 'eldoc-mode)
             (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
             ;; Replace return key with newline-and-indent when in cider mode.
             (add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
             (add-hook 'cider-mode-hook 'company-mode)
             (add-hook 'cider-repl-mode-hook 'subword-mode)
             (add-hook 'cider-repl-mode-hook 'paredit-mode)
             (add-hook 'cider-repl-mode-hook 'company-mode)
             (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

(defun my-replace-symbol ()
  (dolist (mode '(clojure-mode clojurescript-mode cider-mode))
    (eval-after-load mode
      (font-lock-add-keywords
       mode '(
              ("(\\(fn\\)[\[[:space:]]" ; anon funcs 1
               (0 (progn (compose-region (match-beginning 1)
                                         (match-end 1) "λ")
                         nil)))
              ("\\(#\\)("               ; anon funcs 2
               (0 (progn (compose-region (match-beginning 1)
                                         (match-end 1) "ƒ")
                         nil)))
              ("\\(#_\\)("              ; anon funcs 2-1
               (0 (progn (compose-region (match-beginning 1)
                                         (match-end 1) "€")
                         nil)))

              ("\\(#\\){"               ; sets
               (0 (progn (compose-region (match-beginning 1)
                                         (match-end 1) "∈")
                         nil)))
              )))))
(add-hook 'after-init-hook 'my-replace-symbol)
(provide 'init-clojure-cider)
