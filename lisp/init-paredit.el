(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit")
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(defvar paredit-minibuffer-command '(eval-expression
                                     pp-eval-expression
                                     eval-expression-with-eldoc
                                     ibuffer-do-eval
                                     ibuffer-do-view-and-eval))

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-command)
      (enable-paredit-mode)))


(provide 'init-paredit)
