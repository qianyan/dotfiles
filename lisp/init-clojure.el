(require 'clj-refactor)
(require 'rainbow-delimiters)

(use-package midje-mode
  :ensure t)

(defun my-clj-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package clojure-mode
             :ensure t
             :config
             (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
             (add-hook 'clojure-mode-hook #'subword-mode)
             (add-hook 'clojure-mode-hook #'midje-mode)
             (add-hook 'clojure-mode-hook #'my-clj-refactor-mode-hook)
             (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(provide 'init-clojure)
