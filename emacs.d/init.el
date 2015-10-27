(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      paredit
                      rainbow-delimiters
                      company
                      find-file-in-project))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(add-to-list 'exec-path "/usr/local/bin")

;;; format & full screen
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; paredit mode
(require 'paredit)
(autoload 'enable-paredit-mode "paredit")
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(require 'rainbow-delimiters)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(require 'company)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; open recent file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'parenface)
(set-face-foreground 'paren-face "DimGray")
;;;2A2A2A
(set-foreground-color "ivory")
(set-background-color "#2A2A2A")
(set-default-font "Inconsolata 14")

(defun my-replace-symbol ()
  (dolist (mode '(clojure-mode clojurescript-mode cider-mode))
    (eval-after-load mode
      (font-lock-add-keywords
       mode '(
             ("(\\(fn\\)[\[[:space:]]"  ; anon funcs 1
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "λ")
                       nil)))
             ("\\(#\\)("                ; anon funcs 2
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "λ")
                       nil)))
             )))))

(add-hook 'after-init-hook 'my-replace-symbol)
