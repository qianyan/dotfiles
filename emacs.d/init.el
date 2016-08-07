(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      package-enable-at-startup nil)

(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      midje-mode
                      cider
                      clj-refactor
                      paredit
                      rainbow-delimiters
                      company
                      ido-vertical-mode
                      zenburn-theme
                      monokai-theme
                      idea-darkula-theme
                      jazz-theme
                      planet-theme
                      obsidian-theme
                      find-file-in-project
                      kibit-helper
                      highlight-symbol
                      helm-ag
                      helm-themes))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(add-to-list 'exec-path "/usr/local/bin")
;;; set PATH to emacs shell
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":"
         "/usr/local/bin"))

;;; format & full screen
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-agenda-files (quote ("~/Documents/org/work.org"))))
;; cider mode
;;(setq nrepl-log-messages nil)
(setq cider-overlays-use-font-lock t)
(require 'icomplete)

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
(add-hook 'clojure-mode-hook #'midje-mode)

(require 'rainbow-delimiters)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; clj-refactor for clojure
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;;; cider
(require 'company)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;; open recent file
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; highlight symbols
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;;themes
(require 'helm-config)
(require 'helm-themes)
;;(load-theme 'sanityinc-solarized-dark)

;;(load-theme 'zenburn t)

(load-theme 'monokai t)
;;(load-theme 'idea-darkula t)
;;(load-theme 'jazz t)
;;(load-theme 'planet t)
;;(load-theme 'obsidian t)

;;(require 'parenface)
;;(set-face-foreground 'paren-face "DimGray")
;;;2A2A2A
;;(set-foreground-color "ivory")
;;(set-background-color "#2A2A2A")
(set-default-font "Inconsolata 16")

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
                                       (match-end 1) "ƒ")
                       nil)))
            ("\\(#\\){"              ; sets
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "∈")
                       nil)))
             )))))
(add-hook 'after-init-hook 'my-replace-symbol)

;;; edit config file
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))


;;;(add-to-list 'load-path "/Users/qianyan/.emacs.d/neotree")
;;;(require 'neotree)
;;;(global-set-key [f8] 'neotree-toggle)

;;; specific for per project
(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; ido mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background "#e5b7c0")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background "#e52b50"
                    :foreground "white")
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground "#b00000")
(ido-vertical-mode 1)

;;; unit tests
;;; (setq cider-test-show-report-on-success t) ; whatever it fails or success, show report anyway.
;(cider-auto-test-mode 1)

;;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; add org directory to agenda.
(setq org-agenda-files (quote ("~/Documents/org")))
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;;; multiple cursors
(global-set-key (kbd "C-M") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; switch windows with SHIFT + ARROW
(progn
  (require 'windmove)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))
