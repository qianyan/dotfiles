(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      package-enable-at-startup nil)

(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

(defconst initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold initial-gc-cons-threshold)))

;;; init screen
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; format & full screen
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq cider-overlays-use-font-lock t)
(use-package better-defaults
  :ensure t)

(use-package projectile
  :ensure t
  :init (add-hook 'after-init-hook 'projectile-global-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package clj-refactor
  :ensure t)

(use-package smex
  :ensure t
  :defer t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-copy-env "/usr/local/bin")
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

(use-package company
  :ensure t
  :defer t
  :config (global-company-mode))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c C-SPC" . ace-jump-mode))

(use-package helm-ag
  :ensure t)

(use-package helm-themes
  :ensure t
  :config (load-theme 'monokai t))

(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

(require 'icomplete)
(require 'init-paredit)
(require 'init-clojure-cider)
(require 'init-recentf)
(require 'init-ido)
(require 'init-powerline)
(require 'init-evil)
(require 'init-themes)

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
             ("\\(#_\\)("                ; anon funcs 2-1
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "€")
                        nil)))

            ("\\(#\\){"              ; sets
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "∈")
                       nil)))
            )))))

(add-hook 'after-init-hook 'my-replace-symbol)

;;; edit config file
(global-set-key (kbd "<f6>") (lambda()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))


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

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(set-default-font "Inconsolata 16")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#282828" :foreground "#fdf4c1"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
 '(term ((t (:foreground "ivory1"))))
 '(term-color-black ((t (:foreground "gray80"))))
 '(term-color-cyan ((t (:foreground "cyan2"))))
 '(term-color-green ((t (:foreground "OliveDrab3"))))
 '(term-color-yellow ((t (:foreground "gold1")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/org/work.org"))))

;;; global set
(global-set-key (kbd "M-f") 'toggle-frame-fullscreen)
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun shortened-path (path max-len)
      "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
      (let* ((components (split-string (abbreviate-file-name path) "/"))
             (len (+ (1- (length components))
                     (reduce '+ components :key 'length)))
             (str ""))
        (while (and (> len max-len)
                    (cdr components))
          (setq str (concat str (if (= 0 (length (car components)))
                                    "/"
                                  (string (elt (car components) 0) ?/)))
                len (- len (1- (length (car components))))
                components (cdr components)))
        (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun rjs-eshell-prompt-function ()
  (concat (shortened-path (eshell/pwd) 10)
          (if (= (user-uid) 0) " # " " $ ")))

(setq eshell-prompt-function 'rjs-eshell-prompt-function)
