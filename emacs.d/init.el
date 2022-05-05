(require 'package)

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-enable-at-startup nil)

(package-initialize)
(package-refresh-contents)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")
;;; pin following packages to melpa-stable
(add-to-list 'package-pinned-packages '(elixir-mode . "melpa-stable") t)

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

(use-package cnfonts
             :ensure t)

(use-package cal-china-x
             :ensure t)

(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays))

(use-package w3m
             :ensure t)

(use-package projectile
             :ensure t
             :init (add-hook 'after-init-hook 'projectile-global-mode))

(use-package hydra
             :ensure t
             :pin melpa-stable)

(use-package rainbow-delimiters
             :ensure t)

(use-package clj-refactor
             :ensure t
             :pin melpa-stable)


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

(use-package helm-ag
             :ensure t)

(use-package helm-themes
             :ensure t
             :config (load-theme 'monokai t))

(use-package avy
             :ensure t
             :bind ("C-," . avy-goto-char)
             ("C-'" . avy-goto-char-2))

(use-package prodigy
             :ensure t
             :commands (prodigy)
             :bind* (("M-m s b" . prodigy))
             :init
             (prodigy-define-tag
               :name 'blog
               :ready-message "Serving blog. Ctrl-C to shutdown server")
             (prodigy-define-service
               :name "hexo generate"
               :command "hexo"
               :args '("g")
               :cwd "~/Sync/blog"
               :tags '(blog)
               :kill-signal 'sigkill)
             (prodigy-define-service
               :name "hexo serve"
               :command "hexo"
               :args '("s")
               :cwd "~/Sync/blog"
               :tags '(blog)
               :kill-signal 'sigkill
               :kill-process-buffer-on-stop t)
             (prodigy-define-service
               :name "hexo deploy"
               :command "hexo"
               :args '("d")
               :cwd "~/Sync/blog"
               :tags '(blog)
               :kill-signal 'sigkill))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; conflict with org-capture
;; (use-package rainbow-mode
;;   :defer t
;;   :hook (org-mode
;;          emacs-lisp-mode
;;          web-mode
;;          typescript-mode
;;          js2-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Sync/workflows/RoamNotes/"))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain-TeX-mode
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plainf
      (file "~/Sync/workflows/RoamNotes/Templates/BookNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"   . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config
  (require 'mu4e-org)
  (setq mu4e-change-filename-when-moving t)
  ;; Refresh mail using isync every 10 minutes.
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-maildir "~/Mail")
  
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-refile-folder "/[Gmail].All Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")

  (setq org-capture-templates
        `(("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp "~/Sync/workflows/Mail.org" "Follow Up")
           "* TODO %a")
          ("mr" "Read Later" entry (file+olp "~/Sync/workflows/Mail.org" "Read Later")
           "* TODO %a")))
  
  (setq mu4e-maildir-shortcuts
        '(("/Inbox"                 . ?i)
          ("/[Gmail].Sent Mail"     . ?s)
          ("/[Gmail].Trash"         . ?t)
          ("/[Gmail].Drafts"        . ?d)    
          ("/[Gmail].All Mail"      . ?a))))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; Set font size
(set-face-attribute 'default nil :font "Fira Mono" :height 200)

(require 'icomplete)
(require 'init-paredit)
(require 'init-clojure-cider)
(require 'init-recentf)
(require 'init-ido)
(require 'init-powerline)
(require 'init-evil)
(require 'init-themes)
(require 'init-haskell)
(require 'init-idris)
(require 'init-elixir)
(require 'init-markdown)

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

;;; hydra-zoom config
(defhydra hydra-zoom (global-map "<f2>")
          "zoom"
          ("g" text-scale-increase "in")
          ("l" text-scale-decrease "out"))

;;; unit tests
;;; (setq cider-test-show-report-on-success t) ; whatever it fails or success, show report anyway.
;(cider-auto-test-mode 1)

;;; w3m setup
;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;quick access hacker news
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-directory (quote ("~/Sync/workflows")))

;;; add org directory to agenda.
(setq org-agenda-files (quote ("~/Sync/workflows/Agenda/")))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
;;(set-default-font "Source Code Pro 16")
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
 '(cider-boot-parameters "repl -s wait")
 '(minibuffer-electric-default-mode t)
 '(org-agenda-files (quote ("~/Documents/org/work.org")))
 '(package-selected-packages
   (quote
    (## cider clj-refactor emacs-w3m w3m elixir-mode idris-mode p avy zenburn-theme xclip use-package tiny-menu smex scpaste rainbow-delimiters projectile prodigy powerline-evil planet-theme parenface obsidian-theme nlinum-relative monokai-theme midje-mode markdown-mode magit kibit-helper key-chord jazz-theme ido-vertical-mode ido-ubiquitous idle-highlight-mode idea-darkula-theme highlight-symbol helm-themes helm-ag haskell-mode find-file-in-project exec-path-from-shell evil-surround evil-leader evil-indent-textobject color-theme-sanityinc-solarized color-theme-monokai clojure-mode-extra-font-locking better-defaults ace-jump-mode ac-alchemist abc-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/run) (user/browser-repl))")
     (cider-refresh-after-fn . "reloaded.repl/go")
     (cider-refresh-before-fn . "reloaded.repl/stop")))))

;;; global set
(global-set-key (kbd "C-M-=") 'toggle-frame-fullscreen)
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

;;; eshell
(defun efs/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert vidsual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  ; :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))
