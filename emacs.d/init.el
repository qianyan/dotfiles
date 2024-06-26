(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpa-devel" . "https://elpa.gnu.org/devel/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-enable-at-startup nil)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun qy/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

(add-hook 'emacs-startup-hook #'qy/display-startup-time)

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

; init screen
(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package better-defaults
  :ensure t
  :config
  (ido-mode -1)
  (mouse-wheel-mode 0))                        ; disable ido-mode becuase of the vertico we enabled have conflict with it.

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

(use-package projectile
             :ensure t
             :bind-keymap ("C-c p" . projectile-command-map)
             :init (add-hook 'after-init-hook 'projectile-global-mode))

(use-package rainbow-delimiters
             :ensure t)

(use-package clj-refactor
             :ensure t
             :pin melpa-stable)

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

(use-package avy
             :ensure t
             :bind
             ("C-," . avy-goto-char)
             ("C-'" . avy-goto-char-2)
             ("M-g f" . avy-goto-line)
             ("M-g w" . avy-goto-word-1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Save what you enter into minibuffer
(savehist-mode 1)

;;; Remember and restore the last location of opened file
(save-place-mode 1)

;;; Revert buffers when the underlying file has changed
;;; M-x revert-buffer
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; define constant variables
(defconst *is-a-mac* (eq system-type 'darwin))

(require 'icomplete)
(require 'init-macos-keys)
(require 'init-paredit)
(require 'init-clojure-cider)
(require 'init-term)
(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-modeline)
(require 'init-hydra)

(require 'init-gui-frame)
(require 'init-icons)
(require 'init-fonts)
(require 'init-dashboard)
(require 'init-presentation)

(require 'init-evil)
(require 'init-doom-themes)
(require 'init-orgmode)
(require 'init-docview)
(require 'init-ai-assistant)
(require 'init-input-methods)

(use-package pyvenv
  :ensure t)

;;; timer
(use-package tmr
  :ensure t)

;;; edit config file
(global-set-key (kbd "<f6>") (lambda()
                               (interactive)
                               (find-file "~/.emacs.d/init.el")))

(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


(org-babel-load-file "~/Sync/org/writing_gnu_emacs_extensions.org" 'compile)

(defun qy/dired-jump-siderbar ()
  (interactive)
  (progn
    (display-buffer-in-side-window
     (dired-noselect default-directory)
     '((side . left))
     )
    (other-window 1)))

;;; most programming mode are inherited from prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; load ChatGPT keys for python environments
(defun qy/setenv-from-dotenv (file-path)
  "Read a .env file and set the environment variables."
  (interactive "fPath to .env file: ")
  (when (file-readable-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
        (setenv (match-string 1) (match-string 2))))))

(qy/setenv-from-dotenv "~/.emacs.d/.env")
