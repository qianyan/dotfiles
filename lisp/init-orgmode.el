(defun qy/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode 0)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . qy/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        ;; hide bold, italic markers
        org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  :custom
  (org-agenda-files
   '("~/Sync/workflows/Agenda/Tasks.org"
     "~/Sync/workflows/Agenda/Family.org"))
  (org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-directory "~/Sync/org")
(defun +org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-index-file (+org-file-path "index.org"))

(defun +open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") '+open-index-file)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ox-latex
  :ensure-system-package latexmk
  :ensure nil
  :after org
  :commands (org-export-dispatch)

  :custom
  (org-latex-pdf-process '("latexmk -xelatex -shell-escape -quiet -f %f"))

  (org-latex-src-block-backend 'listings)
  (org-latex-listings-options
   '(("basicstyle" "\\ttfamily")
     ("showstringspaces" "false")
     ("keywordstyle" "\\color{blue}\\textbf")
     ("commentstyle" "\\color{gray}")
     ("stringstyle" "\\color{green!70!black}")
     ("stringstyle" "\\color{red}")
     ("frame" "single")
     ("numbers" "left")
     ("numberstyle" "\\ttfamily")
     ("columns" "fullflexible")))

  (org-latex-packages-alist '(("" "listings")
                              ("" "booktabs")
                              ("UTF8" "ctex" t)
                              ("AUTO" "polyglossia" t ("xelatex" "lualatex"))
                              ("" "grffile")
                              ("" "unicode-math")
                              ("" "xcolor")))

  :config
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(use-package ox-beamer
  :ensure nil
  :after ox-latex)

(defun qy/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . qy/org-mode-visual-fill))

(provide 'init-orgmode)
