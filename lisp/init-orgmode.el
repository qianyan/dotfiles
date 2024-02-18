
;; (defun dw/center-buffer-with-margins ()
;;   (let ((margin-size (/ (- (frame-width) 80) 3)))
;;     (set-window-margins nil margin-size margin-size)))
;;; org-mode
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
(setq org-log-done t)
(setq org-directory (quote ("~/Sync/workflows")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun qy/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . qy/org-mode-visual-fill))

(provide 'init-orgmode)
