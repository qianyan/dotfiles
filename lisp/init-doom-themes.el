;;; modus-themes
(use-package modus-themes
  :ensure t
  :bind
  (("<f5>" . modus-themes-toggle)
   ("C-<f5>" . modus-themes-select))
  :config
  (modus-themes-load-theme 'modus-vivendi-tinted))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ;if nil, italics is universally disabled
  :config
  (load-theme 'doom-ayu-light t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Function to select and load a Doom theme interactively
(defun my/select-doom-theme ()
  "Select a Doom theme from a list of installed themes."
  (interactive)
  (let ((theme (completing-read "Select Doom theme: "
                                (mapcar 'symbol-name (custom-available-themes)))))
    (load-theme (intern theme) t)))

;; Key binding to trigger the theme selector
(global-set-key (kbd "C-c t") 'my/select-doom-theme)

(provide 'init-doom-themes)
