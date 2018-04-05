(defvar my-themes '(zenburn-theme
                   monokai-theme
                   idea-darkula-theme
                   jazz-theme
                   planet-theme 
                   obsidian-theme))

(dolist (theme my-themes)
  (unless (package-installed-p theme)
    (eval `(use-package ,theme :ensure t :defer t))))

(provide 'init-themes)
