(defvar my-packages '(zenburn-theme
                      monokai-theme
                      idea-darkula-theme
                      jazz-theme
                      planet-theme
                      obsidian-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (use-package p :ensure t :defer t)))

(provide 'init-themes)
