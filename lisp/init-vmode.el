;;; vlang
(require 'v-mode)
(define-key v-mode-map (kbd "M-z") 'v-menu)
(define-key v-mode-map (kbd "C-c C-f") 'v-format-buffer)

(provide 'init-vmode)
