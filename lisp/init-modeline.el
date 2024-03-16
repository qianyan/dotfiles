;;; init-modeline
(use-package spacious-padding
  :ensure t
  :custom
 (spacious-padding-subtle-mode-line
   `(:mode-line-active 'default :mode-line-inactive vertical-border))

  (spacious-padding-widths
   '( :internal-border-width 10
      :header-line-width 4
      :mode-line-width 10
      :tab-width 
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8)))

;;; Word count shown the modeline for writing styles
;;; Example: WC[65+2/68]
(use-package wc-mode
  :ensure t)


(provide 'init-modeline)
