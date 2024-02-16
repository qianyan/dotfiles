(use-package hydra
             :ensure t)
			
;;; hydra-zoom config
(defhydra hydra-zoom (global-map "<f2>")
          "zoom"
          ("g" text-scale-increase "in")
          ("l" text-scale-decrease "out"))

(defhydra hydra-launcher (:color blue)
   "Launch"
   ("h" man "man")
   ("r" (browse-url "http://www.reddit.com/r/emacs/") "reddit")
   ("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
   ("s" shell "shell")
   ("q" nil "cancel"))

(global-set-key (kbd "C-c r") 'hydra-launcher/body)

(defun my-ibuffer-hook ()
  (defhydra hydra-ibuffer (:color pink
                           :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_:save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("U" ibuffer-unmark-all-marks)
    ("d" ibuffer-mark-for-delete)
    ("D" ibuffer-mark-for-delete-backwards)
    ("s" nil "nil")
    ("~" ibuffer-do-toggle-modified)
    ("x" ibuffer-do-shell-command-pipe)
    ("b" ibuffer-bury-buffer)
    ("g" ibuffer-do-revert)
    ("T" ibuffer-do-toggle-read-only)
    ("O" ibuffer-do-occur :color blue)
    ("I" ibuffer-do-query-replace-regexp :color blue)
    ("R" ibuffer-do-rename-uniquely :color blue)
    ("c" nil "cancel")
    ("v" ibuffer-find-file "select" :color blue)
    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" nil "nil"))

  (define-key ibuffer-mode-map (kbd ".") 'hydra-ibuffer/body))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-hook)

(provide 'init-hydra) 
