(require 'hide-mode-line)
(use-package org-present
  :requires (visual-fill-column hide-mode-line)
  :ensure t)

(defun qy/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun qy/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;;  Hide cursor
  (org-present-hide-cursor)
  (evil-mode 0)

  ;; Read only
  (org-present-read-only)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)

  (hide-mode-line-mode +1))

(defun qy/org-present-end ()
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Display cursor
  (org-present-show-cursor)
  (evil-mode 1)

  ;; Read and write
  (org-present-read-write)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)

  (hide-mode-line-mode 0))

; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'qy/org-present-start)
(add-hook 'org-present-mode-quit-hook 'qy/org-present-end)
(add-hook 'org-present-after-navigate-functions 'qy/org-present-prepare-slide)

;; Set reusable font name variables
(defvar qy/fixed-width-font "JetBrains Mono"
  "The font to use for monospaced (fixed width) text.")

(defvar qy/variable-width-font "Iosevka Aile"
  "The font to use for variable-pitch (document) text.")
;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font qy/variable-width-font :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font qy/variable-width-font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
;; Let the desktop background show through
(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;;; Presentation purpose
;;; Another option to make a buffer like a slide
(use-package logos
  :ensure t
  :custom
  (logos-outlines-are-pages t)
  (logos-hide-mode-line t))

(provide 'init-presentation)
