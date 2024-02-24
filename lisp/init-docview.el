(use-package doc-view
  :custom
  ; `brew install mupdf` includes mutool, mupdf-gl, muraster
  ; https://mupdf.readthedocs.io/en/latest/quick-start-guide.html
  (doc-view-dvipdfm-program "/opt/homebrew/bin/mutool"))

(provide 'init-docview)
