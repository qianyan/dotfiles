;;; iuscheme.el --- Scheme support, Indiana Univeristy style
;;;
;;; Time-stamp: <2001-10-03 11:23:55 dyb>
;;;  (update this with M-x time-stamp)

;;; original version by Chris Haynes <chaynes@indiana.edu>
;;; current version by Erik Hilsdale <ehilsdal@indiana.edu>
;;; minor modifications by Kent Dybvig <dyb@cs.indiana.edu>

;;; Tested to work under Gnu Emacs versions 19.34, 20.2.1, and 20.2.3.
;;; It also works with some versions of XEmacs, but I don't remember
;;; which ones -eh

;;; 1999-07-08:  Added 'minimal indentation style, from Chris Haynes


(require 'cmuscheme)

;; -------- Key redefinitions ----

;; CMU likes RET to go to the beginning of the line, and LFD to to a
;; return and an indent. IU tends to like it the other way around.

(define-key scheme-mode-map "\n" 'newline)
(define-key scheme-mode-map "\r" 'newline-and-indent)

;; We also want something like this when running a Scheme process.

(define-key inferior-scheme-mode-map "\n" 'newline)
(define-key inferior-scheme-mode-map "\r" 'scheme-return)

;; -------- Making return work --------

;; The problem with comint mode is that it's extremely line-based.
;; Scheme is not so line based.  This fixes the problem: a Scheme
;; expression is only sent to the Scheme process when it is full and
;; balanced.

(defun scheme-return ()
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents."
  (interactive)
  (let ((input-start (process-mark (get-buffer-process (current-buffer)))))
    (if (< (point) input-start)
        (comint-send-input)             ; this does magic stuff
      (let ((state (save-excursion
                     (parse-partial-sexp input-start (point)))))
        (if (and (< (car state) 1)      ; depth in parens is zero
                 (not (nth 3 state))    ; not in a string
                 (not (save-excursion   ; nothing after the point
                        (search-forward-regexp "[^ \t\n\r]" nil t))))
            (comint-send-input)         ; then go for it.
          (newline-and-indent))))))

;;; -------- Indenting definitions ----

;; If TAB indents lines, it might make sense for C-c TAB to indent
;; definitions.  Or at least that's the story around here.

(defun scheme-indent-definition ()
  "Fix indentation of the current definition."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

(define-key scheme-mode-map "\C-c\C-i" 'scheme-indent-definition)
(define-key inferior-scheme-mode-map "\C-c\C-i" 'scheme-indent-definition)

;;; ---------- Indentation Style ----

(if (< (string-to-number (substring emacs-version 0 2)) 20)
    (defmacro defcustom (name val docstring &rest ignore)
      (list 'defvar name val docstring)))

(defcustom scheme-indentation-style 'iu
  "*This variable governs the style of indentation of Scheme code.
If its value is the symbol 'cps, 'iu, or 'minimal,
applications (where the operator is a symbol) indent as

    (operator arg ...
      arg
      ...)

which is more convenient for code in continuation-passing-style.
Otherwise, such applications indent as

    (operator arg ...
              arg
              ...)

which is a more traditional style of Lisp indentation.

In either case, if the operator is a non-symbol, applications expand as

    ((...) arg ...
     arg
     ...)

And if a symbol's 'scheme-indent-function property is set to an integer n,
then n subforms of that symbol's expression will line up.  So, since the
symbol 'if has as its 'scheme-indent-function property the integer 3,
if-expressions will indent as

    (if first-arg
        second-arg
        third-arg)

If scheme-indentation-style is 'minimal, or the symbol begins with `def',
the 'scheme-indent-function property is ignored, so the form is treated
as applications.
"
  :group 'scheme)

;;; ---------- Indentation keywords ----

;;; With this change, we need to add a few new indentation keywords

(put 'and 'scheme-indent-function 0)
(put 'or 'scheme-indent-function 0)
(put 'cond 'scheme-indent-function 0)
(put 'set! 'scheme-indent-function 1)
(put 'if 'scheme-indent-function 3)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'with-syntax 'scheme-indent-function 1)
(put 'datum->syntax-object 'scheme-indent-function 1)
(put 'syntax-object->datum 'scheme-indent-function 1)

;;; ---------- Tabs ----
;;; tabs are evil; they tend to screw up printed files.  this should
;;; prevent emacs from inserting tabs when indenting.
(setq-default indent-tabs-mode nil)

;;; ------------------------------
;;; Don't read below here

;;; The following code shouldn't be here.  It's copied directly from
;;; scheme.el which is taken from lisp-mode.el.  I'm extremely
;;; irritated that advice.el doesn't help with this, but it doesn't.
;;; The only changes from scheme.el is marked with -ehilsdal

(defun scheme-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (scheme-indent-as-defun-p method function)) ;-ehilsdal
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(defun scheme-indent-as-defun-p (method function)
  "Given a method and function name, determine whether the function
indents as a definition."
  (or   
   (and (null method)
        (or (equal scheme-indentation-style 'iu)
            (equal scheme-indentation-style 'cps)
            (and (> (length function) 3)
                 (string-match "\\`def" function))))
   (equal scheme-indentation-style 'minimal)))

;;; ------------------------------
;;; Compatability

;;; The above version works perfectly under Gnu Emacs 20.2.  However,
;;; emacs (and xemacs) 19.34 needs some help.  This still isn't
;;; perfect, but it'll do.

(cond
 ((>= (string-to-number (substring emacs-version 0 2)) 20)
  ;; in Gnu Emacs 20.2.1, there was a bug in the handling of let expressions.
  ;; the following is the definition of scheme-let-indent from 20.2.3.
  (defun scheme-let-indent (state indent-point normal-indent)
    (skip-chars-forward " \t")
    (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
        (lisp-indent-specform 2 state indent-point normal-indent)
      (lisp-indent-specform 1 state indent-point normal-indent)))
  )
 (t
  ;;; Gnu Emacs 19 uses different technology for indentation.
  (add-hook 'scheme-mode-hook
            (function
             (lambda ()
               (make-local-variable 'indent-sexp)
               (fset 'indent-sexp 'scheme-indent-sexp))))
 (defun scheme-indent-function (indent-point state)
   (let ((normal-indent (current-column)))
     (save-excursion
       (goto-char (1+ (car (cdr state))))
       (re-search-forward "\\sw\\|\\s_")
       (if (/= (point) (car (cdr state)))
           (let ((function (buffer-substring (progn (forward-char -1) (point))
                                             (progn (forward-sexp 1) (point))))
                 method)
             ;; Who cares about this, really?
             ;;(if (not (string-match "\\\\\\||" function)))
             (setq function (downcase function))
             (setq method (get (intern-soft function) 'scheme-indent-function))
             (cond ((or (eq method 'defun)
                        (scheme-indent-as-defun-p method function)) ; -ehilsdal
                    (scheme-indent-defform state indent-point))
                   ((integerp method)
                    (scheme-indent-specform method state indent-point))
                   (method
                    (funcall method state indent-point))))))))))

;;; ---- end iuscheme.el

(provide 'iuscheme)
