;;; count-leaves
(define tree (cons '(1 2) '(3 4)))


(define (length tr)
  (if (null? tr)
      0
      (+ 1 (length (cdr tr)))))

(define (count-leaves tr)
  (cond ((null? tr) 0)
        ((not (pair? tr)) 1)
        (else
         (+ (count-leaves (car tr))
            (count-leaves (cdr tr))))))

(display (length tree))

(newline)

(display (count-leaves tree))


;;; 2.25
(newline)
(define l1 '(1 3 (5 7) 9))
(display (car (cdr (car (cdr (cdr l1))))))

(newline)
(define l2 '((7)))
(display (car (car l2)))

(newline)
(define l3 '(1 (2 (3 (4 (5 (6 (7))))))))

(display (car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))))


(newline)
(define tr1 '((1 2) (3 4)))
;;; reverse ((1 2) (3 4)) => ((3 4) (1 2))
(define (reverse tr)
  (if (null? tr)
      '()
      (append (reverse (cdr tr))
              (list (car tr)))))

;;; deep reverse ((1 2) (3 4)) => ((4 3) (2 1))
(define (deep-reverse tr)
  (cond ((null? tr)
         '())
        ((not (pair? tr))
         tr)
        (else
         (append (deep-reverse (cdr tr)) (list (deep-reverse (car tr)))))))

;;; fringe ((1 2) (3 4)) => (1 2 3 4)
(define (fringe tr)
  (cond ((null? tr)
         '())
        ((not (pair? tr))
         (list tr))
        (else
         (append (fringe (car tr)) (fringe (cdr tr))))))
