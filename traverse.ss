(define (check-property p tree)
  (if (pair? tree)
      (if (check-property p (car tree))
          (check-property p (cdr tree))
          #f)
      (p tree)))

(define (check-property-cps p tree sk fk)
  (if (pair? tree)
      (check-property p
                      (car tree)
                      (lambda () (check-property p (cdr tree)))
                      fk)
      (if (p tree)
          (sk)
          (fk))))

