;; A syntax-rules style pattern matcher.

;; We start by making a simple pattern matcher that simply calls the
;; success continuation if the pattern matches, and the failure
;; continuation otherwise.
(define (match? kw* p e sk fk)
  (cond
    ((and (pair? p) (pair? e))
     (match? kw* (car p) (car e)
             (lambda ()
               (match? kw* (cdr p) (cdr e)
                       (lambda () (sk))
                       fk))
             fk))
    ((and (memq p kw*) (eq? p e))
     (sk))
    ((and (symbol? p) (not (memq p kw*)))
     (if (memq e kw*)
         (error 'match? "misplaced aux keyword" e)
         (sk)))
    ((and (null? p) (null? e))
     (sk))
    (else (fk))))

;; Now we extend it to handle ... patterns
(define (match*? kw* p e sk fk)
  (cond
    ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)) (pair? e))
     (match*? kw* (car p) (car e)
              (lambda ()
                (match*? kw* p (cdr e) sk
                         (lambda () (match*? kw* (cdr p) e sk fk))))
              (lambda () (match*? kw* (cdr p) e sk fk))))
    ((and (pair? p) (pair? e))
     (match*? kw* (car p) (car e)
             (lambda ()
               (match*? kw* (cdr p) (cdr e)
                       (lambda () (sk))
                       fk))
             fk))
    ((and (memq p kw*) (eq? p e))
     (sk))
    ((and (symbol? p) (not (memq p kw*)))
     (if (memq e kw*)
         (error 'match*? "misplaced aux keyword" e)
         (sk)))
    ((and (null? p) (null? e))
     (sk))
    (else (fk))))
