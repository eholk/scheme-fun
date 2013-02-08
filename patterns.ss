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

;; The hard part of matching ... and tracking bindings seems to be
;; more on the substitution side. We'll pause for a moment to do
;; substitution to figure out a decent data structure.
(define (instantiate p bindings)
  (cond
    ((pair? p)
     (cons (instantiate (car p) bindings)
           (instantiate (cdr p) bindings)))
    ((assq p bindings) => cdr)
    (else p)))

;; This version handles ... patterns. The environment is now a list of
;; (name depth . val*) things.
(define (instantiate* p bindings)
  (cond
    ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
     ;; Find the bindings used by the car of this pattern.
     (let ((bindings (filter-bindings (car p) bindings)))
       ;; TODO
       bindings))
    ((pair? p)
     (cons (instantiate* (car p) bindings)
           (instantiate* (cdr p) bindings)))
    ;; We want to make sure that the ... depth counter is 0.
    ((assq p bindings) => cddr)
    (else p)))

(define (filter-bindings p bindings)
  (if (null? bindings)
      '()
      (if (mem* (caar bindings) p)
          (cons (car bindings) (filter-bindings p (cdr bindings)))
          (filter-bindings p (cdr bindings)))))

(define (mem* x ls)
  (if (pair? ls)
      (or (mem* x (car ls)) (mem* x (cdr ls)))
      (eq? x ls)))
