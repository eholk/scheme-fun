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
     (let ((bindings... (extract-... (car p) bindings)))
       (append
        (apply map (cons (lambda b*
                           (instantiate* (car p)
                                         (append (apply append b*) bindings)))
                         bindings...))
        (instantiate* (cddr p) bindings))))
    ((pair? p)
     (cons (instantiate* (car p) bindings)
           (instantiate* (cdr p) bindings)))
    ;; We want to make sure that the ... depth counter is 0.
    ((assq p bindings) => cdr)
    (else p)))

(define (extract-... p bindings)
  (if (null? bindings)
      '()
      (let ((rest (extract-... p (cdr bindings)))
            (b (car bindings)))
        (if (eq? (car b) '...)
            (let ((names (map car (cadr b))))
              (if (ormap (lambda (x) (mem* x p)) names)
                  (cons (cdr b) rest)
                  rest))
            rest))))

(define (mem* x ls)
  (if (pair? ls)
      (or (mem* x (car ls)) (mem* x (cdr ls)))
      (eq? x ls)))

;; Here's an example...
'(let ((a 5) (b 6) (c 7)) (+ a b c))

;; The pattern is...
'(_ ((x e) ...) b)

;; We'll represent the environment like this:
'((... ((x . a) (e . 5)) ((x . b) (e . 6)) ((x . c) (e . 7))) (b . (+ a b c)))

(instantiate* '(let ((x e) ...) b) '((... ((x . a) (e . 5)) ((x . b) (e . 6)) ((x . c) (e . 7))) (b . (+ a b c))))
