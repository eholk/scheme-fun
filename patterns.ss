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

;; This version handles ... patterns. The environment can now contain
;; (...) entries, which contain bindings that were matched under an
;; ellipsis pattern.
(define (instantiate* p bindings)
  (cond
    ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
     (let ((bindings... (extract-... (car p) bindings)))
       (if (null? bindings...)
           (instantiate* (cddr p) bindings)
           (append
            (apply map (cons (lambda b*
                               (instantiate* (car p)
                                             (append (apply append b*)
                                                     bindings)))
                             bindings...))
            (instantiate* (cddr p) bindings)))))
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
        (if (and (eq? (car b) '...) (not (null? (cdr b))))
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

(instantiate* '(let ((x e) ...) b) '((...) (b . (+ a b c))))


;; Now we make a new version of match* that passes a list of bindings
;; to the success continuation.
(define (match* kw* p e sk fk)
  (cond
    ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
     (let loop ((e e)
                (b '()))
       (if (null? e)
           (match* kw* (cddr p) e
                   (lambda (b^) (sk `((... . ,b) . ,b^)))
                   fk)
           (match* kw* (car p) (car e)
                   (lambda (b^)
                     (loop (cdr e) (cons b^ b)))
                   (lambda ()
                     (match* kw* (cddr p) e
                             (lambda (b^) (sk `((... . ,b) . ,b^)))
                             fk))))))
    ((and (pair? p) (pair? e))
     (match* kw* (car p) (car e)
             (lambda (b)
               (match* kw* (cdr p) (cdr e)
                        (lambda (b^) (sk (append b b^)))
                        fk))
             fk))
    ((and (memq p kw*) (eq? p e))
     (sk '()))
    ((and (symbol? p) (not (memq p kw*)))
     (if (memq e kw*)
         (error 'match* "misplaced aux keyword" e)
         (sk (list (cons p e)))))
    ((and (null? p) (null? e))
     (sk '()))
    (else (fk))))

;; some tests
(match* '() '(_ ((x e) ...) b) '(let () (+ a b c))
        (lambda (b) b) (lambda () #f))

(match* '() '(_ () b) '(let () (+ a b c))
        (lambda (b) b) (lambda () #f))

(match* '() '(_ (xe ...) b) '(let () (+ a b c))
        (lambda (b) b) (lambda () #f))

(match* '() '(_ ((x e) ...) b) '(let () (+ a b c))
        (lambda (b) (list (instantiate* '(_ ((x e) ...) b) b) b)) (lambda () #f))

(match* '() '(_ ((x e) ...) b) '(let ((a 1) (b 2) (c 3)) (+ a b c))
        (lambda (b) (list (instantiate* '(_ ((x e) ...) b) b) b))
        (lambda () #f))
