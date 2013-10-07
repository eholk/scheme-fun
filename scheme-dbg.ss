;; A Scheme interpreter with debugging features.

(load "rn-match.r5rs")

(define (lookup x env)
  (if (eq? x (caar env))
      (cdar env)
      (lookup x (cdr env))))

(define (update-env! x v env)
  (if (eq? x (caar env))
      (set-cdr! (car env) v)
      (update-env! x v (cdr env))))

(define (value-of e env)
  (match e
    (,x (guard (symbol? x)) (lookup x env))
    (,n (guard (number? n)) n)
    ((lambda (,x) ,e)
     (lambda (v) (value-of e (cons (cons x v) env))))
    ((set! ,x ,[e])
     (update-env! x e env))
    ((begin ,e* ... ,e)
     (begin
       (let loop ((e* e*))
         (if (pair? e*)
             (begin
               (value-of (car e*) env)
               (loop (cdr e*)))))
       (value-of e env)))
    ((debug) (debugger env))
    ((,op ,[e1] ,[e2]) (guard (memq op '(+ * -)))
     ((eval op) e1 e2))
    ((,[e1] ,[e2])
     (e1 e2))))

(define (debugger env)
  (printf "debug> ")
  (let ((cmd (read))
        (continue #t))
    (match cmd
      ((continue) (set! continue #f))
      ((eval ,e)
       (printf "~s => ~s\n" e (value-of e env)))
      ((show-env) (show-env env))
      (,else (printf "unknown command\n")))
    (if continue
        (debugger env)
        (printf "running...\n"))))

(define (show-env env)
  (if (pair? env)
      (let ((depth (show-env (cdr env))))
        (printf "~d. ~a: ~s\n" depth (caar env) (cdar env))
        (+ 1 depth))
      0))

(((lambda (x)
    (lambda (y)
      (begin
        (debug)
        (+ x y))))
  4)
 5)
