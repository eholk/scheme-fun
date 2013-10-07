(load "rn-match.r5rs")

(define (lookup x env)
  (if (eq? x (caar env))
      (cdar env)
      (lookup x (cdr env))))

(define (update-env! x v env)
  (if (eq? x (caar env))
      (set-cdr! (car env) v)
      (update-env! x v (cdr env))))

(define (value-of-cps e env k)
  (match e
    (,x (guard (symbol? x)) (k (lookup x env)))
    (,n (guard (number? n)) (k n))
    ((lambda (,x) ,e)
     (k (lambda (v k) (value-of-cps e (cons (cons x v) env) k))))
    ((set! ,x ,[e])
     (k (update-env! x e env)))
    ((begin ,e* ... ,e)
     (let loop ((e* e*))
         (if (pair? e*)
             (value-of-cps (car e*) env
                           (lambda (_)
                             (loop (cdr e*))))
             (value-of-cps e env k))))
    ((,op ,e1 ,e2) (guard (memq op '(+ * -)))
     (value-of-cps e1 env
                   (lambda (v1)
                     (value-of-cps e2 env
                                   (lambda (v2)
                                     (k ((eval op) v1 v2)))))))
    ((,e1 ,e2)
     (value-of-cps e1 env
                   (lambda (f)
                     (value-of-cps e2 env
                                   (lambda (v)
                                     (f v k))))))))
