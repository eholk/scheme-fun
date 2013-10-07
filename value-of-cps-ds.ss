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
    (,x (guard (symbol? x)) (apply-k k (lookup x env)))
    (,n (guard (number? n)) (apply-k k n))
    ((lambda (,x) ,e)
     (apply-k k (lambda (v k) (value-of-cps e (cons (cons x v) env) k))))
    ((set! ,x ,[e])
     (apply-k k (update-env! x e env)))
    ((begin ,e* ... ,e)
     (let loop ((e* e*))
         (if (pair? e*)
             (value-of-cps (car e*) env
                           `(loop-k ,loop ,e*))
             (value-of-cps e env k))))
    ((,op ,e1 ,e2) (guard (memq op '(+ * -)))
     (value-of-cps e1 env
                   `(v1-k ,e2 ,env ,op ,k)))
    ((,e1 ,e2)
     (value-of-cps e1 env
                   `(rator-k ,e2 ,env ,k)))))

(define (apply-k k v)
  (match k
    ((empty-k) v)
    ((loop-k ,loop ,e*)
     (loop (cdr e*)))
    ((v1-k ,e2 ,env ,op ,k)
     (value-of-cps e2 env `(v2-k ,op ,v ,k)))
    ((v2-k ,op ,v1 ,k)
     (apply-k k ((eval op) v1 v)))
    ((rator-k ,e2 ,env ,k)
     (value-of-cps e2 env `(rand-k ,v ,k)))
    ((rand-k ,f ,k)
     (f v k))))
