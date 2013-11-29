(trace-define-syntax value-of-k
  (syntax-rules (lambda)
    ;; lambda
    ((_ (lambda (x) b) k)
     (apply-k k (syntax-rules ()
                  ((_ v k^)
                   (let-syntax ((x (syntax-rules ()
                                     ((_ k^^)
                                      (apply-k k^^ v)))))
                     (value-of-k b k^))))))
    ;; application
    ((_ (e1 e2) k)
     (value-of-k e1 (syntax-rules ()
                      ((_ v1)
                       (value-of-k e2 (syntax-rules ()
                                        ((_ v2)
                                         (apply-k v1 v2 k))))))))
    ;; variable
    ((_ x k)
     (x k))))

(trace-define-syntax apply-k
  (syntax-rules ()
    ((_ k v ...)
     (let-syntax ((t k))
       (t v ...)))))

(trace-define-syntax value-of
  (syntax-rules ()
    ((_ e) (value-of-k e (syntax-rules ()
                           ((_ v) 'v))))))

'((lambda (z) (z z)) (lambda (y) (lambda (x) (y x))))
;;=>
'(lambda (x) ((lambda (y) (lambda (x) (y x))) x))
