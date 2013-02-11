(define-syntax or2
  (syntax-rules ()
    ((_ e1 e2)
     (let ((t e1))
       (if t t e2)))))

(define-syntax my-let
  (syntax-rules ()
    ((_ ((x e) ...) b)
     ((lambda (x ...) b) e ...))))

(define-syntax my-or
  (syntax-rules ()
    ((_ e) e)
    ((_ e e* ...)
     (let ((t e))
       (if t t (my-or e* ...))))))
