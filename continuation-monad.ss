(load "rn-match.r5rs")

;; Another version of monad derivation, this time using factorial to
;; derive the continuation monad.

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define (fact-cps n k)
  (if (zero? n)
      (k 1)
      (fact-cps (- n 1) (lambda (v) (k (* v n))))))

;; okay, now curry the k
(define (fact-cps/curry n)
  (lambda (k)
    (if (zero? n)
        (k 1)
        ((fact-cps/curry (- n 1)) (lambda (v) (k (* v n)))))))

;; push the lambda
(define (fact-cps/pushed n)
  (if (zero? n)
      (lambda (k)
        (k 1))
      (lambda (k)
        ((fact-cps/pushed (- n 1)) (lambda (v) (k (* v n)))))))

;; define return
(define (return v)
  (lambda (k) (k v)))

(define (fact-cps/return n)
  (if (zero? n)
      (return 1)
      (lambda (k)
        ((fact-cps/return (- n 1)) (lambda (v) (k (* v n)))))))

;; define bind
(define (bind m f)
  (lambda (k)
    (m (lambda (v) ((f v) k)))))

(define (fact-cps/bind n)
  (if (zero? n)
      (return 1)
      (bind (fact-cps/bind (- n 1))
            (lambda (v) (* v n)))))

;; and do syntax. We should be able to use the same macro if we did it
;; right.
(define-syntax do*
  (syntax-rules ()
    ((_ ((x e) rest ...) body)
     (bind e (lambda (x) (do* (rest ...) body))))
    ((_ () body)
     body)))

(define (fact-cps/monadic n)
  (if (zero? n)
      (return 1)
      (do* ((v (fact-cps/monadic (- n 1))))
           (return (* v n)))))
