(load "rn-match.r5rs")

;; A look at deriving the store monad.

;; Normally introductions to monads start with the monad laws and some
;; category theory. This looks at them more as a programming style,
;; like CPS.


;; We start with our normal interpreter, and some helpers

(define empty-env
  (lambda (x)
    (error 'value-of "variable not bound" x)))

(define extend-env
  (lambda (x v env)
    (lambda (y)
      (if (eq? x y)
          v
          (env y)))))

(define lookup
  (lambda (x env)
    (env x)))

(define value-of
  (lambda (e env)
    (match e
      (,x (guard (symbol? x))
       (lookup x env))
      (,n (guard (number? n))
       n)
      ((λ (,x) ,e)
       (lambda (v)
         (value-of e (extend-env x v env))))
      ((,rator ,rand)
       (let ((rator (value-of rator env))
             (rand  (value-of rand env)))
         (rator rand))))))

;; Next, we want to add mutable references.
;;
;; (ref e) creates a reference containing e
;; (deref e) gives the value of the thing e points to
;; (update! e1 e2
;;
;; The first thing we need to do is thread a store through everything
;;
;; Let's try this:
(define value-of/store
  (lambda (e env store)
    (match e
      (,x (guard (symbol? x))
       (lookup x env))
      (,n (guard (number? n))
       n)
      ((λ (,x) ,e)
       (lambda (v)
         (value-of/store e (extend-env x v env) store)))
      ((,rator ,rand)
       (let ((rator (value-of/store rator env store))
             (rand  (value-of/store rand env store)))
         (rator rand))))))

;; So far so good. (Actually, there are numerous issues, which will
;; become apparently soon.)
;;
;; Let's start with deref, since that will show us how the store
;; works. We'll assume the store is just an associate list, mapping
;; labels to values. Since we're purely functional, we update by
;; prepending the new value with the same label to the list. This
;; means at the end we'll have a list of all the changes made to the
;; store.
;;
;; We'll start wrapping the evaluator in a letrec so internally we
;; don't have to keep changing the name.
(define value-of/store+deref
  (letrec ((value-of/store
            (lambda (e env store)
              (match e
                (,x (guard (symbol? x))
                    (lookup x env))
                (,n (guard (number? n))
                    n)
                ((λ (,x) ,e)
                 (lambda (v)
                   (value-of/store e (extend-env x v env) store)))
                ((deref ,e)
                 (match (value-of/store e env store)
                   ((ref ,label)
                    (cdr (assq label store)))))
                ((,rator ,rand)
                 (let ((rator (value-of/store rator env store))
                       (rand  (value-of/store rand env store)))
                   (rator rand)))))))
    value-of/store))

;; Okay, cool.
;;
;; Now we look at ref. The basic idea is to cons a new pair onto the
;; store. But wait, where do we put the updated store? We'll have to
;; return two values. We could use Scheme's multiple return values,
;; but we'll just return a cons cell instead.
;;
;; Here's the interpreter with the store properly threaded. Notice
;; that we have to sequence the evaluation of rator and
;; rand. We do this by nesting match clauses.
;;
;; Notice also that applying a lambda now takes a store too.
;;
;; Otherwise, everything just returns the store unchanged.
(define value-of/store+deref+threaded-store
  (letrec ((value-of/store
            (lambda (e env store)
              (match e
                (,x (guard (symbol? x))
                    (cons (lookup x env) store))
                (,n (guard (number? n))
                    (cons n store))
                ((λ (,x) ,e)
                 (cons (lambda (v store)
                         (value-of/store e (extend-env x v env) store))
                       store))
                ((deref ,e)
                 (match (value-of/store e env store)
                   (((ref ,label) . ,store)
                    (cons (cdr (assq label store))
                          store))))
                ((,rator ,rand)
                 (match (value-of/store rator env store)
                   ((,rator . ,store)
                    (match (value-of/store rand env store)
                      ((,rand . ,store)
                       (rator rand store))))))))))
    value-of/store))

;; This is pretty good. Now we have all that we need to do ref.
(define value-of/store+deref+ref
  (letrec ((value-of/store
            (lambda (e env store)
              (match e
                (,x (guard (symbol? x))
                    (cons (lookup x env) store))
                (,n (guard (number? n))
                    (cons n store))
                ((λ (,x) ,e)
                 (cons (lambda (v store)
                         (value-of/store e (extend-env x v env) store))
                       store))
                ((ref ,e)
                 (match (value-of/store e env store)
                   ((,v . ,store)
                    (let ((label (gensym)))
                      (cons `(ref ,label)
                            (cons (cons label v) store))))))
                ((deref ,e)
                 (match (value-of/store e env store)
                   (((ref ,label) . ,store)
                    (cons (cdr (assq label store))
                          store))))
                ((,rator ,rand)
                 (match (value-of/store rator env store)
                   ((,rator . ,store)
                    (match (value-of/store rand env store)
                      ((,rand . ,store)
                       (rator rand store))))))))))
    value-of/store))

;; Update is similar.
(define value-of/store+deref+ref+update
  (letrec ((value-of/store
            (lambda (e env store)
              (match e
                (,x (guard (symbol? x))
                    (cons (lookup x env) store))
                (,n (guard (number? n))
                    (cons n store))
                ((λ (,x) ,e)
                 (cons (lambda (v store)
                         (value-of/store e (extend-env x v env) store))
                       store))
                ((ref ,e)
                 (match (value-of/store e env store)
                   ((,v . ,store)
                    (let ((label (gensym)))
                      (cons `(ref ,label)
                            (cons (cons label v) store))))))
                ((deref ,e)
                 (match (value-of/store e env store)
                   (((ref ,label) . ,store)
                    (cons (cdr (assq label store))
                          store))))
                ((update ,e-ref ,e-val)
                 (match (value-of/store e-ref env store)
                   (((ref ,label) . ,store)
                    (match (value-of/store e-val env store)
                      ((,val . ,store)
                       (cons `(ref ,label)
                             (cons (cons label val) store)))))))
                ((,rator ,rand)
                 (match (value-of/store rator env store)
                   ((,rator . ,store)
                    (match (value-of/store rand env store)
                      ((,rand . ,store)
                       (rator rand store))))))))))
    value-of/store))

;; Okay, cool. We're done.
;;
;; But, the new interpreter is significantly longer and harder to read
;; with all those conses. Half of the forms in the language don't even
;; touch the store, and yet everywhere must thread the store through
;; calls to value-of. What if we accidentally thread the wrong store?
;; We might accidentally fork the store, and other kinds of
;; badness. Let's see if we can make this better.
;;
;; It may not be obvious yet, but lets start by currying
;; (schönfinkeling) the store argument.
(define value-of/store+deref+ref+update/curried
  (letrec ((value-of/store
            (lambda (e env)
              (lambda (store)
                (match e
                  (,x (guard (symbol? x))
                      (cons (lookup x env) store))
                  (,n (guard (number? n))
                      (cons n store))
                  ((λ (,x) ,e)
                   (cons (lambda (v)
                           (lambda (store)
                             ((value-of/store e (extend-env x v env)) store)))
                         store))
                  ((ref ,e)
                   (match ((value-of/store e env) store)
                     ((,v . ,store)
                      (let ((label (gensym)))
                        (cons `(ref ,label)
                              (cons (cons label v) store))))))
                  ((deref ,e)
                   (match ((value-of/store e env) store)
                     (((ref ,label) . ,store)
                      (cons (cdr (assq label store))
                            store))))
                  ((update ,e-ref ,e-val)
                   (match ((value-of/store e-ref env) store)
                     (((ref ,label) . ,store)
                      (match ((value-of/store e-val env) store)
                        ((,val . ,store)
                         (cons `(ref ,label)
                               (cons (cons label val) store)))))))
                  ((,rator ,rand)
                   (match ((value-of/store rator env) store)
                     ((,rator . ,store)
                      (match ((value-of/store rand env) store)
                        ((,rand . ,store)
                         ((rator rand) store)))))))))))
    (lambda (e env store)
      ((value-of/store e env) store))))

;; Well, now it's even longer and even harder to read. Progress! (It's
;; like cleaning your room. Things get worse before they get better.)
;;
;; Let's push the (lambda (store) ...) in through all the match
;; branches. This way, it will be even longer! We can do this because
;; we don't need the store to match.
(define value-of/store+deref+ref+update/curried+pushed
  (letrec ((value-of/store
            (lambda (e env)
              (match e
                (,x (guard (symbol? x))
                    (lambda (store)
                      (cons (lookup x env) store)))
                (,n (guard (number? n))
                    (lambda (store)
                      (cons n store)))
                ((λ (,x) ,e)
                 (lambda (store)
                   (cons (lambda (v)
                           (lambda (store)
                             ((value-of/store e (extend-env x v env)) store)))
                         store)))
                ((ref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     ((,v . ,store)
                      (let ((label (gensym)))
                        (cons `(ref ,label)
                              (cons (cons label v) store)))))))
                ((deref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     (((ref ,label) . ,store)
                      (cons (cdr (assq label store))
                            store)))))
                ((update ,e-ref ,e-val)
                 (lambda (store)
                   (match ((value-of/store e-ref env) store)
                     (((ref ,label) . ,store)
                      (match ((value-of/store e-val env) store)
                        ((,val . ,store)
                         (cons `(ref ,label)
                               (cons (cons label val) store))))))))
                ((,rator ,rand)
                 (lambda (store)
                   (match ((value-of/store rator env) store)
                     ((,rator . ,store)
                      (match ((value-of/store rand env) store)
                        ((,rand . ,store)
                         ((rator rand) store)))))))))))
    (lambda (e env store)
      ((value-of/store e env) store))))

;; Notice on the variable and lambda lines all we do is cons the store
;; onto a value that doesn't depend on the store. Let's define a
;; function called return that does this for us.
(define return
  (lambda (v)
    (lambda (store)
      (cons v store))))

;; Now, we can use this function to make our interpreter shorter for
;; the first time in a while. Also, notice that (lambda (x) (p x)) ≡
;; p, so we will apply this in the lambda line as well.
(define value-of/store+deref+ref+update/curried+pushed+return
  (letrec ((value-of/store
            (lambda (e env)
              (match e
                (,x (guard (symbol? x))
                    (return (lookup x env)))
                (,n (guard (number? n))
                    (return n))
                ((λ (,x) ,e)
                 (return (lambda (v)
                           (value-of/store e (extend-env x v env)))))
                ((ref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     ((,v . ,store)
                      (let ((label (gensym)))
                        (cons `(ref ,label)
                              (cons (cons label v) store)))))))
                ((deref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     (((ref ,label) . ,store)
                      (cons (cdr (assq label store))
                            store)))))
                ((update ,e-ref ,e-val)
                 (lambda (store)
                   (match ((value-of/store e-ref env) store)
                     (((ref ,label) . ,store)
                      (match ((value-of/store e-val env) store)
                        ((,val . ,store)
                         (cons `(ref ,label)
                               (cons (cons label val) store))))))))
                ((,rator ,rand)
                 (lambda (store)
                   (match ((value-of/store rator env) store)
                     ((,rator . ,store)
                      (match ((value-of/store rand env) store)
                        ((,rand . ,store)
                         ((rator rand) store))))))))))) 
    (lambda (e env store)
      ((value-of/store e env) store))))

;; Excellent. Now the lambda line and the variable line look almost as
;; good as before.


;; Now let's see if we can simplify the application line. Once again,
;; we're just threading the store around. Let's define a function
;; called bind that does that for us.
;;
;; m is for monad, meaning it's a function that expects a store. We
;; return a (lambda (store) ...) to get a store.
(define bind
  (lambda (m f)
    (lambda (store)
      (match (m store)
        ((,v . ,store)
         ((f v) store))))))
;; This is starting to get a bit incomprehensible, but fortunately,
;; you'll never have to look at it again. Let's use this to clean up
;; the lambda line.
(define value-of/store+deref+ref+update/curried+pushed+return+bind
  (letrec ((value-of/store
            (lambda (e env)
              (match e
                (,x (guard (symbol? x))
                    (return (lookup x env)))
                (,n (guard (number? n))
                    (return n))
                ((λ (,x) ,e)
                 (return (lambda (v)
                           (value-of/store e (extend-env x v env)))))
                ((ref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     ((,v . ,store)
                      (let ((label (gensym)))
                        (cons `(ref ,label)
                              (cons (cons label v) store)))))))
                ((deref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     (((ref ,label) . ,store)
                      (cons (cdr (assq label store))
                            store)))))
                ((update ,e-ref ,e-val)
                 (lambda (store)
                   (match ((value-of/store e-ref env) store)
                     (((ref ,label) . ,store)
                      (match ((value-of/store e-val env) store)
                        ((,val . ,store)
                         (cons `(ref ,label)
                               (cons (cons label val) store))))))))
                ((,rator ,rand)
                 (bind (value-of/store rator env)
                       (lambda (rator)
                         (bind (value-of/store rand env)
                               (lambda (rand)
                                 (rator rand)))))))))) 
    (lambda (e env store)
      ((value-of/store e env) store))))
;; Now we can't see store at all in the application line!
;;
;; You might notice that these nested binds and lambdas looks a bit
;; like the standard expansion of a let expression. Let's make some
;; syntax. We'll call it do*, which is sort of like let* but it
;; transparently threads a store around.
(define-syntax do*
  (syntax-rules ()
    ((_ ((x e) rest ...) body)
     (bind e (lambda (x) (do* (rest ...) body))))
    ((_ () body)
     body)))
;; And now our application line looks pretty good.
(define value-of/store+deref+ref+update/curried+pushed+return+do
  (letrec ((value-of/store
            (lambda (e env)
              (match e
                (,x (guard (symbol? x))
                    (return (lookup x env)))
                (,n (guard (number? n))
                    (return n))
                ((λ (,x) ,e)
                 (return (lambda (v)
                           (value-of/store e (extend-env x v env)))))
                ((ref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     ((,v . ,store)
                      (let ((label (gensym)))
                        (cons `(ref ,label)
                              (cons (cons label v) store)))))))
                ((deref ,e)
                 (lambda (store)
                   (match ((value-of/store e env) store)
                     (((ref ,label) . ,store)
                      (cons (cdr (assq label store))
                            store)))))
                ((update ,e-ref ,e-val)
                 (lambda (store)
                   (match ((value-of/store e-ref env) store)
                     (((ref ,label) . ,store)
                      (match ((value-of/store e-val env) store)
                        ((,val . ,store)
                         (cons `(ref ,label)
                               (cons (cons label val) store))))))))
                ((,rator ,rand)
                 (do* ((rator (value-of/store rator env))
                       (rand  (value-of/store rand  env)))
                      (rator rand)))))))
    (lambda (e env store)
      ((value-of/store e env) store))))

;; The only nasty lines now are the ones relating to references. Let's
;; hide them in helper functions.
(define (create-ref val)
  (lambda (store)
    (let ((label (gensym)))
      (cons `(ref ,label)
            (cons (cons label val) store)))))

(define (get-ref ref)
  (lambda (store)
    (match ref
      ((ref ,label)
       (cons (cdr (assq label store)) store)))))

(define (set-ref ref val)
  (lambda (store)
    (match ref
      ((ref ,label)
       (cons ref (cons (cons label val) store))))))

;; We can use these to finish the interpreter.
(define value-of/store-monadic
  (letrec ((value-of/store
            (lambda (e env)
              (match e
                (,x (guard (symbol? x))
                    (return (lookup x env)))
                (,n (guard (number? n))
                    (return n))
                ((λ (,x) ,e)
                 (return (lambda (v)
                           (value-of/store e (extend-env x v env)))))
                ((ref ,e)
                 (do* ((val (value-of/store e env)))
                      (create-ref val)))
                ((deref ,e)
                 (do* ((ref (value-of/store e env)))
                      (get-ref ref)))
                ((update ,e-ref ,e-val)
                 (do* ((ref (value-of/store e-ref env))
                       (val (value-of/store e-val env)))
                      (set-ref ref val)))
                ((,rator ,rand)
                 (do* ((rator (value-of/store rator env))
                       (rand  (value-of/store rand  env)))
                      (rator rand)))))))
    (lambda (e env store)
      ((value-of/store e env) store))))

;; Look how nice that is! You can't tell that there's a store being
;; threaded around anywhere. Now you can easily add new forms to the
;; language without having to worry about threading things the right
;; way.
;;
;; Notice how if we had written this monadically to start with, all
;; we'd have to do is add three short clauses to the interpreter,
;; rather than rewriting everything.


;; This expression uses all parts of the interpreter:
(define test-expr '(deref (update ((λ (x) (ref x)) 5) 120)))
