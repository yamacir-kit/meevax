; 6.1 Equivalent Predicates

(eqv? 'a 'a); -> #true
(eqv? 'a 'b); -> #false

(eqv? 2 2); -> #true
(eqv? 2 2.0); -> #false

(eqv? '() '()); -> #true

(eqv? 100000000 100000000); -> #true

(eqv? 0.0 +nan.0); -> #false

(eqv? (cons 1 2)
      (cons 1 2)
); -> #false

(eqv? (lambda () 1)
      (lambda () 2)
); -> #false

(let (
       (p (lambda (x) x))
     )
  (eqv? p p)
); -> #true

(eqv? #false 'nil); -> #false

(eqv? "" ""); -> #<unspecified>
(eqv? '#() '#()); -> #<unspecified>

(eqv? (lambda (x) x)
      (lambda (x) x)
); -> #<unspecified>

(eqv? (lambda (x) x)
      (lambda (x) y)
); -> #<unspecified>

(eqv? 1.0e0 1.0f0); -> #<unspecified>
(eqv? +nan.0 +nan.0); -> #<unspecified>

(define generate-counter
  (lambda ()
    (let (
           (n 0)
         )
      (lambda () (set! n (+ n 1)) n)
    )
  )
)

(let (
       (g (generate-counter))
     )
  (eqv? g g)
); -> #true

(eqv? (generate-counter) (generate-counter)); -> #false

(define generate-loser
  (lambda ()
    (let (
           (n 0)
         )
      (lambda () (set! n (+ n 1)) 27)
    )
  )
)

(let (
       (g (generate-loser))
     )
  (eqv? g g)
); -> #true

(eqv? (generate-loser) (generate-loser)); -> #<unspecified>

(letrec (
          (f (lambda () (if (eqv? f g) 'both 'f)))
          (g (lambda () (if (eqv? f g) 'both 'g)))
        )
 (eqv? f g)
); -> #<unspecified>

(letrec (
          (f (lambda () (if (eqv? f g) ’f ’both)))
          (g (lambda () (if (eqv? f g) ’g ’both)))
        )
  (eqv? f g)
); -> #false

(eqv? '(a) '(a)); -> #<unspecified>
(eqv? "a" "a"); -> #<unspecified>
(eqv? '(b) (cdr '(a b))); -> #<unspecified>

(let (
       (x '(a))
     )
  (eqv? x x)
); -> #true

(eq? 'a 'a); -> #true
(eq? '(a) '(a)); -> #<unspecified>
(eq? (list 'a) (list 'a)); -> #false

(eq? "a" "a"); -> #<unspecified>
(eq? "" ""); -> #<unspecified>

(eq? '() '()); -> #true
(eq? 2 2); -> #<unspecified>
(eq? #\A #\A); -> #<unspecified>
(eq? car car); -> #true

(let (
       (n (+ 2 3))
     )
  (eq? n n)
); -> #<unspecified>

(let (
       (x '(a))
     )
  (eq? x x)
); -> #true

(let (
       (x '#())
     )
  (eq? x x)
); -> #true

(let (
       (p (lambda (x) x))
     )
  (eq? p p)
); -> #true

(equal? 'a 'a); -> #true
(equal? '(a) '(a)); -> #true
(equal? '(a (b) c)
        '(a (b) c)
); -> #true

(equal? "abc" "abc"); -> #true
(equal? 2 2); -> #true

(equal? (make-vector 5 'a)
        (make-vector 5 'a)
); -> #true

(equal? '#1=(a b . #1#)
        '#2=(a b a b . #2#)
); -> #true

(equal? (lambda (x) x)
        (lambda (y) y)
); -> #<unspecified>

