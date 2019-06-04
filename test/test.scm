(load "../setup.scm")

(define x 42)
x
(set! x 100)
x

(define y 'hoge)
y
(set! y 100)
y

(define accumulator
  (lambda (n)
    (lambda ()
      (set! n (+ n 1)))))

(define acc (accumulator x))
(acc)
(acc)
(acc)

(or)
(or #true)
(or #false)
(or (eq? 'a 'a) (eq? 'b 'b))
(or (eq? 'a 'b) (eq? 'c 'c))
(or (eq? 'a 'b) (eq? 'c 'd))

(define a '(1 2 3))

`(a b c); => (a b c)
`(,a b c); => ((1 2 3) b c)
`(,@a b c); => (1 2 3 b c)

(define map
  (lambda (callee &list)
    (if (null? &list)
       '()
        (cons (callee (car &list))
              (map callee (cdr &list))))))

; (define let
;   (macro (bindings . body)
;    `((lambda ,(map car bindings) ,@body)
;      ,@(map cadr bindings))))

(let ((a 1)
      (b 2))
  (+ a b))

(define when
  (macro (<test> . <expression>)
   `(if ,@<test> (begin ,@<expression>))))

(define unless
  (macro (<test> . <expression>)
   `(if (not ,@<test>) (begin ,@<expression>))))

(begin (display 1)
       (display 2)
       (display 3))

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

