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

(define let
  (macro (bindings . body)
    `((lambda ,(map car bindings) ,@body)
      ,@(map cadr bindings))))

(let ((a 1)
      (b 2))
  (+ a b))

