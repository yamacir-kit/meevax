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

(emergency-exit)

