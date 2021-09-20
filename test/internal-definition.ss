(define a 100)
(define b 200)

(let ()
  (define a 1)
  (define b 2)
  (print (+ a b)))

(define hoge "global:hoge")
(define fuga "global:fuga")

(let ((define (lambda (var expr)
                (print "variable = " var ", expression = " expr))))
  (define hoge "local:hoge")
  (define fuga "local:fuga")
  (print hoge)
  (print fuga))
