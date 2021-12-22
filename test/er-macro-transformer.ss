(define-syntax (er-macro-transformer expression)
  (define (evaluate x)
    (eval x er-macro-transformer))
  (define transform (evaluate expression))
  (fork/csc
    (lambda form
      (transform form evaluate free-identifier=?))))

(define swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
       `(,(rename 'let) ((,(rename 'x) ,a))
          (,(rename 'set!) ,a ,b)
          (,(rename 'set!) ,b ,(rename 'x)))))))

(define x 1)
(define y 2)

(check (cons x y) => (1 . 2))
(swap! x y)
(check (cons x y) => (2 . 1))

(check-report)

(exit (check-passed? check:correct))
