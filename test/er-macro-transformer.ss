(define-syntax (er-macro-transformer expression)
  (define evaluate (current-evaluator))
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

(print (cons x y))
(swap! x y)
(print (cons x y))
