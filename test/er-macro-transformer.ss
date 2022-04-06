(define-syntax swap!
  (hygienic-macro-transformer
    (lambda (a b)
      `(,let ((,x ,a))
         (,set! ,a ,b)
         (,set! ,b ,x)))))

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

(swap! x y)

(check (cons x y) => (2 . 1))

; ------------------------------------------------------------------------------

(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
       `(,(rename 'let) ((,(rename 'x) ,a))
          (,(rename 'set!) ,a ,b)
          (,(rename 'set!) ,b ,(rename 'x)))))))

(check (er-macro-transformer? swap!) => #t)

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

(swap! x y)

(check (cons x y) => (2 . 1))

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? check:correct))
