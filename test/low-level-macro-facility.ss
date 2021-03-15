(define x 42)

(check x => 42)

(define hygienic-x (syntax x))

(define rename.v1
  (let ((e (fork/csc identity)))
    (lambda (x)
      (eval x e))))

(let ((x 3.14))
  (check x => 3.14)
  (check (hygienic-x) => 42)
  (check (macroexpand-1 '(hygienic-x)) => 42)
  (check (rename.v1 'x) => 42)
  )

; (define-syntax (swap! x y)
;   `(,let ((,value ,x))
;      (,set! ,x ,y)
;      (,set! ,y ,value)))

(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare)

      (check (syntactic-continuation? (rename 'let)) => #t)

      (let ((a (cadr form))
            (b (caddr form)))
        `(,(rename 'let) ((,(rename 'value) ,a))
           (,(rename 'set!) ,a ,b)
           (,(rename 'set!) ,b ,(rename 'value)))))))

(check (let ((x 1)
             (y 2))
         (swap! x y)
         (cons x y)) => (2 . 1))

(check (let ((x 1)
             (y 2)
             (let '())
             (set! '())
             (value 42))
         (swap! x y)
         (cons x y)) => (2 . 1))

(check-report)

(emergency-exit (check-passed? check:correct))
