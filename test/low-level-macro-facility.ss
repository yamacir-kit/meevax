(define x 42)

(check x => 42)

; (define hygienic-x (syntax x))

(define rename
  (let ((e (hygienic-macro-transformer list)))
    (lambda (x)
      (eval x e))))

(let ((x 3.14))
  (check x => 3.14)
  ; (check hygienic-x => 42)
  (check (rename 'x) => 42))

; (define-syntax (swap! x y)
;   `(,let ((,value ,x))
;      (,set! ,x ,y)
;      (,set! ,y ,value)))

(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (check (transformer? (rename 'let)) => #t)
      (check (identifier? (rename 'value)) => #t)
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

(exit (check-passed? check:correct))
