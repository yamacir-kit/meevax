(experimental:define-syntax swap!
  (traditional-macro-transformer
    (lambda (a b)
      `(let ((value ,a))
         (set! ,a ,b)
         (set! ,b value)))))

(check (transformer? swap!) => #t)

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

; (print (macroexpand-1 '(swap! x y)))

(swap! x y)

(check (cons x y) => (2 . 1))

; ; ------------------------------------------------------------------------------

(experimental:define-syntax swap!
  (sc-macro-transformer
    (lambda (form use-env)
      (let ((a (make-syntactic-closure use-env '() (cadr form)))
            (b (make-syntactic-closure use-env '() (caddr form))))
        `(let ((value ,a))
           (set! ,a ,b)
           (set! ,b value))))))

(check (transformer? swap!) => #t)

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

; (print (macroexpand-1 '(swap! x y)))

(let ((a     'non-hygienic!)
      (b     'non-hygienic!)
      (let   'non-hygienic!)
      (set!  'non-hygienic!)
      (value 'non-hygienic!))
  (swap! x y))

(check (cons x y) => (2 . 1))

; ------------------------------------------------------------------------------

(experimental:define-syntax swap!
  (rsc-macro-transformer
    (lambda (form mac-env)
      (let ((a (cadr form))
            (b (caddr form))
            (LET   (make-syntactic-closure mac-env '() 'let))
            (VALUE (make-syntactic-closure mac-env '() 'value))
            (SET!  (make-syntactic-closure mac-env '() 'set!)))
        `(,LET ((,VALUE ,a))
           (,SET! ,a ,b)
           (,SET! ,b ,VALUE))))))

(check (transformer? swap!) => #t)

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

; (print (macroexpand-1 '(swap! x y)))

(let ((a     'non-hygienic!)
      (b     'non-hygienic!)
      (let   'non-hygienic!)
      (set!  'non-hygienic!)
      (value 'non-hygienic!))
  (swap! x y))

(check (cons x y) => (2 . 1))

; ------------------------------------------------------------------------------

(experimental:define-syntax swap!
  (experimental:er-macro-transformer
    (lambda (form rename compare?)
      (let ((a (cadr form))
            (b (caddr form))
            (LET   (rename 'let))
            (VALUE (rename 'value))
            (SET!  (rename 'set!)))
        `(,LET ((,VALUE ,a))
          (,SET! ,a ,b)
          (,SET! ,b ,VALUE))))))

(check (transformer? swap!) => #t)

(define x 1)

(define y 2)

(check (cons x y) => (1 . 2))

; (print (macroexpand-1 '(swap! x y)))

(let ((a     'non-hygienic!)
      (b     'non-hygienic!)
      (let   'non-hygienic!)
      (set!  'non-hygienic!)
      (value 'non-hygienic!))
  (swap! x y))

(check (cons x y) => (2 . 1))

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? check:correct))
