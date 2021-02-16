(define fork/csc fork-with-current-syntactic-continuation)

(define define-syntax
  (fork/csc
    (lambda (define-syntax keyword . transformer)

      (define (list . xs) xs)

      (if (pair? keyword)

          ; (define-syntax (<keyword> <formals>) <body>)
          ;
          ; => (define <keyword>
          ;      (fork/csc
          ;        (lambda (<keyword> . <formals>) <body>)))
          ;
          (list define (car keyword)
                (list fork/csc
                      (list lambda keyword . transformer)))

          ; (define-syntax <keyword> <transformer>)
          ;
          ; => (define <keyword> <transformer>)
          ;
          (list define keyword . transformer)))))
