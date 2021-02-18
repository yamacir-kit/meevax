(define (identity x) x)

(define (not x) (if x #f #t))

(define (list . xs) xs)

(define (null? x) (eqv? x '()))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (unspecified) (if #f #f))

(define (append-2 x y)
  (if (null? x) y
      (cons (car x)
            (append-2 (cdr x) y))))

(define (reverse x)
  (if (null? x) '()
      (append-2 (reverse (cdr x))
                (list (car x)))))

(define (append . xs) ; Simpler than SRFI-1

  (define (append-aux x xs)
    (if (null? x) xs
        (append-aux (cdr xs)
                    (append-2 (car x) xs))))

  (if (null? xs) '()
      ((lambda (xs)
         (append-aux (cdr xs)
                     (car xs)))
       (reverse xs))))

(define (free-identifier=? x y)
  (if (symbol? x)
      (if (symbol? y)
          (eq? x y)
          (if (syntactic-closure? y)
              (eq? x (car y))
              #f))
      (if (syntactic-closure? x)
          (if (syntactic-closure? y)
              (eqv? x y)
              (if (symbol? y)
                  (eq? (car x) y)
                  #f))
          #f)))

(define fork/csc fork-with-current-syntactic-continuation)

(define define-syntax
  (fork/csc
    (lambda (define-syntax keyword . transformer)

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

; (define er-macro-transformer ; unstable
;   (lambda (transform)
;     (fork/csc
;       (lambda form
;         (transform form (lambda (x) (eval x (car form))) free-identifier=?)))))

(define er-macro-transformer ; unhygienic-dummy
  (lambda (transform)
    (fork/csc
      (lambda form
        (transform form identity eqv?)))))
