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


(define-syntax (cond . clauses)

  (define (list . xs) xs)

  (define (null? x) (eqv? x '()))

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cddr x) (cdr (cdr x)))

  (define (caddr x) (car (cdr (cdr x))))

  (define (unspecified) (if #f #f))

  (if (null? clauses)
      (unspecified)
      ((lambda (clause)
         (if (free-identifier=? else (car clause))
             (if (pair? (cdr clauses))
                 (error "else clause must be at the end of cond clause" clauses)
                 (cons begin (cdr clause)))
             (if (if (null? (cdr clause)) #t
                     (free-identifier=? => (cadr clause)))
                 (list (list lambda (list result)
                             (list if result
                                   (if (null? (cdr clause)) result
                                       (list (caddr clause) result))
                                   (cons cond (cdr clauses))))
                       (car clause))
                 (list if (car clause)
                          (cons begin (cdr clause))
                          (cons cond (cdr clauses))))))
       (car clauses))))


(define-syntax (and . tests)

  (define (list . xs) xs)

  (define (null? x) (eqv? x '()))

  (cond ((null? tests))
        ((null? (cdr tests)) (car tests))
        (else (list if (car tests)
                    (cons and (cdr tests))
                    #f))))


(define-syntax (or . tests)

  (define (list . xs) xs)

  (define (null? x) (eqv? x '()))

  (cond
    ((null? tests) #f)
    ((null? (cdr tests)) (car tests))
    (else (list (list lambda (list result)
                  (list if result
                           result
                           (cons or (cdr tests))))
                (car tests)))))
