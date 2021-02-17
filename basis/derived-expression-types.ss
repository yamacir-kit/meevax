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
                                       (list (car (cddr clause)) result))
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


(define-syntax (quasiquote template)

  (define (list . xs) xs)

  (define (null? x) (eqv? x '()))

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cddr x) (cdr (cdr x)))

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

  (define (expand x depth)
    (cond
      ((pair? x)
       (cond

         ((free-identifier=? unquote (car x))
          (if (<= depth 0)
              (cadr x)
              (list list (list quote unquote) (expand (cadr x) (- depth 1)))))

         ((free-identifier=? unquote-splicing (car x))
          (if (<= depth 0)
              (list cons (expand (car x) depth)
                         (expand (cdr x) depth))
              (list list (list quote unquote-splicing)
                         (expand (cadr x) (- depth 1)))))

         ((free-identifier=? quasiquote (car x))
          (list list (list quote quasiquote)
                     (expand (cadr x) (+ depth 1))))

         ((and (<= depth 0)
               (pair? (car x))
               (free-identifier=? unquote-splicing (caar x)))
          (if (null? (cdr x))
              (cadr (car x))
              (list append (cadr (car x)) (expand (cdr x) depth))))

         (else (list cons (expand (car x) depth)
                          (expand (cdr x) depth)))))

      ((vector? x)
       (list list->vector (expand (vector->list x) depth)))

      ((or (identifier? x)
           (null? x))
       (list quote x))

      (else x)))

  (expand template 0))
