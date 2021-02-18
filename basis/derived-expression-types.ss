(define-syntax (cond . clauses)

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

  (cond ((null? tests))
        ((null? (cdr tests)) (car tests))
        (else (list if (car tests)
                    (cons and (cdr tests))
                    #f))))


(define-syntax (or . tests)

  (cond
    ((null? tests) #f)
    ((null? (cdr tests)) (car tests))
    (else (list (list lambda (list result)
                  (list if result
                           result
                           (cons or (cdr tests))))
                (car tests)))))


(define-syntax (quasiquote template)

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
