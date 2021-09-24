(check (symbol? (syntax x)) => #f)

; (check
;   (let-syntax ((car
;                  (lambda (x)
;                    (syntax car))))
;     ((car) '(0)))
;   => 0)

; (check
;   (let-syntax ((quote-quote
;                  (lambda (x)
;                    (list (syntax quote) 'quote))))
;     (quote-quote))
;   => quote)

; (check
;   (let-syntax ((quote-quote
;                  (lambda (x)
;                    (list 'quote 'quote))))
;     (quote-quote))
;   => error)

; (check
;   (let-syntax ((quote-me
;                  (lambda (x)
;                    (list (syntax quote) x))))
;     (quote-me please))
;   => (quote-me please))

; (check
;   (let ((x 0))
;     (let-syntax ((alpha
;                    (lambda (e) (syntax x))))
;       (alpha)))
;   => 0)

; (check
;   (let ((x 0))
;     (let-syntax ((alpha
;                    (lambda (x) (syntax x))))
;       (alpha)))
;   => error)

; (let-syntax ((alpha
;                (let-syntax ((beta
;                               (syntax-rules ()
;                                 ((beta) 0))))
;                  (lambda (x)
;                    (syntax (beta))))))
;   (alpha))
;   => error)

; (check
;   (let-syntax ((alpha
;                  (syntax-rules ()
;                    ((alpha) 0))))
;     (let-syntax ((beta
;                    (lambda (x)
;                      (alpha))))
;       (beta)))
;   => 0)

; (check
;   (let ((list 0))
;     (let-syntax ((alpha
;                    (lambda (x)
;                      (list 0))))
;       (alpha)))
;   => error)

(check (identifier? (syntax x)) => #t)
; (check (identifier? (quote x)) => #f)
(check (identifier? 3) => #f)

; (check
;   (identifier?
;     (unwrap-syntax (syntax x)))
;   => #t)
;
; (check
;   (identifier?
;     (car (unwrap-syntax (syntax x))))
;   => #t)
;
; (check
;   (unwrap-syntax
;     (cdr (unwrap-syntax (syntax x))))
;   => ())

; (check
;   (free-identifier=? (syntax x)
;                      (syntax x))
;   => #t)

; (check
;   (free-identifier=? (syntax x)
;                      (syntax y))
;   => #f)

; (check
;   (let ((x (syntax x)))
;     (free-identifier=? x (syntax x)))
;   => #f)

; (check
;   (let-syntax ((alpha
;                  (lambda (x)
;                    (free-identifier=? (car (unwrap-syntax x))
;                                       (syntax alpha)))))
;     (alpha))
;   => #f)

; (check
;   (letrec-syntax ((alpha
;                     (lambda (x)
;                       (free-identifier=? (car (unwrap-syntax x))
;                                          (syntax alpha)))))
;     (alpha))
;   => #t)

; (check
;   (bound-identifier=? (syntax x)
;                       (syntax x))
;   => #t)

; (check
;   (letrec-syntax
;     ((alpha
;        (lambda (x)
;          (bound-identifier=? (car (unwrap-syntax x))
;                              (syntax alpha)))))
;     (alpha))
;   => #f)

; (check
;   (symbol? (identifier->symbol (syntax x)))
;   => #t)

; (check
;   (identifier->symbol (syntax x))
;   => x)

; (check
;   (identifier->symbol (generate-identifier 'x))
;   => x)

; (check
;   (bound-identifier=? (generate-identifier 'x)
;                       (generate-identifier 'x))
;   => #f)

; (define-syntax set*! ; (set*! (<identifier> <expression>) ...)
;   (lambda (x)
;     (letrec
;       ((unwrap-exp
;          (lambda (x)
;            (let ((x (unwrap-syntax x)))
;              (if (pair? x)
;                  (cons (car x)
;                        (unwrap-exp (cdr x)))
;                  x)))))
;       (let ((sets (map unwrap-exp
;                        (cdr (unwrap-exp x)))))
;         (let ((ids (map car sets))
;               (vals (map cadr sets))
;               (temps (map (lambda (x)
;                             (generate-identifier))
;                           sets)))
;           `(,(syntax let) ,(map list temps vals)
;              ,@(map (lambda (id temp)
;                       `(,(syntax set!) ,id ,temp))
;                     ids
;                     temps)
;              #f))))))

; (define-syntax loop-until-exit
;   (lambda (x)
;     (let ((exit (construct-identifier
;                   (car (unwrap-syntax x))
;                   'exit))
;           (body (car (unwrap-syntax
;                        (cdr (unwrap-syntax x))))))
;       `(,(syntax call-with-current-continuation)
;          (,(syntax lambda)
;            (,exit)
;            (,(syntax letrec)
;              ((,(syntax loop)
;                 (,(syntax lambda) ()
;                   ,body
;                   (,(syntax loop)))))
;              (,(syntax loop))))))))

; ------------------------------------------------------------------------------

(check-report)

(emergency-exit (check-passed? check:correct))
