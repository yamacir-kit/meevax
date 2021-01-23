(define null-environment
  (fork-with-current-syntactic-continuation
    (lambda (this) this)))

(define fork/csc fork-with-current-syntactic-continuation)

(define identity
  (lambda (x) x))

(define unspecified
  (lambda ()
    (if #false #false #;unspecified)))

; ------------------------------------------------------------------------------
;  6.3 Booleans (Part 1 of 2)
; ------------------------------------------------------------------------------

(define not
  (lambda (x)
    (if x #f #t)))

; ------------------------------------------------------------------------------
;  6.4 Pairs and Lists (Part 1 of 2)
; ------------------------------------------------------------------------------

(define append-2 ; from SICP
  (lambda (x y)
    (if (null? x) y
        (cons (car x)
              (append-2 (cdr x) y)))))

(define reverse ; simple but slow
  (lambda (x)
    (if (null? x) '()
        (append-2 (reverse (cdr x))
                  (list (car x))))))

; ==== Low-Level Macro Facility ================================================

(define define-syntax
  (fork/csc
    (lambda (define-syntax identifier . transformer)
      (if (pair? identifier)
          (list define (car identifier)
            (list fork/csc
              (list lambda identifier . transformer)))
          (list define identifier . transformer)))))

(define free-identifier=?
  (lambda (x y)
    (if (symbol? x)
        (if (symbol? y)
            (eq? x y)
            (if (syntactic-closure? y)
                (eq? x (car y))
                #false))
        (if (syntactic-closure? x)
            (if (syntactic-closure? y)
                (eqv? x y)
                (if (symbol? y)
                    (eq? (car x) y)
                    #false))
            #false))))

; (define er-macro-transformer
;   (lambda (transform)
;     (fork/csc
;       (lambda form
;         (transform form (lambda (x) (eval x (car form))) free-identifier=?)))))

(define er-macro-transformer ; unhygienic-dummy
  (lambda (transform)
    (fork/csc
      (lambda form
        (transform form identity eqv?)))))

; --------------------------------------------------------------------------
;  4.2.1 Standard Conditional Library (Part 1 of 2)
; --------------------------------------------------------------------------

(define-syntax cond
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cdr form))
          (unspecified)
          ((lambda (clause)
             (if (compare (rename 'else) (car clause))
                 (if (pair? (cddr form))
                     (error "else clause must be at the end of cond clause" form)
                     (cons (rename 'begin) (cdr clause)))
                 (if (if (null? (cdr clause)) #t
                         (compare (rename '=>) (cadr clause)))
                     (list (list (rename 'lambda) (list (rename 'result))
                                 (list (rename 'if) (rename 'result)
                                       (if (null? (cdr clause))
                                           (rename 'result)
                                           (list (car (cddr clause)) (rename 'result)))
                                       (cons (rename 'cond) (cddr form))))
                           (car clause))
                     (list (rename 'if) (car clause)
                           (cons (rename 'begin) (cdr clause))
                           (cons (rename 'cond) (cddr form))))))
           (cadr form))))))

; (define-syntax (cond . clauses)
;   (if (null? clauses)
;       (if #f #f)
;       ((lambda (clause)
;          (if (free-identifier=? else (car clause))
;              (if (pair? (cdr clauses))
;                  (error "else clause must be at the end of cond clause" clauses)
;                  (cons begin (cdr clause)))
;              (if (if (null? (cdr clause)) #t
;                      (free-identifier=? => (cadr clause)))
;                  (list (list lambda (list result)
;                              (list if result
;                                    (if (null? (cdr clause)) result
;                                        (list (caddr clause) result))
;                                    (cons cond (cdr clauses))))
;                        (car clause))
;                  (list if (car clause)
;                           (cons begin (cdr clause))
;                           (cons cond (cdr clauses))))))
;        (car clauses))))

(define-syntax and
  (er-macro-transformer
    (lambda (form rename compare)
      (cond ((null? (cdr form)))
            ((null? (cddr form)) (cadr form))
            (else (list (rename 'if) (cadr form)
                        (cons (rename 'and) (cddr form))
                        #f))))))

; (define-syntax (and . tests)
;   (cond ((null? tests))
;         ((null? (cdr tests)) (car tests))
;         (else (list if (car tests)
;                     (cons and (cdr tests))
;                     #f))))

(define-syntax or
  (er-macro-transformer
    (lambda (form rename compare)
      (cond ((null? (cdr form)) #f)
            ((null? (cddr form)) (cadr form))
            (else
              (list (list (rename 'lambda) (list (rename 'result))
                          (list (rename 'if) (rename 'result)
                                (rename 'result)
                                (cons (rename 'or) (cddr form))))
                    (cadr form)))))))

; (define-syntax (or . tests)
;   (cond
;     ((null? tests) #false)
;     ((null? (cdr tests)) (car tests))
;     (else (list (list lambda (list result)
;                   (list if result
;                            result
;                            (cons or (cdr tests))))
;                 (car tests)))))

; --------------------------------------------------------------------------
;  4.2.8 Quasiquotations
; --------------------------------------------------------------------------

; (define unquote          identity)
; (define unquote-splicing identity)

(define-syntax quasiquote ; from Chibi-Scheme
  (er-macro-transformer
    (lambda (expr rename compare)
      (define (qq x d)
        (cond

          ((pair? x)
           (cond

             ((compare (rename 'unquote) (car x))
              (if (<= d 0)
                  (cadr x)
                  (list (rename 'list)
                        (list (rename 'quote) 'unquote)
                        (qq (cadr x) (- d 1)))))

             ((compare (rename 'unquote-splicing) (car x))
              (if (<= d 0)
                  (list (rename 'cons) (qq (car x) d)
                                       (qq (cdr x) d))
                  (list (rename 'list)
                        (list (rename 'quote) 'unquote-splicing)
                        (qq (cadr x) (- d 1)))))

             ((compare (rename 'quasiquote) (car x))
              (list (rename 'list)
                    (list (rename 'quote) 'quasiquote)
                    (qq (cadr x) (+ d 1))))

             ((and (<= d 0)
                   (pair? (car x))
                   (compare (rename 'unquote-splicing) (caar x)))
              (if (null? (cdr x))
                  (cadr (car x))
                  (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))

             (else (list (rename 'cons)
                         (qq (car x) d)
                         (qq (cdr x) d)))))

          ((vector? x)
           (list (rename 'list->vector)
                 (qq (vector->list x) d)))

          ((if (identifier? x) #t
               (null? x))
           (list (rename 'quote) x))

          (else x)))

      (qq (cadr expr) 0))))

; (define-syntax (quasiquote x)
;
;   (define quasiquote-expand
;     (lambda (e depth)
;       (if (not (pair? e))
;           (list 'quote e)
;           (if (eqv? (car e) 'quasiquote)
;               (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1)))
;               (if (eqv? (car e) 'unquote)
;                   (if (< 0 depth)
;                       (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1)))
;                       (if (and (not (null? (cdr e))) (null? (cddr e)))
;                           (cadr e)
;                           (error "illegal unquote")))
;                   (if (eqv? (car e) 'unquote-splicing)
;                       (if (< 0 depth)
;                           (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1)))
;                           (error "illegal unquote-splicing"))
;                       (list 'append (quasiquote-expand-list (car e) depth)
;                                     (quasiquote-expand      (cdr e) depth) )))))))
;
;   (define quasiquote-expand-list
;     (lambda (e depth)
;       (if (not (pair? e))
;           (list 'quote (list e))
;           (if (eqv? (car e) 'quasiquote)
;               (list 'list (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1)) ))
;               (if (eqv? (car e) 'unquote)
;                   (if (< 0 depth)
;                       (list 'list (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1)) ))
;                       (cons 'list (cdr e)))
;                   (if (eqv? (car e) 'unquote-splicing)
;                       (if (< 0 depth)
;                           (list 'list (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1)) ))
;                           (cons 'append (cdr e)))
;                       (list 'list (list 'append (quasiquote-expand-list (car e) depth)
;                                                 (quasiquote-expand      (cdr e) depth) ))))))))
;
;   (quasiquote-expand x 0) )


; ------------------------------------------------------------------------------
;  6.10 Control features (Part 1 of 2)
; ------------------------------------------------------------------------------

(define apply
  (lambda (procedure x . xs)

    (define apply-1
      (lambda (procedure xs)
        (procedure . xs) ))

    (if (null? xs)
        (apply-1 procedure x)
        ((lambda (rxs)
           (apply-1 procedure
                    (append-2 (reverse (cdr rxs))
                              (car rxs) )))
         (reverse (cons x xs)) ))))

(define map
  (lambda (procedure x . xs)

    (define map-1
      (lambda (procedure x result)
        (if (pair? x)
            (map-1 procedure
                   (cdr x)
                   (cons (procedure (car x)) result))
            (reverse result) )))

    (define map-n
      (lambda (procedure xs result)
        (if (every pair? xs)
            (map-n procedure
                   (map-1 cdr xs '())
                   (cons (apply procedure (map-1 car xs '())) result) )
            (reverse result) )))

    (if (null? xs)
        (map-1 procedure x '())
        (map-n procedure (cons x xs) '()) )))

(define for-each
  (lambda (procedure x . xs)

    (define for-each-1
      (lambda (procedure x)
        (if (pair? x)
            (begin (procedure (car x))
                   (for-each-1 procedure (cdr x)) ))))

    (if (null? xs)
        (for-each-1 procedure x)
        (begin (apply map procedure x xs)
               (unspecified) ))))

(define any
  (lambda (predicate x . xs)

    (define any-1
      (lambda (predicate x)
        (if (pair? (cdr x))
            ((lambda (result)
               (if result
                   result
                   (any-1 predicate (cdr x)) ))
             (predicate (car x)) )
            (predicate (car x)) )))

    (define any-n
      (lambda (predicate xs)
        (if (every pair? xs)
            ((lambda (result)
               (if result
                   result
                   (any-n predicate (map cdr xs)) ))
             (apply predicate (map car xs)) )
            #false )))

    (if (null? xs)
        (if (pair? x)
            (any-1 predicate x)
            #false )
        (any-n predicate (cons x xs)) )))

(define every
  (lambda (predicate x . xs)

    (define every-1
      (lambda (predicate x)
        (if (null? (cdr x))
            (predicate (car x))
            (if (predicate (car x))
                (every-1 predicate (cdr x))
                #false ))))

    (if (null? xs)
        (if (pair? x)
            (every-1 predicate x)
            #true )
        (not (apply any
                    (lambda xs
                      (not (apply predicate xs)) )
                    x xs) ))))

; ------------------------------------------------------------------------------
;  4.2.2 Binding constructs
; ------------------------------------------------------------------------------

(define-syntax letrec
  (er-macro-transformer
    (lambda (form rename compare)
      ((lambda (definitions)
         `((,(rename 'lambda) () ,@definitions ,@(cddr form))))
       (map (lambda (x)
              (cons (rename 'define) x))
            (cadr form))))))

; (define-syntax (letrec bindings . body)
;   ((lambda (definitions)
;     `((,lambda () ,@definitions ,@body)) )
;    (map (lambda (x) (cons 'define x)) bindings) ))

(define letrec* letrec)

(define-syntax (let bindings . body)
  (if (identifier? bindings)
      `(,letrec ((,bindings
                   (,lambda ,(map car (car body)) ,@(cdr body))))
         (,bindings ,@(map cadr (car body))))
      `((,lambda ,(map car bindings) ,@body)
        ,@(map cadr bindings))))

(define-syntax (let* bindings . body)

  (if (null? bindings)
      (error "The let* syntax is defined as the form (let* <bindings> <body>) \
              but lacks <bindings> and <body>.") )

  (if (null? body)
      (error "The let* syntax is defined as the form (let* <bindings> <body>) \
              but lacks <body>.") )

  (if (or (null? bindings)
          (null? (cdr bindings)) )
     `(,let (,(car bindings)) ,@body)
     `(,let (,(car bindings)) (,let* ,(cdr bindings) ,@body))))

; TODO let-values
; TODO let*-values

; ------------------------------------------------------------------------------
;  4.2.6. Dynamic bindings
; ------------------------------------------------------------------------------

; SRFI 39: Parameter objects

(define dynamic-environment '())

(define make-parameter
  (lambda (init . maybe-converter)
    (let* ((convert
             (if (null? maybe-converter)
                 (lambda (x) x)
                 (car maybe-converter)))
           (global-dynamic-environment
             (cons #f (convert init))))

      (define dynamic-lookup
        (lambda (parameter global-dynamic-environment)
          (or (assq parameter dynamic-environment) global-dynamic-environment)))

      (define parameter
        (lambda maybe-value
          (let ((binding
                  (dynamic-lookup parameter global-dynamic-environment)))
            (cond ((null? maybe-value)
                   (cdr binding))
                  ((null? (cdr maybe-value))
                   (set-cdr! binding (convert (car maybe-value))))
                  (else (convert (car maybe-value)))))))

      (set-car! global-dynamic-environment parameter)
      parameter)))

(define parameterize-aux
  (lambda (parameters values body)
    (let* ((saved dynamic-environment)
           (bindings
             (map (lambda (parameter value)
                    (cons parameter (parameter value #f)))
                  parameters
                  values)))
      (dynamic-wind
        (lambda () (set! dynamic-environment (append bindings saved)))
        body
        (lambda () (set! dynamic-environment                  saved))))))

(define-syntax parameterize
  (er-macro-transformer
    (lambda (form rename compare)
      (let* ((bindings (cadr form))
             (body (cddr form)))
        `(parameterize-aux
           (list ,@(map  car bindings))
           (list ,@(map cadr bindings))
           (lambda () ,@body))))))

; ------------------------------------------------------------------------------
;  6.4 Pairs and Lists (Part 2 of 2)
; ------------------------------------------------------------------------------

(define list? proper-list?)

(define dotted-list?
  (lambda (x)
    (let rec ((x x)
              (y x))
      (if (pair? x)
          (let ((x (cdr x)))
            (if (pair? x)
                (let ((x (cdr x))
                      (y (cdr y)))
                  (and (not (eq? x y))
                       (rec x y)))
                (not (null? x))))
          (not (null? x))))))

(define circular-list?
  (lambda (x)
    (let rec ((x x)
              (y x))
      (and (pair? x)
           (let ((x (cdr x)))
             (and (pair? x)
                  (let ((x (cdr x))
                        (y (cdr y)))
                    (or (eq? x y)
                        (rec x y)))))))))

(define null-list?
  (lambda (x)
    (cond
      ((pair? x) #false)
      ((null? x) #true)
      (else (error "from null-list?, argument out of domain" x) ))))

(define make-list
  (lambda (k . x)
    (let ((default (if (pair? x) (car x) #;unspecified)))
      (let rec ((k k)
                (result '()))
        (if (<= k 0) result
            (rec (- k 1)
                 (cons default result) ))))))

; (define length*
;   (lambda (x)
;     (let ((length (length x)))
;       (cond
;         ((positive? length) length)
;         ((= length -2) #false)
;         (else (let rec ((k 0)
;                         (x x))
;                 (if (not (pair? x)) k
;                     (rec (+ 1 i) (cdr x)))))))))

(define length*
  (lambda (x)
    (if (not (pair? x)) 0
        (let rec ((succeed x)
                  (precede (cdr x))
                  (result 1))
          (cond
            ((eq? succeed precede) #false)
            ((and (pair? precede)
                  (pair? (cdr precede)) )
             (rec (cdr succeed)
                  (cddr precede)
                  (+ 2 result) ))
            (else (if (pair? precede) (+ 1 result) result)) )))))

(define list-tail drop) ; SRFI-1

(define (list-set! x k object)
  (set-car! (list-tail x k) object))

; (define shallow-copy
;   (lambda (x)
;     (if (not (pair? x)) x
;         (cons (car x)
;               (cdr x)))))
;
; (define deep-copy
;   (lambda (x)
;     (if (not (pair? x)) x
;         (cons (deep-copy (car x))
;               (deep-copy (cdr x))))))

; ------------------------------------------------------------------------------
;  4.2.1 Conditionals (Part 2 of 2)
; ------------------------------------------------------------------------------

(define-syntax (case key . clauses)

  (define body
    (lambda (expressions)
      (cond
        ((null? expressions) result)
        ((free-identifier=? => (car expressions))
        `(,(cadr expressions) ,result))
        (else
         `(,begin ,@expressions) ))))

  (define each-clause
    (lambda (clauses)
      (cond
        ((null? clauses) #false)
        ((free-identifier=? else (caar clauses))
         (body (cdar clauses)))
        ((and (pair? (caar clauses))
              (null? (cdaar clauses)))
        `(,if (,eqv? ,result (,quote ,(caaar clauses)))
             ,(body (cdar clauses))
             ,(each-clause (cdr clauses))))
        (else
         `(,if (,memv ,result (,quote ,(caar clauses)))
              ,(body (cdar clauses))
              ,(each-clause (cdr clauses))) ))))

 `(,let ((,result ,key))
   ,(each-clause clauses)) )

(define-syntax (when test . body)
  `(,if ,test (,begin ,@body)))

(define-syntax (unless test . body)
 `(,if (,not ,test) (,begin ,@body)))

; ------------------------------------------------------------------------------
;  4.2.4 Iteration
; ------------------------------------------------------------------------------

(define-syntax (do variables test . commands)
  (let ((body
         `(,begin ,@commands
                  (,rec ,@(map (lambda (x)
                                 (if (pair? (cddr x))
                                     (caddr x)
                                     (car x)))
                               variables)))))
   `(,let ,rec ,(map (lambda (x)
                       (list (car x)
                             (cadr x)))
                     variables)
     ,(if (null? (cdr test))
         `(,let ((,result ,(car test)))
            (,if ,result ,result ,body))
         `(,if ,(car test) (,begin ,@(cdr test)) ,body)))))

; ------------------------------------------------------------------------------
;  4.2.5 Delayed Evaluation
; ------------------------------------------------------------------------------

(define <promise> (list 'promise))

(define promise ; ((#false . #,(closure ...)) promise)
  (lambda (forced? closure)
    (cons (cons forced? closure) <promise>)))

(define promise?
  (lambda (x)
    (and (pair? x)
         (eq? <promise> (cdr x)))))

(define force
  (lambda (promise)

    (define done? caar)

    (define cache cdar)

    (define update!
      (lambda (new old)
        (set-car! (car old) (done? new))
        (set-cdr! (car old) (cache new))
        (set-car! new (car old))))

    (if (done? promise)
        (cache promise)
        (let ((new ((cache promise))))
          (unless (done? promise)
                  (update! new promise))
          (force promise)))))

(define-syntax delay-force
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'promise) #f (,(rename 'lambda) () ,(cadr form))))))

(define-syntax delay
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'delay-force)
         (,(rename 'promise) #t ,(cadr form))))))

(define make-promise
  (lambda (x)
    (if (promise? x) x
        (delay x))))

; ------------------------------------------------------------------------------
;  6.1 Standard Equivalence Predicates Library (Part 2 of 2)
; ------------------------------------------------------------------------------

(define equal? ; list-equal?
  (lambda (x y)
    (if (and (pair? x)
             (pair? y))
        (and (equal? (car x)
                     (car y))
             (equal? (cdr x)
                     (cdr y)))
        (eqv? x y))))

; ------------------------------------------------------------------------------
;  6.2 Standard Numerical Library (Part 2 of 2)
; ------------------------------------------------------------------------------

; number?
;  `-- complex?
;       |-- %complex? ................................................... atomic
;       `-- real?
;            |-- floating-point?
;            |    |-- single-float? ..................................... atomic
;            |    `-- double-float? ..................................... atomic
;            `-- rational?
;                 |-- ratio? ............................................ atomic
;                 `-- integer?
;                      `-- exact-integer? ............................... atomic

(define floating-point?
  (lambda (z)
    (or (single-float? z)
        (double-float? z))))

(define real?
  (lambda (x)
    (or (floating-point? x)
        (rational? x))))

(define rational?
  (lambda (x)
    (or (ratio? x)
        (integer? x))))

(define irrational?
  (lambda (x) #f))

(define almost-exact-floating-point?
  (lambda (x)
    (and (floating-point? x)
         (= x (truncate x)))))

(define integer?
  (lambda (x)
    (or (exact-integer? x)
        (almost-exact-floating-point? x))))

;  .
;  |-- exact?
;  |    |-- exact-complex?
;  |    |-- exact-integer? .............................................. atomic
;  |    `-- ratio? ...................................................... atomic
;  `-- inexact?
;       |-- floating-point?
;       |    |--- single-float? ......................................... atomic
;       |    `--- single-float? ......................................... atomic
;       `-- inexact-complex?

(define exact?
  (lambda (z)
    (or (exact-integer? z)
        (ratio? z)
        (exact-complex? z))))

(define exact-complex?
  (lambda (x)
    (and (COMPLEX? x)
         (exact? (real-part x))
         (exact? (imag-part x)))))

(define inexact?
  (lambda (z)
    (or (floating-point? z)
        (inexact-complex? z))))

(define inexact-complex?
  (lambda (x)
    (and (COMPLEX? x)
         (inexact? (real-part x))
         (inexact? (imag-part x)))))

(define complex?
  (lambda (x)
    (or (exact? x)
        (inexact? x))))

(define number? complex?)

(define finite?
  (lambda (z)
    (not (infinite? z))))

(define infinite?
  (lambda (z)
    (or (= +inf.0 z)
        (= -inf.0 z))))

(define nan?
  (lambda (z)
    (if (COMPLEX? z)
        (or (ieee-nan? (real-part z))
            (ieee-nan? (imag-part z)))
        (ieee-nan? z))))

(define zero?     (lambda (n) (= n 0)))
(define positive? (lambda (n) (> n 0)))
(define negative? (lambda (n) (< n 0)))

(define odd?
  (lambda (n)
    (not (even? n))))

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(define max
  (lambda (x . xs)
    (define max-aux
      (lambda (x xs)
        (if (null? xs)
            (inexact x)
            (max-aux (if (< x (car xs)) (car xs) x)
                     (cdr xs)))))
    (if (inexact? x)
        (max-aux x xs)
        (let rec ((x x) (xs xs))
          (if (null? xs) x
              (if (inexact? (car xs))
                  (max-aux x xs)
                  (rec (if (< x (car xs)) (car xs) x)
                       (cdr xs))))))))

(define min
  (lambda (x . xs)
    (define min-aux
      (lambda (x xs)
        (if (null? xs)
            (inexact x)
            (min-aux (if (< (car xs) x) (car xs) x)
                     (cdr xs)))))
    (if (inexact? x)
        (min-aux x xs)
        (let rec ((x x) (xs xs))
          (if (null? xs) x
              (if (inexact? (car xs))
                  (min-aux x xs)
                  (rec (if (< (car xs) x) (car xs) x)
                       (cdr xs))))))))

(define abs
  (lambda (n)
    (if (< n 0) (- n) n)))

(define floor-quotient
  (lambda (x y)
    (floor (/ x y))))

(define floor-remainder
  (lambda (a b)
    (% (+ b (% a b)) b)))

(define floor/
  (lambda (x y)
    (values (floor-quotient x y)
            (floor-remainder x y))))

(define truncate-quotient
  (lambda (x y)
    (truncate (/ x y))))

(define truncate-remainder %)

(define truncate/
  (lambda (x y)
    (values (truncate-quotient x y)
            (truncate-remainder x y))))

(define quotient truncate-quotient)
(define remainder truncate-remainder)
(define modulo floor-remainder)

(define gcd ; from Chibi-Scheme lib/init7.scm
  (lambda xs
    (define gcd-2
      (lambda (a b)
        (if (zero? b)
            (abs a)
            (gcd b (remainder a b)))))
    (if (null? xs) 0
        (let rec ((n  (car xs))
                  (ns (cdr xs)))
          (if (null? ns) n
              (rec (gcd-2 n (car ns)) (cdr ns)) )))))

(define lcm ; from Chibi-Scheme lib/init7.scm
  (lambda xs
    (define lcm-2
      (lambda (a b)
        (abs (quotient (* a b) (gcd a b)))))
    (if (null? xs) 1
        (let rec ((n  (car xs))
                  (ns (cdr xs)))
          (if (null? ns) n
              (rec (lcm-2 n (car ns)) (cdr ns)))))))

(define numerator
  (lambda (x)
    (if (ratio? x)
        (car x)
        (if (exact? x) x
            (inexact (numerator (exact x)))))))

(define denominator
  (lambda (x)
    (if (exact? x)
        (if (ratio? x) (cdr x) 1)
        (if (integer? x) 1.0
            (inexact (denominator (exact x)))))))

(define rationalize ; from Chibi-Scheme lib/scheme/extras.scm (https://ml.cddddr.org/scheme/msg01498.html)
  (lambda (x e)

    (define sr
      (lambda (x y return)
        (let ((fx (floor x))
              (fy (floor y)))
          (cond ((>= fx x)
                 (return fx 1))
                ((= fx fy)
                 (sr (/ (- y fy))
                     (/ (- x fx))
                     (lambda (n d)
                       (return (+ d (* fx n)) n))))
                (else (return (+ fx 1) 1))))))

    (let ((return (if (negative? x)
                      (lambda (num den)
                        (/ (- num) den))
                      /))
          (x (abs x))
          (e (abs e)))
      (sr (- x e) (+ x e) return))))

(define log
  (lambda (z . base)
    (if (pair? base)
        (/ (ln z)
           (ln (car base)))
        (ln z))))

(define atan
  (lambda (y . o)
    (if (pair? o)
        (atan-2 y (car o))
        (atan-1 y))))

(define square
  (lambda (z)
    (* z z)))

; TODO exact-integer-sqrt

(define make-rectangular
  (lambda (x y)
    (+ x (* y (sqrt -1)))))

(define make-polar
  (lambda (radius phi)
    (make-rectangular (* radius (cos phi))
                      (* radius (sin phi)) )))

(define real-part
  (lambda (z)
    (if (COMPLEX? z) (car z) z)))

(define imag-part
  (lambda (z)
    (if (COMPLEX? z) (cdr z) 0)))

(define magnitude
  (lambda (z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)) ))))

(define angle
  (lambda (z)
    (atan (imag-part z)
          (real-part z) )))

(define inexact->exact exact)
(define exact->inexact inexact)

; ------------------------------------------------------------------------------
;  6.3 Standard Boolean Library (Part 2 of 2)
; ------------------------------------------------------------------------------

(define boolean?
  (lambda (x)
    (or (eqv? x #t)
        (eqv? x #f))))

(define boolean=?
  (lambda (x y . xs)
    (and (eqv? x y)
         (if (pair? xs)
             (apply boolean=? y xs)
             #t))))

; ------------------------------------------------------------------------------
;  6.5 Symbols
; ------------------------------------------------------------------------------

(define symbol=?
  (lambda (x y . xs)
    (and (eqv? x y)
         (if (pair? xs)
             (apply symbol=? y xs)
             #t))))

; ------------------------------------------------------------------------------
;  6.6 Characters
; ------------------------------------------------------------------------------

(define char-compare
  (lambda (x xs compare)
    (let rec ((compare compare)
              (lhs (char->integer x))
              (xs xs))
      (if (null? xs) #true
          (let ((rhs (char->integer (car xs))))
            (and (compare lhs rhs)
                 (rec compare rhs (cdr xs))))))))

(define (char=?  x . xs) (char-compare x xs =))
(define (char<?  x . xs) (char-compare x xs <))
(define (char>?  x . xs) (char-compare x xs >))
(define (char<=? x . xs) (char-compare x xs <=))
(define (char>=? x . xs) (char-compare x xs >=))

(define char-ci-compare
  (lambda (x xs compare)
    (let rec ((compare compare)
              (lhs (char->integer (char-downcase x)))
              (xs xs))
      (if (null? xs) #true
          (let ((rhs (char->integer (char-downcase (car xs)))))
            (and (compare lhs rhs)
                 (rec compare rhs (cdr xs))))))))

(define (char-ci=?  x . xs) (char-ci-compare x xs =))
(define (char-ci<?  x . xs) (char-ci-compare x xs <))
(define (char-ci>?  x . xs) (char-ci-compare x xs >))
(define (char-ci<=? x . xs) (char-ci-compare x xs <=))
(define (char-ci>=? x . xs) (char-ci-compare x xs >=))

(define char-alphabetic? ;                                         (scheme char)
  (lambda (x)
    (<= #,(char->integer #\a)
          (char->integer (char-downcase x))
        #,(char->integer #\z))))

(define char-numeric? ;                                            (scheme char)
  (lambda (x)
    (<= #,(char->integer #\0)
          (char->integer x)
        #,(char->integer #\9))))

(define char-whitespace? ;                                         (scheme char)
  (lambda (x)
    (or (eqv? x #\space)
        (eqv? x #\tab)
        (eqv? x #\newline)
        (eqv? x #\return))))

(define char-upper-case? ;                                         (scheme char)
  (lambda (x)
    (<= #,(char->integer #\A)
          (char->integer x)
        #,(char->integer #\Z))))

(define char-lower-case? ;                                         (scheme char)
  (lambda (x)
    (<= #,(char->integer #\a)
          (char->integer x)
        #,(char->integer #\z))))

(define char-downcase
  (lambda (c)
    (if (char-lower-case? c) c
        (integer->char (+ (char->integer c) 32)))))

(define char-upcase
  (lambda (c)
    (if (char-upper-case? c) c
        (integer->char (- (char->integer c) 32)))))

(define char-foldcase char-downcase)

; ------------------------------------------------------------------------------
;  6.7 Strings
; ------------------------------------------------------------------------------

(define (make-string k . c)
  (let ((c (if (pair? c) (car c) #\space)))
    (let rec ((k k)
              (result '()))
      (if (<= k 0) result
          (rec (- k 1)
               (char-cons c result))))))

(define (string . xs) (list->string xs))

(define (string-length s)
  (let rec ((s s)
            (k 0))
    (if (null? s) k
        (rec (cdr s)
             (+ k 1)))))

(define string-ref list-ref)

(define string-set! list-set!)

(define lexicographical-compare
  (lambda (x xs . compare)

    (define distance
      (lambda (x y)
        (if (or (null? x)
                (null? y))
            (- (string-length y)
               (string-length x))
            (let ((d (- (char->integer (car y))
                     (char->integer (car x)))))
              (if (zero? d)
                  (distance (cdr x)
                            (cdr y))
                  d)))))

    (let ((compare (if (pair? compare) (car compare) =)))
      (if (null? xs) #t
          (and (compare 0 (distance x (car xs)))
               (lexicographical-compare (car xs)
                                        (cdr xs)
                                        compare))))))

(define string=?
  (lambda (x . xs)
    (lexicographical-compare x xs =)))

(define string<?
  (lambda (x . xs)
    (lexicographical-compare x xs <)))

(define string>?
  (lambda (x . xs)
    (lexicographical-compare x xs >)))

(define string<=?
  (lambda (x . xs)
    (lexicographical-compare x xs <=)))

(define string>=?
  (lambda (x . xs)
    (lexicographical-compare x xs >=)))

(define lexicographical-ci-compare
  (lambda (x xs . compare)

    (define distance
      (lambda (x y)
        (if (or (null? x)
                (null? y))
            (- (string-length y)
               (string-length x))
            (let ((d (- (char->integer (char-downcase (car y)))
                        (char->integer (char-downcase (car x))))))
              (if (zero? d)
                  (distance (cdr x)
                            (cdr y))
                  d)))))

    (let ((compare (if (pair? compare) (car compare) =)))
      (if (null? xs) #t
          (and (compare 0 (distance x (car xs)))
               (lexicographical-ci-compare (car xs)
                                           (cdr xs)
                                           compare))))))

(define string-ci=?
  (lambda (x . xs)
    (lexicographical-ci-compare x xs =)))

(define string-ci<?
  (lambda (x . xs)
    (lexicographical-ci-compare x xs <)))

(define string-ci>?
  (lambda (x . xs)
    (lexicographical-ci-compare x xs >)))

(define string-ci<=?
  (lambda (x . xs)
    (lexicographical-ci-compare x xs <=)))

(define string-ci>=?
  (lambda (x . xs)
    (lexicographical-ci-compare x xs >=)))

(define string-upcase ; Toy implementaion
  (lambda (s)
    (string-map char-upcase s)))

(define string-downcase ; Toy implementaion
  (lambda (s)
    (string-map char-downcase s)))

(define string-foldcase
  (lambda (s)
    (string-map char-foldcase s)))

(define string-take
  (lambda (x k)
    (let rec ((x x)
              (k k))
      (if (zero? k) '()
          (char-cons (car x)
                     (rec (cdr x) (- k 1)))))))

(define string-drop drop)

(define string-copy
  (lambda (s . o)

    (define start
      (if (and (pair? o)
               (exact-integer? (car o)))
          (car o)
          0))

    (define end
      (if (and (pair? o)
               (pair? (cdr o))
               (exact-integer? (cadr o)))
          (cadr o)
          (string-length s)))

    (string-take (string-drop s start) (- end start))))

(define substring string-copy)

(define string-append-2
  (lambda (x y)
    (if (null? x) y
        (char-cons (car x)
                   (string-append-2 (cdr x) y) ))))

(define string-reverse
  (lambda (x)
    (if (null? x) ""
        (string-append-2 (string-reverse (cdr x))
                         (string (car x))))))

(define string-append
  (lambda x
    (define string-append-aux
      (lambda (x y)
        (if (null? x) y
            (string-append-aux (cdr x)
                               (string-append-2 (car x) y) ))))
    (if (null? x) ""
        (let ((rx (string-reverse x)))
          (string-append-aux (cdr rx)
                             (car rx))))))

(define string->list
  (lambda (s . o)

    (define start
      (if (and (pair? o)
               (exact-integer? (car o)))
          (car o)
          0))

    (define end
      (if (and (pair? o)
               (pair? (cdr o))
               (exact-integer? (cadr o)))
          (cadr o)
          (string-length s)))

    (take (drop s start) (- end start))))

(define (list->string x)
  (let rec ((x x))
    (cond ((null? x) '())
          ((pair? x)
           (char-cons (car x)
                      (rec (cdr x))))
          (else (char-cons x '())))))

; string-copy!

(define string-fill!
  (lambda (s c . o)
    (let ((start (if (and (pair? o)
                          (exact-integer? (car o)))
                     (car o)
                     0))
          (end (if (and (pair? o)
                        (pair? (cdr o))
                        (exact-integer? (cadr o)))
                   (cadr o)
                   (string-length s))))
      (let rec ((k (- end 1)))
        (if (<= start k)
            (begin (string-set! s k c)
                   (rec (- k 1))))))))

; ------------------------------------------------------------------------------
;  6.9 Standard Bytevectors Library
; ------------------------------------------------------------------------------

(define bytevector?
  (lambda (x) #false))

; ------------------------------------------------------------------------------
;  6.10 Control features (Part 2 of 2)
; ------------------------------------------------------------------------------

(define procedure?
  (lambda (x)
    (or (native-procedure? x)
        (closure? x)
        (continuation? x) )))

(define string-map
  (lambda (f x . xs)

    (define map-1
      (lambda (f x result)
        (if (string? x)
            (map-1 f
                   (cdr x)
                   (char-cons (f (car x))
                              result))
            (string-reverse result))))

    (define map-n
      (lambda (f xs result)
        (if (every string? xs)
            (map-n f
                   (map-1 cdr xs '())
                   (char-cons (apply f (map-1 car xs '()))
                              result))
            (string-reverse result))))

    (if (null? xs)
        (map-1 f x '())
        (map-n f (char-cons x xs) '()))))

; TODO vector-map

; TODO string-for-each
; TODO vector-for-each

; (define values
;   (lambda xs
;     (call-with-current-continuation
;       (lambda (continuation)
;         (apply continuation xs)))))

; Magic Token Trick
; https://stackoverflow.com/questions/16674214/how-to-implement-call-with-values-to-match-the-values-example-in-r5rs
(define values-magic-token (list 'values))

(define values-magic-token?
  (lambda (x)
    (and (pair? x)
         (eq? (car x) values-magic-token) )))

(define values
  (lambda xs
    (if (and (not (null? xs))
             (null? (cdr xs)) )
        (car xs)
        (cons values-magic-token xs) )))

(define call-with-values
  (lambda (producer consumer)
    (let ((result (producer)))
      (if (values-magic-token? result)
          (apply consumer (cdr result))
          (consumer result) ))))

; ---- dynamic-wind ------------------------------------------------------------

; from SRFI-39

(define dynamic-extents '())

(define dynamic-wind
  (lambda (before body after)
    (before)
    (set! dynamic-extents (cons (cons before after) dynamic-extents))
    (let ((result (body)))
      (set! dynamic-extents (cdr dynamic-extents))
      (after)
      result)))

(define call-with-current-continuation
  (let ((call/cc
          (lambda (procedure)
            (call-with-current-continuation procedure)))) ; Original call/cc is syntax
    (lambda (proc)

      (define windup!
        (lambda (from to)
          (set! dynamic-extents from)
          (cond ((eq? from to))
                ((null? from)
                 (windup! from (cdr to))
                 ((caar to)))
                ((null? to)
                 ((cdar from))
                 (windup! (cdr from) to))
                (else ((cdar from))
                      (windup! (cdr from) (cdr to))
                      ((caar to))))
          (set! dynamic-extents to)))

      (let ((winds dynamic-extents))
        (call/cc
          (lambda (k1)
            (proc (lambda (k2)
                    (windup! dynamic-extents winds)
                    (k1 k2)))))))))

(define call/cc call-with-current-continuation)

; ------------------------------------------------------------------------------
;  6.11 Standard Exceptions Library
; ------------------------------------------------------------------------------

; (define-syntax receive
;   (syntax-rules ()
;     ((receive parameters expression . body)
;      (call-with-values
;        (lambda () expression)
;        (lambda parameters . body)))))

(define-syntax receive ; (receive parameters expression . body)
  (er-macro-transformer
    (lambda (form rename compare)
      `(call-with-values
         (,(rename 'lambda) () ,(caddr form))
         (,(rename 'lambda) ,(cadr form) ,@(cdddr form))))))

; TODO with-exception-handler
; TODO raise ; SRFI-18
; TODO raise-continuable

(define error ; SRFI-23
  (lambda (message . irritants)
    (display "error: ")
    (display message)
    (for-each (lambda (each)
                (display " ")
                (write each))
              irritants)
    (newline)
    (exit 1)))

(define error-object?
  (lambda (x) #false) )

; TODO error-object-message
; TODO error-object-irritants
; TODO read-error?
; TODO file-error?

; ------------------------------------------------------------------------------
;  6.12 Standard Environments and Evaluation Library
; ------------------------------------------------------------------------------

; TODO scheme-report-environment
; TODO null-environment

(define current-lexical-environment ; deprecated
  (lambda ()
    (cdar (fork/csc
            (lambda ()
             '())))))

(define interaction-environment ; deprecated
  (lambda ()
    (cdr (fork/csc
           (lambda ()
            '())))))

; ------------------------------------------------------------------------------
;  6.13 Standard Input and Output Library
; ------------------------------------------------------------------------------

(define input-standard-port?
  (lambda (x)
    (eq? x (input-standard-port))))

(define output-standard-port?
  (lambda (x)
    (eq? x (output-standard-port))))

(define error-standard-port?
  (lambda (x)
    (eq? x (error-standard-port))))


(define call-with-port
  (lambda (port procedure)
    (procedure port)))

(define call-with-input-file
  (lambda (path procedure)
    (call-with-port (open-input-file path) procedure)))

(define call-with-output-file
  (lambda (path procedure)
    (call-with-port (open-output-file path) procedure)))


(define input-port?
  (lambda (x)
    (or (input-file-port? x)
        (input-string-port? x)
        (input-standard-port? x))))

(define output-port?
  (lambda (x)
    (or (output-file-port? x)
        (output-string-port? x)
        (output-standard-port? x))))

(define textual-port?
  (lambda (x)
    (or (input-file-port? x)
        (input-string-port? x)
        (input-standard-port? x)
        (output-file-port? x)
        (output-string-port? x)
        (output-standard-port? x)
        (error-standard-port? x))))

(define binary-port?
  (lambda (x) #f))

(define port?
  (lambda (x)
    (or (input-port? x)
        (output-port? x))))


(define input-port-open?
  (lambda (x)
    (cond ((input-file-port? x)
           (input-file-port-open? x))
          ((input-string-port? x) #t)
          ((input-standard-port? x) #t)
          (else #f))))

(define output-port-open?
  (lambda (x)
    (cond ((output-file-port? x)
           (output-file-port-open? x))
          ((output-string-port? x) #t)
          ((output-standard-port? x) #t)
          ((error-standard-port? x) #t)
          (else #f))))


(define current-input-port
  (make-parameter (input-standard-port)
    (lambda (x)
      (cond ((not (input-port? x))
             (error "current-input-port: not input-port" x))
            ((not (input-port-open? x))
             (error "current-input-port: not input-port-open" x))
            (else x)))))

(define current-output-port
  (make-parameter (output-standard-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-output-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-output-port: not output-port-open" x))
            (else x)))))

(define current-error-port
  (make-parameter (error-standard-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-error-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-error-port: not output-port-open" x))
            (else x)))))


(define with-input-from-file
  (lambda (string thunk)
    (parameterize ((current-input-port (open-input-file string)))
      (thunk))))

(define with-output-to-file
  (lambda (string thunk)
    (parameterize ((current-output-port (open-output-file string)))
      (thunk))))


(define close-port
  (lambda (x)
    (cond (( input-port? x) ( close-input-port x))
          ((output-port? x) (close-output-port x))
          (else (unspecified)))))

(define close-input-port
  (lambda (x)
    (cond ((input-file-port? x)
           (close-input-file-port x))
          (else (unspecified)))))

(define close-output-port
  (lambda (x)
    (cond ((output-file-port? x)
           (close-output-file-port x))
          (else (unspecified)))))


; TODO open-input-bytevector
; TODO open-output-bytevector
; TODO get-output-bytevector


(define read        (lambda x (::read        (if (pair? x) (car x) (current-input-port)))))
(define read-char   (lambda x (::read-char   (if (pair? x) (car x) (current-input-port)))))
(define peek-char   (lambda x (::peek-char   (if (pair? x) (car x) (current-input-port)))))
(define char-ready? (lambda x (::char-ready? (if (pair? x) (car x) (current-input-port)))))

(define write-simple
  (lambda (datum . maybe-port)
    (let ((port (if (pair? maybe-port)
                    (car maybe-port)
                    (current-output-port))))
      (::write-simple datum port))))

(define write write-simple)

(define display
  (lambda (datum . maybe-port)
    (cond ((char?   datum) (apply write-char    datum maybe-port))
          ((string? datum) (apply write-string  datum maybe-port))
          (else            (apply write         datum maybe-port)))))

(define newline
  (lambda xs
    (apply write-char #\newline xs)))

(define write-char
  (lambda (char . maybe-port)
    (::write-char char (if (pair? maybe-port)
                           (car maybe-port)
                           (current-output-port)))))

(define write-string
  (lambda (string . xs)
    (case (length xs)
      ((0)  (::write-string string (current-output-port)))
      ((1)  (::write-string string (car xs)))
      (else (::write-string (apply string-copy string (cadr xs)) (car xs))))))

; TODO write-u8
; TODO write-bytevector

(define flush-output-port
  (lambda maybe-port
    (::flush-output-port (if (pair? maybe-port)
                             (car maybe-port)
                             (current-output-port)))))


; ------------------------------------------------------------------------------
;  6.14 Standard System Interface Library
; ------------------------------------------------------------------------------

; TODO file-exists?
; TODO delete-file
; TODO command-line

(define exit emergency-exit) ;                          (scheme process-context)

; TODO get-environment-variable
; TODO get-environment-variables

; TODO current-second
; TODO current-jiffy
; TODO jiffies-per-second

; ------------------------------------------------------------------------------
;  Miscellaneous
; ------------------------------------------------------------------------------

(define swap!
  (fork/csc
    (lambda (swap! x y)
      (let ((temporary (string->symbol)))
       `(,let ((,temporary ,x))
          (,set! ,x ,y)
          (,set! ,y ,temporary)) ))))

(define swap!
  (fork/csc
    (lambda (swap! x y)
     `(,let ((,value ,x))
        (,set! ,x ,y)
        (,set! ,y ,value)) )))

(define-syntax (swap! x y)
 `(,let ((,value ,x))
    (,set! ,x ,y)
    (,set! ,y ,value)))

; (define swap!
;   (er-macro-transformer
;     (lambda (expression rename compare)
;       (let ((a (cadr expression))
;             (b (caddr expression)))
;        `(,(rename 'let) ((,(rename 'value) ,a))
;           (,(rename 'set!) ,a ,b)
;           (,(rename 'set!) ,b ,(rename 'value))) ))))

(define loop
  (fork/csc
    (lambda form
     `(,call/cc
        (,lambda (exit)
          (,let ,rec ()
           ,(cadr form)
            (,rec)))) )))

(define f
  (lambda ()
    (define x 0)

    (define let     3.14)
    (define call/cc 3.141)
    ; (define lambda  3.1415) ; IMPLEMENTATION MISS
    (define exit    3.14159)
    (define rec     3.141592)

    (loop
      (if (< 9 x)
          (begin (display "!")
                 (display exit)
                 (exit 42))
          (begin (display x)
                 (set! x (+ x 1)) )))))

; (define-syntax loop
;   (non-hygienic-macro-transformer
;     (lambda (form)
;      `(call-with-current-continuation
;         (lambda (exit)
;           (let loop ()
;             ,form
;             (loop)))))))

; (define-syntax loop
;   (sc-macro-transformer
;     (lambda (form environment)
;      `(call-with-current-continuation
;         (lambda (exit)
;           (let loop ()
;            ,(make-syntactic-closure environment '(exit) (cadr form))
;             (loop)))))))

; ------------------------------------------------------------------------------
;  Library
; ------------------------------------------------------------------------------

(define define-something
  (fork/csc
    (lambda (_ name value)
     `(,define ,name ,value))))

(define define-library
  (fork/csc
    (lambda (define-library name . declarations)
     `(,define ,name
        (,fork/csc
          (,lambda (,this . ,expression)
            ; (,begin (,define ,name ,this))
            ,@declarations
            ; (,if (,null? ,expression) ,this
            ;      (,begin
            ;        (,display "; library\t; received expression " ,expression "\n")
            ;        (,display ";\t\t; evaluate " (,car ,expression) " (a.k.a rename)\n")
            ;        (evaluate (,car ,expression))
            ;        )
            ;      )
            )))
     )))

; (define export
;   (fork/csc
;     (lambda (_ . export-specs)
;      `(,display "; dummy-export\t; " ',export-specs "\n"))))

; (define export
;   (fork/csc
;     (lambda (this . export-specs)
;      `(stage ,@(map (lambda (each)
;                       (list quote each))
;                     export-specs)))))

; (define import
;   (fork/csc
;     (lambda (import . import-set)
;      `(,display "; dummy-import\t; " ',import-set "\n"))))

; (define instantiate-library
;   (fork/csc
;     (lambda (this library-name)
;      `(,let ((,object (,reference ,library-name)))
;         (,object)))))

; TODO REMOVE
; (define evaluate-in
;   (fork/csc
;     (lambda (this namespace identifier)
;      `((,reference ,namespace) ',identifier))))

(define-library (example empty) '())

(define-library (scheme base)
  (export x)
  (begin
    (define x 42)
    (display "instantiating dummy-library (scheme base)!\n")))

(define-library (example hello)
  (export hello
          goodbye)

  (begin
    (define hello
      (lambda ()
        (begin (display "hello, world!")
               (newline) )))

    (define goodbye
      (lambda ()
        (begin (display "goodbye, world!")
               (newline) )))

    (define greet-to
      (lambda (name)
        (begin (display "hello, " name)
               (newline) ))))

  (begin
    (define one   1)
    (define two   2)
    (define three 3) ))

; XXX this cause compile error (bug)
; (lambda ()
;   (begin
;     (define x 1)
;     (define y 2))
;   (+ x y)
;   )

; (define-library (example grid)
;   (export make
;           rows
;           columns
;           reference
;           each
;           (rename put! set!))
;
;   (import (scheme base))
;
;   (begin
;
;     (define make
;       (lambda (n m)
;         (let ((grid (make-vector n)))
;           (do ((i 0 (+ i 1)))
;               ((= i n) grid)
;             (let ((v (make-vector m #false)))
;               (vector-set! grid i v))))))
;
;     (define rows
;       (lambda (grid)
;         (vector-length grid)))
;
;     (define columns
;       (lambda (grid)
;         (vector-length (vector-ref grid 0))))
;
;     (define reference
;       (lambda (grid n m)
;         (and (< -1 n (rows grid))
;              (< -1 m (columns grid))
;              (vector-ref (vector-ref grid n) m))))
;
;     (define put!
;       (lambda (grid n m v)
;         (vector-set!  (vector-ref grid n) m v)))
;
;     (define each
;       (lambda (grid procedure)
;         (do ((j 0 (+ j 1)))
;             ((= j (rows grid)))
;           (do ((k 0 (+ k 0)))
;               ((= k (columns grid)))
;             (procedure j k (reference grid j k))))))))
;
; (define-library (example life)
;   (export life)
;   (import (except (scheme base) set!)
;           (scheme write)
;           (example grid))
;
;   (begin
;
;     (define life-count
;       (lambda (grid i j)
;
;         (define (count i j)
;           (if (reference grid i j) 1 0))
;
;         (+ (count (- i 1) (- j 1))
;            (count (- i 1)    j   )
;            (count (- i 1) (+ j 1))
;            (count    i    (- j 1))
;            (count    i    (+ j 1))
;            (count (+ i 1) (- j 1))
;            (count (+ i 1)    j   )
;            (count (+ i 1) (+ j 1)))))
;
;     (define life-alive?
;       (lambda (grid i j)
;         (case (life-count grid i j)
;           ((3) #true)
;           ((2) (reference grid i j))
;           (else #false))))
;
;     (define life-print
;       (lambda (grid)
;         (display "\x1B;[1H\x1B;[J"); clear vt100
;         (each grid
;           (lambda (i j v)
;             (display (if v "*" " "))
;             (when (= j (- (columns grid) 1))
;               (newline))))))
;
;     (define life
;       (lambda (grid iterations)
;         (do ((i 0 (+ i 1))
;              (grid0 grid grid1)
;              (grid1 (make (rows grid)
;                           (columns grid))
;                     grid0))
;             ((= i iterations))
;           (each grid0
;             (lambda (j k v)
;               (let ((a (life-alive? grid0 j k)))
;                 (set! grid1 j k a))))
;           (life-print grid1))))))

(define-library (example value)
  (import (scheme base))
  (export increment
          reference-value)
  (begin
    (define value 0)
    (define increment
      (lambda ()
        (set! value (+ value 1))))
    (define reference-value
      (lambda () value))))

; (define reference-value
;   (lambda operands
;     (let ((evaluate (reference (example value))))
;       (evaluate `(reference-value ,@operands)))))
;
; (define increment
;   (lambda operands
;     (let ((evaluate (reference (example value))))
;       (evaluate `(increment ,@operands)))))

(define from
  (fork/csc
    (lambda (this library-name expression)
     `(,apply (,reference ,library-name) ,expression) )))

(define reference-value
  (lambda xs
    (from (example value)
         `(reference-value ,xs) )))

(define increment
  (lambda xs
    (from (example value)
         `(increment ,xs) )))

(define Module
  (fork/csc
    (lambda (this)
      (begin
        (define x 1)
        (define y 2)
        (define div
          (lambda ()
            (/ x y) ))
        (define sum
          (lambda ()
            (+ x y) ))))))

(define factory ; letrec
  (fork/csc
    (lambda (this)
      (letrec ((value 0)
               (increment
                 (lambda ()
                   (set! value (+ value 1)) ))
               (get
                 (lambda () value)))
       `(begin (define increment ,increment)
               (define get ,get)) ))))

(define factory ; internal-define
  (fork/csc
    (lambda (this)
      (define value 0)
      (define increment
        (lambda ()
          (set! value (+ value 1)) ))
      (define get
        (lambda () value))

     `(begin (define increment ,increment)
             (define get ,get)) )))

(define factory
  (fork/csc
    (lambda (this)

      (begin (define value 0)

             (define increment
               (lambda ()
                 (set! value (+ value 1)) ))

             (define get
               (lambda () value))

             ; (define even?
             ;   (lambda ()
             ;     (if (zero? value) #true
             ;         (odd? (- value 1)) )))
             ;
             ; (define odd?
             ;   (lambda ()
             ;     (if (zero? value) #false
             ;         (even? (- value 1)) )))
             )

     `(,begin (define increment ,increment)
              (define get ,get)
              ; (define even? ,even?)
              )
     )))

(define let-syntax
  (fork/csc
    (lambda (let-syntax bindings . body)
     `((fork/csc
         (,lambda (,this ,@(map car bindings))
            ,@body
            )
         )
       ,@(map cadr bindings)) )))

; (let-syntax ((given-that (fork/csc
;                            (lambda (this test . statements)
;                             `(,if test
;                                  (,begin ,statements))))))
;   (let ((if #true))
;     (given-that if (set! if 'now))
;     if))

; ((fork/csc
;    (lambda (this given-that)
;      (let ((if #true))
;        (given-that if (set! if 'now))
;        if)
;      )
;    )
;  (fork/csc
;    (lambda (this test . statements)
;     `(,if test
;        (,begin ,statements)))))

; (let ((x 'outer))
;   (let-syntax ((m (fork/csc
;                     (lambda (m) x))))
;     (let ((x 'inner))
;       (m))))

(let ((x 'outer))
  (fork/csc
    (lambda (this)
      (begin (define m (fork/csc
                         (lambda (this) x)) ))
      (let ((x 'inner))
        (m) ))))

; (define letrec* ; transform to internal-definitions
;   (fork/csc
;     (lambda (letrec* bindings . body)
;       ((lambda (definitions)
;         `((,lambda () ,@definitions ,@body)))
;        (map (lambda (x) (cons 'define x)) bindings)))))

; (define letrec-syntax
;   (fork/csc
;     (lambda (letrec-syntax bindings . body)
;       ((lambda (definitions)
;         `((,lambda ()
;             ,@definitions
;             ,@body)))
;        (map (lambda (x) (cons define x)) bindings)))))
;
;
; ; (define letrec-syntax let-syntax)
;
; (letrec-syntax ((or (fork/csc
;                       (lambda (or . tests)
;                         (cond
;                           ((null? tests) #false)
;                           ((null? (cdr tests)) (car tests))
;                           (#true ; else
;                             (list (list lambda (list result)
;                                         (list if result
;                                               result
;                                               (cons or (cdr tests))))
;                                   (car tests))))))))
;   (let ((x #false)
;         (y 7)
;         (temp 8)
;         (let odd?)
;         (if even?))
;     (or x
;         (let temp)
;         (if y)
;         y)))

(define scheme
  (fork/csc
    (lambda (this . submodule)

      (begin (define identity
               (lambda (x) x) )

             (define unspecified
               (lambda ()
                 (if #false #false) ))
             ) ; begin

      (begin (define println
               (fork/csc
                 (lambda (this . xs)
                  `(,display ,@xs "\n"))))
        )

      (begin (define base
               (fork/csc
                 (lambda (this)

                   (begin (define null-environment
                            (lambda () this) )

                          (define eq?
                            (procedure equivalence.so "equals") )

                          (define eqv?
                            (procedure equivalence.so "equivalent") )

                          ; (define hello
                          ;   (lambda ()
                          ;     (display "hello, world!\n") ))
                          (define hello
                            (lambda ()
                              (println "hello, world!")))
                          )

                  `(,begin (,define hello, hello))

                   ))) ; base
             ) ; begin

      (cond
        ((null? submodule) this)
        ((null? (car submodule)) this)
        ((eq? (car submodule) 'base) `(,base))
        (else (error))
        )
     ))) ; scheme

; (define let-syntax
;   (fork/csc
;     (lambda (let-syntax bindings . body)
;
;       (let ((definitions (map (lambda (x) (cons define x)) bindings)))
;        `(,fork/csc
;           (,lambda (this)
;             (,begin ,definitions)
;             ,@body
;             ))
;         )
;       )))
;
; (let-syntax ((given-that (fork/csc
;                            (lambda (this test . statements)
;                             `(,if test
;                                  (,begin ,statements))))))
;   (let ((if #true))
;     (given-that if (set! if 'now))
;     if))
;
; (let ((x 'outer))
;   (let-syntax ((m (fork/csc
;                     (lambda (m) x))))
;     (let ((x 'inner))
;       (m))))

(define-syntax (increment x . n)
  (let ((n (if (pair? n) (car n) 1)))
    `(,begin (,set! ,x (,+ ,x ,n)) ,x)))
