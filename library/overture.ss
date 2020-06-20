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
;  6.1 Equivalence predicates (Part 1 of 2)
; ------------------------------------------------------------------------------

(define equivalence.so
  (linker "libmeevax-equivalence.so"))

(define eq?
  (procedure equivalence.so "equals"))

(define eqv?
  (procedure equivalence.so "equivalent"))

; ------------------------------------------------------------------------------
;  6.2 Numbers (Part 1 of 2)
; ------------------------------------------------------------------------------

(define numerical.so
  (linker "libmeevax-numerical.so"))

(define = eqv?)

(define <
  (procedure numerical.so "less"))

(define <=
  (procedure numerical.so "less_equal"))

(define >
  (procedure numerical.so "greater"))

(define >=
  (procedure numerical.so "greater_equal"))

(define *
  (procedure numerical.so "multiplication"))

(define +
  (procedure numerical.so "addition"))

(define -
  (procedure numerical.so "subtraction"))

(define /
  (procedure numerical.so "division"))

; ------------------------------------------------------------------------------
;  6.3 Booleans (Part 1 of 2)
; ------------------------------------------------------------------------------

(define not
  (lambda (x)
    (if x #f #t)))

; ------------------------------------------------------------------------------
;  6.4 Pairs and Lists (Part 1 of 2)
; ------------------------------------------------------------------------------

(define pair.so
  (linker "libmeevax-pair.so"))

(define pair?
  (procedure pair.so "pair_"))

(define cons ; pair
  (procedure pair.so "cons"))

; (define cons ; hack
;   (lambda (x y)
;     (cons x y)))

(define car (procedure pair.so "car"))
(define cdr (procedure pair.so "cdr"))

; TODO set-car!
; TODO set-cdr!

(define caar (lambda (x) (car (car x) )))
(define cadr (lambda (x) (car (cdr x) )))
(define cdar (lambda (x) (cdr (car x) )))
(define cddr (lambda (x) (cdr (cdr x) )))

(define caaar (lambda (x) (car (car (car x) ))))
(define caadr (lambda (x) (car (car (cdr x) ))))
(define cadar (lambda (x) (car (cdr (car x) ))))
(define caddr (lambda (x) (car (cdr (cdr x) ))))
(define cdaar (lambda (x) (cdr (car (car x) ))))
(define cdadr (lambda (x) (cdr (car (cdr x) ))))
(define cddar (lambda (x) (cdr (cdr (car x) ))))
(define cdddr (lambda (x) (cdr (cdr (cdr x) ))))

(define caaaar (lambda (x) (car (car (car (car x) )))))
(define caaadr (lambda (x) (car (car (car (cdr x) )))))
(define caadar (lambda (x) (car (car (cdr (car x) )))))
(define caaddr (lambda (x) (car (car (cdr (cdr x) )))))
(define cadaar (lambda (x) (car (cdr (car (car x) )))))
(define cadadr (lambda (x) (car (cdr (car (cdr x) )))))
(define caddar (lambda (x) (car (cdr (cdr (car x) )))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x) )))))
(define cdaaar (lambda (x) (cdr (car (car (car x) )))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x) )))))
(define cdadar (lambda (x) (cdr (car (cdr (car x) )))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x) )))))
(define cddaar (lambda (x) (cdr (cdr (car (car x) )))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x) )))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x) )))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x) )))))

(define null?
  (lambda (x)
    (eqv? x '()) ))

(define list
  (lambda x x))

(define append-2
  (lambda (x y)
    (if (null? x) y
        (cons (car x)
              (append-2 (cdr x) y) ))))

(define reverse ; simple but slow
  (lambda (x)
    (if (null? x) '()
        (append-2 (reverse (cdr x))
                  (list (car x)) ))))

(define append
  (lambda x
    (define append-aux
      (lambda (x y)
        (if (null? x) y
            (append-aux (cdr x)
                        (append-2 (car x) y) ))))
    (if (null? x) '()
        ((lambda (reversed)
           (append-aux (cdr reversed)
                       (car reversed) ))
         (reverse x) ))))

; ==== Low-Level Macro Facility ================================================

(define define-syntax
  (fork/csc
    (lambda (define-syntax identifier . transformer)
      (if (pair? identifier)
          (list define (car identifier)
            (list fork/csc
              (list lambda identifier . transformer)))
          (list define identifier . transformer)))))

(define identifier?
  (lambda (x)
    (if (null? x) #false
        (syntactic-closure? x))))

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

(define er-macro-transformer
  (lambda (transform)
    (fork/csc
      (lambda form
        (transform form (lambda (x) (eval x (car form))) free-identifier=?)))))

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

(define-syntax (and . tests)
  (cond ((null? tests))
        ((null? (cdr tests)) (car tests))
        (else (list if (car tests)
                    (cons and (cdr tests))
                    #f))))

(define-syntax (or . tests)
  (cond
    ((null? tests) #false)
    ((null? (cdr tests)) (car tests))
    (else (list (list lambda (list result)
                  (list if result
                           result
                           (cons or (cdr tests)) ))
                (car tests) ))))

; --------------------------------------------------------------------------
;  4.2.8 Quasiquotations
; --------------------------------------------------------------------------

; (define unquote          identity)
; (define unquote-splicing identity)

(define-syntax (quasiquote x)

  (define quasiquote-expand
    (lambda (e depth)
      (if (not (pair? e))
          (list 'quote e)
          (if (eqv? (car e) 'quasiquote)
              (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1)))
              (if (eqv? (car e) 'unquote)
                  (if (< 0 depth)
                      (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1)))
                      (if (and (not (null? (cdr e))) (null? (cddr e)))
                          (cadr e)
                          (error "illegal unquote")))
                  (if (eqv? (car e) 'unquote-splicing)
                      (if (< 0 depth)
                          (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1)))
                          (error "illegal unquote-splicing"))
                      (list 'append (quasiquote-expand-list (car e) depth)
                                    (quasiquote-expand      (cdr e) depth) )))))))

  (define quasiquote-expand-list
    (lambda (e depth)
      (if (not (pair? e))
          (list 'quote (list e))
          (if (eqv? (car e) 'quasiquote)
              (list 'list (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1)) ))
              (if (eqv? (car e) 'unquote)
                  (if (< 0 depth)
                      (list 'list (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1)) ))
                      (cons 'list (cdr e)))
                  (if (eqv? (car e) 'unquote-splicing)
                      (if (< 0 depth)
                          (list 'list (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1)) ))
                          (cons 'append (cdr e)))
                      (list 'list (list 'append (quasiquote-expand-list (car e) depth)
                                                (quasiquote-expand      (cdr e) depth) ))))))))

  (quasiquote-expand x 0) )

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

(define-syntax (letrec bindings . body)
  ((lambda (definitions)
    `((,lambda () ,@definitions ,@body)) )
   (map (lambda (x) (cons 'define x)) bindings) ))

(define letrec* letrec)

(define-syntax (let bindings . body)
  (if (pair? bindings)
      `((,lambda ,(map car bindings) ,@body)
        ,@(map cadr bindings))
      `(,letrec ((,bindings
                   (,lambda ,(map car (car body)) ,@(cdr body))))
         (,bindings ,@(map cadr (car body))))))

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
;  6.4 Pairs and Lists (Part 2 of 2)
; ------------------------------------------------------------------------------

(define proper-list?
  (lambda (x)
    (let rec ((x x)
              (y x))
      (if (pair? x)
          (let ((x (cdr x)))
            (if (pair? x)
                (let ((x (cdr x))
                      (y (cdr y)))
                  (and (not (eq? x y))
                       (rec x y) ))
                (null? x) ))
          (null? x) ))))

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
                       (rec x y) ))
                (not (null? x)) ))
          (not (null? x)) ))))

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
                        (rec x y) ))))))))

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

(define length
  (lambda (x)
    (let rec ((x x)
              (lag x)
              (result 0))
      (if (pair? x)
          (let ((x (cdr x))
                (result (+ result 1)))
            (if (pair? x)
                (let ((x (cdr x))
                      (lag (cdr lag))
                      (result (+ result 1)))
                  (and (not (eq? x lag))
                       (rec x lag result) ))
                result))
          result ))))

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

(define list-tail
  (lambda (x k)
    (if (zero? k) x
        (list-tail (cdr x) (- k 1)) )))

(define list-ref
  (lambda (x k)
    (car (list-tail x k)) ))

; TODO list-set!

(define member
  (lambda (o x . c)
    (let ((compare (if (pair? c) (car c) equal?)))
      (let rec ((x x))
        (and (pair? x)
             (if (compare o (car x)) x
                 (rec (cdr x)) ))))))

(define memq
  (lambda (o x)
    (member o x eq?) ))

(define memv
  (lambda (o x)
    (member o x eqv?) ))

(define assoc
  (lambda (o x . c)
    (let ((compare (if (pair? c) (car c) equal?)))
      (let assoc ((x x))
        (if (null? x) #false
            (if (compare o (caar x))
                (car x)
                (assoc (cdr x)) ))))))

(define assq
  (lambda (o x)
    (assoc o x eq?) ))

(define assv
  (lambda (o x)
    (assoc o x eqv?) ))

(define list-copy
  (lambda (x)
    (let rec ((x x)
              (result '()))
      (if (pair? x)
          (rec (cdr x)
               (cons (car x) result))
          (append (reverse result) x) ))))

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
;  4.2.5 Standard Delayed Evaluation Library (Part 1 of 2)
; ------------------------------------------------------------------------------

(define-syntax (delay-force expression)
 `(,promise #f (,lambda () ,expression)))

(define-syntax (delay expression)
 `(,delay-force (,promise #t expression)))

; TODO promise?
; TODO make-promise?

; ------------------------------------------------------------------------------
;  6.1 Standard Equivalence Predicates Library (Part 2 of 2)
; ------------------------------------------------------------------------------

(define equal? ; list-equal?
  (lambda (x y)
    (if (and (pair? x)
             (pair? y))
        (and (equal? (car x) (car y))
             (equal? (cdr x) (cdr y)))
        (eqv? x y) )))

; ------------------------------------------------------------------------------
;  6.2 Standard Numerical Library (Part 2 of 2)
; ------------------------------------------------------------------------------

(define number?
  (lambda (x)
    (or (exact? x)
        (inexact? x) )))

(define complex?
  (lambda (x)
    ; (procedure numerical.so "is_complex") ; unimplemented
    #false
    ))

(define real?
  (procedure numerical.so "real_"))

(define rational?
  (lambda (x)
    ; (procedure numerical.so "is_rational") ; unimplemented
    #false
    ))

(define exact-integer?
  (lambda (x)
    ; (procedure numerical.so "is_exact_integer") ; unimplemented
    #false
    ))

(define integer?
  (lambda (x)
    (or (exact-integer? x)
        (and (real? x)
             (= x (truncate x)) ))))

(define exact? ; Currently, any real numbers returns #false
  (lambda (z)
    (or (integer? z)
        (rational? z)
        ; TODO for exact-complex
        )))

(define inexact?
  (lambda (z)
    (not (exact? z))))

(define finite?
  (lambda (z)
    (not (infinite? z))))

(define infinite?
  (lambda (z)
    #false
    ; (or (= +inf.0 z)
    ;     (= -inf.0 z))
    ))

(define nan?
  (lambda (z)
    #false
    ; (if (complex? z)
    ;     (or (= (real-part z) +nan.0)
    ;         (= (imag-part z) +nan.0))
    ;     (= z +nan.0))
    ))

(define zero?
  (lambda (n)
    (= n 0)))

(define positive?  (lambda (n) (> n 0)))
(define negative?  (lambda (n) (< n 0)))

(define even?
  (lambda (n)
    ; (= (remainder n 2) 0)
    (if (zero? n) #true
        (odd? (- n 1)))
    ))

(define odd?
  (lambda (n)
    ; (not (even? n))))
    (if (zero? n) #false
        (even? (- n 1)))
    ))

(define minimum
  (lambda (x . xs)
    (define minimum-aux
      (lambda (x xs)
        (if (null? xs)
            (inexact x)
            (minimum-aux (if (< (car xs) x) (car xs) x)
                         (cdr xs) ))))
    (if (inexact? x)
        (minimum-aux x xs)
        (let rec ((x x) (xs xs))
          (if (null? xs) x
              (if (inexact? (car xs))
                  (mimimum-aux x xs)
                  (rec (if (< (car xs) x) (car xs) x)
                       (cdr xs) )))))))

(define min minimum)

(define maximum
  (lambda (x . xs)
    (define maximum-aux
      (lambda (x xs)
        (if (null? xs)
            (inexact x)
            (maximum-aux (if (> (car xs) x) (car xs) x)
                         (cdr xs) ))))
    (if (inexact? x)
        (maximum-aux x xs)
        (let rec ((x x) (xs xs))
          (if (null? xs) x
              (if (inexact? (car xs))
                  (maximum-aux x xs)
                  (rec (if (> (car xs) x) (car xs) x)
                       (cdr xs) )))))))

(define max maximum)

(define abs
  (lambda (n)
    (if (< n 0) (- n) n)))

(define floor/)
(define floor-quotient)
(define floor-remainder)
(define truncate/)
(define truncate-quotient)
(define truncate-remainder)

(define quotient truncate-quotient)
(define remainder truncate-remainder)
(define modulo floor-remainder)

(define gcd
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

(define lcm
  (lambda xs
    (define lcm-2
      (lambda (a b)
        (abs (quotient (* a b) (gcd a b)))))
    (if (null? xs) 1
        (let rec ((n  (car xs))
                  (ns (cdr ns)))
          (if (null? ns) n
              (rec (lcm-2 n (car ns)) (cdr ns)) )))))

(define numerator
  (lambda (x)
    (if (rational? x)
        (car x)
        (if (exact? x) x
            (inexact (numerator (exact x))) ))))

(define denominator
  (lambda (x)
    (if (exact? x)
        (if (rational? x) (cdr x) 1)
        (if (integer? x) 1.0
            (inexact (denominator (exact x))) ))))

; TODO floor
; TODO ceiling
; TODO truncate
; TODO round

(define rationalize ; from Chibi-Scheme's lib/scheme/extras.scm
  (lambda (x e)
    (define sr
      (lambda (x y return)
        (let ((fx (floor x))
              (fy (floor y)))
          (cond
            ((>= fx x)
             (return fx 1))
            ((= fx fy)
             (sr (/ (- y fy))
                 (/ (- x fx))
                 (lambda (n d)
                   (return (+ d (* fx n)) n))))
            (else
              (return (+ fx 1) 1))))))
    (let ((return (if (not (negative? x)) /
                      (lambda (num den)
                        (/ (- num) den))))
          (x (abs x))
          (e (abs e)))
      (sr (- x e)
          (+ x e)
          return))))

; TODO (exp z)

(define log
  (lambda (z . base)
    (if (pair? base)
        (/ (ln x)
           (ln (car base)))
        (ln x))))

; TODO (sin z)
; TODO (cos z)
; TODO (tan z)
; TODO (asin z)
; TODO (acos z)

(define atan
  (lambda (y . x)
    (if (not (pair? x))
        (atan-1 y)
        (let ((x (inexact (car x))))
          (if (and (infinite? x)
                   (infinite? y))
              (* 0.7853981633974483
                 (if (< y 0) -1 1)
                 (if (= x -inf.0) 3 1))
              (if (negative? x)
                  (if (or (negative? y)
                          (= y -0.0))
                      (- (atan-1 (/ y x)) pi)
                      (- pi (atan-1 (/ y (- x)))))
                  (if (and (zero? x)
                           (zero? y))
                      (* (if (= y -0.0) -1 1)
                         (if (= x -0.0) pi x))
                      (atan-1 (/ y x)) )))))))

(define square
  (lambda (z)
    (* z z)))

; TODO sqrt
; TODO exact-integer-sqrt
; TODO expt


;; Standard Complex Library

(define make-rectangular
  (lambda (x y)
    (+ x (* y (sqrt -1)))))

(define make-polar
  (lambda (radius phi)
    (make-rectangular (* radius (cos phi))
                      (* radius (sin phi)) )))

(define real-part
  (lambda (z)
    (if (complex? z) (car z) z)))

(define imag-part
  (lambda (z)
    (if (complex? z) (cdr z) 0)))

(define magnitude
  (lambda (z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)) ))))

(define angle
  (lambda (z)
    (atan (imag-part z)
          (real-part z) )))

(define inexact identity)
(define exact #;undefined)

; TODO number->string
; TODO string->number

; ------------------------------------------------------------------------------
;  6.3 Standard Boolean Library (Part 2 of 2)
; ------------------------------------------------------------------------------

(define boolean?
  (lambda (x)
    (or (eqv? x #true)
        (eqv? x #false))))

(define boolean=?
  (lambda (x y . xs)
    (and (eqv? x y)
         (if (pair? xs)
             (apply boolean=? y xs)
             #true ))))

; ------------------------------------------------------------------------------
;  6.5 Symbols
; ------------------------------------------------------------------------------

(define symbol.so
  (linker "libmeevax-symbol.so"))

(define symbol ; Constructor
  (procedure symbol.so "symbol"))

; (define symbol?
;   (procedure symbol.so "is_symbol"))

(define symbol=?
  (lambda (x y . xs)
    (and (eq? x y)
         (if (pair? xs)
             (apply symbol=? y xs)
             #true ))))

; TODO symbol->string

(define string->symbol symbol)

; ------------------------------------------------------------------------------
;  6.6 Standard Characters Library
; ------------------------------------------------------------------------------

(define character.so
  (linker "libmeevax-character.so"))

(define character?
  (procedure character.so "is_character"))

(define char? character?)

(define character-compare
  (lambda (x xs compare)
    (let rec ((compare compare)
              (lhs (char->integer x))
              (xs xs))
      (if (null? xs) #true
          (let ((rhs (char->integer (car xs))))
            (and (compare lhs rhs)
                 (rec rhs (cdr xs) compare) ))))))

(define char=?
  (lambda (x . xs)
    (character-compare x xs =)))

(define char<?
  (lambda (x . xs)
    (character-compare x xs <)))

(define char>?
  (lambda (x . xs)
    (character-compare x xs >)))

(define char<=?
  (lambda (x . xs)
    (character-compare x xs <=)))

(define char>=?
  (lambda (x . xs)
    (character-compare x xs >=)))

(define case-insensitive-character-compare
  (lambda (x xs compare)
    (let rec ((compare compare)
              (lhs (char->integer (char-downcase x)))
              (xs xs))
      (if (null? xs) #true
          (let ((rhs (char->integer (char-downcase (car xs)))))
            (and (compare lhs rhs)
                 (rec rhs (cdr xs) compare)))))))

(define char-ci=? ;                                                (scheme char)
  (lambda (x . xs)
    (case-insensitive-character-compare x xs =)))

(define char-ci<? ;                                                (scheme char)
  (lambda (x . xs)
    (case-insensitive-character-compare x xs <)))

(define char-ci>? ;                                                (scheme char)
  (lambda (x . xs)
    (case-insensitive-character-compare x xs >)))

(define char-ci<=? ;                                               (scheme char)
  (lambda (x . xs)
    (case-insensitive-character-compare x xs <=)))

(define char-ci>=? ;                                               (scheme char)
  (lambda (x . xs)
    (case-insensitive-character-compare x xs >=)))

(define codepoint
  (procedure character.so "codepoint"))

(define char->integer codepoint)

; (define alphabetical-character?
;   (lambda (x)
;     (<= #,(char->integer #\A)
;           (char->integer (char-upcase x))
;         #,(char->integer #\Z))))
;
; (define char-alphabetic? alphabetical-character?) ;                (scheme char)

(define digit-value ;                                              (scheme char)
  (procedure character.so "digit_value"))

; (define numerical-character?
;   (lambda (x)
;     (<= #,(char->integer #\0)
;           (char->integer x)
;         #,(char->integer #\9))))
;
; (define char-numeric? numerical-character?) ;                      (scheme char)

(define whitespace-character?
  (lambda (x)
    (or (eqv? x #\space)
        (eqv? x #\tab)
        (eqv? x #\newline)
        (eqv? x #\return))))

(define char-whitespace? whitespace-character?) ;                  (scheme char)

; (define uppercase-character?
;   (lambda (x)
;     (<= #,(char->integer #\A)
;           (char->integer x)
;         #,(char->integer #\Z))))
;
; (define char-upper-case? uppercase-character?) ;                   (scheme char)

; (define lowercase-character?
;   (lambda (x)
;     (<= #,(char->integer #\a)
;           (char->integer x)
;         #,(char->integer #\z))))
;
; (define char-lower-case? lowercase-character?) ;                   (scheme char)

; ------------------------------------------------------------------------------
;  6.7 Standard Strings Library
; ------------------------------------------------------------------------------

(define string.so
  (linker "libmeevax-string.so"))

(define string?
  (procedure string.so "is_string"))

(define ccons
  (procedure string.so "ccons"))

(define make-string
  (lambda (k . x)
    (let ((default (if (pair? x) (car x) #\null)))
      (let rec ((k k)
                (result '()))
        (if (<= k 0) result
            (rec (- k 1)
                 (ccons default result)))))))

(define string
  (lambda xs
    (if (null? xs)
       '()
        (list->string xs))))

(define list->string
  (lambda (x)
    (if (null? x)
       '()
        (if (pair? x)
            (ccons (car x)
                   (list->string (cdr x)))
            (ccons x '())))))

(define string-from-number
  (procedure string.so "string_from_number"))

(define number->string string-from-number)

(define string->list
  (lambda (x)
    (if (null? x)
       '()
        (if (string? x)
            (cons (car x)
                  (string->list (cdr x)))
            (cons x '()) ; This maybe error
          ))))

(define string-length
  (lambda (x)
    (let rec ((x x)
              (result 0))
      (if (null? x) result
          (rec (cdr x) (+ result 1))))))

(define lexicographical-compare-2
  (lambda (x y)
    (if (or (null? x)
            (null? y))
        (- (string-length y)
           (string-length x))
        (let ((distance (- (char->integer (car y))
                           (char->integer (car x)))))
          (if (not (zero? distance)) distance
              (lexicographical-compare-2 (cdr x)
                                         (cdr y)))))))

(define lexicographical-compare
  (lambda (x xs . compare)
    (let ((compare (if (pair? compare) (car compare) =)))
      (if (null? xs) #true
          (and (compare 0 (lexicographical-compare-2 x (car xs)))
               (lexicographical-compare (car xs) (cdr xs) compare))))))

(define string=? (lambda (x . xs) (lexicographical-compare x xs =)))
(define string<? (lambda (x . xs) (lexicographical-compare x xs <)))
(define string>? (lambda (x . xs) (lexicographical-compare x xs >)))

(define string<=? (lambda (x . xs) (lexicographical-compare x xs <=)))
(define string>=? (lambda (x . xs) (lexicographical-compare x xs >=)))

(define case-insensitive-lexicographical-compare-2
  (lambda (x y)
    (if (or (null? x)
            (null? y))
        (- (string-length y)
           (string-length x))
        (let ((distance (- (char->integer (char-downcase (car y)))
                           (char->integer (char-downcase (car x))))))
          (if (not (zero? distance)) distance
              (case-insensitive-lexicographical-compare-2 (cdr x)
                                                          (cdr y)))))))

(define case-insensitive-lexicographical-compare
  (lambda (x xs . compare)
    (let ((compare (if (pair? compare) (car compare) =)))
      (if (null? xs) #true
          (and (compare 0 (case-insensitive-lexicographical-compare-2 x (car xs)))
               (case-insensitive-lexicographical-compare (car xs) (cdr xs) compare))))))

(define string-ci=? (lambda (x . xs) (case-insensitive-lexicographical-compare x xs =)))
(define string-ci<? (lambda (x . xs) (case-insensitive-lexicographical-compare x xs <)))
(define string-ci>? (lambda (x . xs) (case-insensitive-lexicographical-compare x xs >)))

(define string-ci<=? (lambda (x . xs) (case-insensitive-lexicographical-compare x xs <=)))
(define string-ci>=? (lambda (x . xs) (case-insensitive-lexicographical-compare x xs >=)))

(define string-reference list-ref)
(define string-ref string-reference)

(define string-append-2
  (lambda (x y)
    (if (null? x) y
        (ccons (car x)
               (string-append-2 (cdr x) y) ))))

(define string-reverse
  (lambda (x)
    (if (null? x)
       '()
        (string-append-2 (string-reverse (cdr x))
                         (string (car x)) ))))

(define string-append
  (lambda x
    (define string-append-aux
      (lambda (x y)
        (if (null? x) y
            (string-append-aux (cdr x)
                               (string-append-2 (car x) y) ))))
    (if (null? x)
       '()
        (let ((reversed (string-reverse x)))
          (string-append-aux (cdr reversed)
                             (car reversed) )))))

; string->list

; string-copy
; string-copy!
; string-fill!

; ------------------------------------------------------------------------------
;  6.8 Standard Vectors Library
; ------------------------------------------------------------------------------

(define vector.so
  (linker "libmeevax-vector.so"))

(define vector?
  (lambda (object) #false))

(define vector-of
  (procedure vector.so "vector_of"))

(define vector vector-of)

(define vector-reference
  (procedure vector.so "vector_reference"))

(define vector-ref
        vector-reference)

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
    (or (procedure? x)
        (closure? x)
        (continuation? x) )))

; TODO string-map
; TODO vector-map

; TODO string-for-each
; TODO vector-for-each

(define call-with-current-continuation ; hack
  (lambda (procedure)
    (call-with-current-continuation procedure)))

(define call/cc call-with-current-continuation)

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

; TODO dynamic-wind

; ------------------------------------------------------------------------------
;  6.11 Standard Exceptions Library
; ------------------------------------------------------------------------------

; TODO with-exception-handler
; TODO raise
; TODO raise-continuable

(define error display)

(define error-object?
  (lambda (x) #false) )

; TODO error-object?
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

(define io.so
  (linker "libmeevax-io.so"))

; TODO call-with-port

(define call-with-input-file
  (lambda (path proc)
    (let* ((input-port (open-input-file path))
           (result (proc input-port)))
      (close-input-port input-port)
      result)))

(define call-with-output-file
  (lambda (path proc)
    (let* ((output-port (open-output-file path))
           (result (proc output-port)))
      (close-output-port output-port)
      result)))

(define input-port?
  (procedure io.so "is_input_port"))

(define output-port?
  (procedure io.so "is_output_port"))

(define port?
  (lambda (x)
    (or (input-port? x) (output-port? x))))

(define textual-port? port?)

(define binary-port?
  (lambda (x) #false))

; TODO input-port-open?
; TODO output-port-open?

; TODO current-input-port
; TODO current-output-port
; TODO current-error-port

; TODO with-input-from-file
; TODO with-output-to-file

(define open-input-file
  (procedure io.so "open_input_file"))

; TODO open-binary-input-file

(define open-output-file
  (procedure io.so "open_output_file"))

; TODO open-binary-output-file

(define close-input-port
  (procedure io.so "close_input_port"))

(define close-output-port
  (procedure io.so "close_output_port"))

(define close-port
  (lambda (x)
    (if (input-port? x)
        (close-input-port x)
        (if (output-port? x)
            (close-output-port x)
           '())))) ; TODO unspecified

; TODO open-input-string
; TODO open-output-string
; TODO get-output-string

; TODO open-input-bytevector
; TODO open-output-bytevector
; TODO get-output-bytevector

; TODO read-char
; TODO peek-char
; TODO read-line

; (define eof-object
;   (lambda () #\end-of-file))
;
; (define eof-object?
;   (lambda (x)
;     (eqv? x #\end-of-file)))

; TODO eof-object

; TODO char-ready?

; TODO read-string
; TODO read-u8

; TODO u8-ready?

; TODO read-bytevector
; TODO read-bytevector!

; TODO write-shared

(define experimental.so
  (linker "libmeevax-experimental.so"))

(define display
  (procedure experimental.so "display")
  ; (lambda (x . option)
  ;   (let ((output-port (if (pair? option)
  ;                          (car option)
  ;                          (current-output-port))))
  ;     (if (char? x)
  ;         (write-char x output-port)
  ;         (write      x output-port))))
  )

(define newline
  (lambda ()
    (display "\n"))
  ; (lambda option
  ;   (write-char #\newline (if (pair? option)
  ;                             (car option)
  ;                             (current-output-port))))
  )

; TODO write-char
; TODO write-string
; TODO write-u8
; TODO write-bytevector
; TODO flush-output-port

; ------------------------------------------------------------------------------
;  6.14 Standard System Interface Library
; ------------------------------------------------------------------------------

; TODO file-exists?
; TODO delete-file
; TODO command-line

(define emergency-exit ;                                (scheme process-context)
  (procedure experimental.so "emergency_exit"))

(define exit emergency-exit) ;                          (scheme process-context)

; TODO get-environment-variable
; TODO get-environment-variables

; TODO current-second
; TODO current-jiffy
; TODO jiffies-per-second

; ------------------------------------------------------------------------------
;  SRFI 1 Extended Pairs and Lists Library
; ------------------------------------------------------------------------------

(define xcons
  (lambda (x y)
    (cons y x)))

(define find
  (lambda (predicate list)
    (cond
      ((find-tail predicate list)
       => car)
      (else #false) )))

(define find-tail
  (lambda (predicate list)
    (let rec ((list list))
      (and (not (null-list? list))
           (if (predicate (car list)) list
               (rec (cdr list)) )))))

(define null-list?
  (lambda (list)
    (cond
      ((pair? list) #false)
      ((null? list) #true)
      (else
       (error "null-list?: argument out of domain" list) ))))

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

      (begin (define equivalence.so (linker "libmeevax-equivalence.so"))
             (define numerical.so   (linker "libmeevax-numerical.so"))

             (define identity
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

