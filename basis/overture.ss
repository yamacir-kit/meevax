(define (identity x) x)

(define (list . xs) xs)

(define fork/csc fork-with-current-syntactic-continuation)

(define define-syntax
  (fork/csc
    (lambda (define-syntax keyword . transformer)
      (if (pair? keyword)
          (list define (car keyword)
            (list fork/csc
              (list lambda keyword . transformer)))
          (list define keyword . transformer)))))

(define-syntax (syntax datum)
  (if (pair? datum)
      (list fork/csc (list lambda '() datum))
      (eval datum (fork/csc identity))))

(define (current-environment-specifier)
  (fork/csc identity))

(define (current-evaluator)
  ((lambda (e)
     (lambda (x)
       (eval x e)))
   (current-environment-specifier)))

(define (er-macro-transformer transform)
  (fork/csc
    (lambda form
      (transform form (current-evaluator) free-identifier=?))))

(define (null? x) (eqv? x '()))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (unspecified) (if #f #f))

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
  (cond ((null? tests) #f)
        ((null? (cdr tests)) (car tests))
        (else (list (list lambda (list result)
                          (list if result
                                result
                                (cons or (cdr tests))))
                    (car tests)))))

(define (append-2 x y)
  (if (null? x) y
      (cons (car x)
            (append-2 (cdr x) y))))

(define (reverse x)
  (if (null? x) '()
      (append-2 (reverse (cdr x))
                (list (car x)))))

(define (append . xs) ; redefine
  (define (append-aux x xs)
    (if (null? x) xs
        (append-aux (cdr xs)
                    (append-2 (car x) xs))))

  (if (null? xs) '()
      ((lambda (xs)
         (append-aux (cdr xs)
                     (car xs)))
       (reverse xs))))

(define-syntax (quasiquote template)
  (define (expand x depth)
    (cond
      ((pair? x)
       (cond

         ((free-identifier=? unquote (car x))
          (if (<= depth 0)
              (cadr x)
              (list list (list quote 'unquote) (expand (cadr x) (- depth 1)))))

         ((free-identifier=? unquote-splicing (car x))
          (if (<= depth 0)
              (list cons (expand (car x) depth)
                         (expand (cdr x) depth))
              (list list (list quote 'unquote-splicing)
                         (expand (cadr x) (- depth 1)))))

         ((free-identifier=? quasiquote (car x))
          (list list (list quote 'quasiquote)
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

(define (not x) (if x #f #t))

(define-syntax (when   test . body) `(,if       ,test  (,begin ,@body))) ; TODO MOVE INTO (scheme base)
(define-syntax (unless test . body) `(,if (,not ,test) (,begin ,@body))) ; TODO MOVE INTO (scheme base)

(define (map f x . xs) ; map-unorder
    (define (map-1 f x xs)
      (if (pair? x)
          (map-1 f
                 (cdr x)
                 (cons (f (car x)) xs))
          (reverse xs)))

    (define (map-2+ f xs xss)
      (if (every pair? xs)
          (map-2+ f
                  (map-1 cdr xs '())
                  (cons (apply f (map-1 car xs '())) xss))
          (reverse xss)))

    (if (null? xs)
        (map-1  f       x     '())
        (map-2+ f (cons x xs) '())))

(define (apply f x . xs) ; for map
  (define (apply-1 f xs) (f . xs))

  (if (null? xs)
      (apply-1 f x)
      ((lambda (rxs)
         (apply-1 f
                  (append-2 (reverse (cdr rxs))
                            (car rxs))))
       (reverse (cons x xs)))))

(define (every f x . xs) ; from SRFI-1 for map
  (define (every-1 f x)
    (if (null? (cdr x))
        (f (car x))
        (if (f (car x))
            (every-1 f (cdr x))
            #f)))

  (if (null? xs)
      (if (pair? x)
          (every-1 f x)
          #t)
      (not (apply any
                  (lambda xs
                    (not (apply f xs)))
                  x xs))))

(define (any f x . xs) ; from SRFI-1 for every
  (define (any-1 f x)
    (if (pair? (cdr x))
        ((lambda (result)
           (if result result (any-1 f (cdr x))))
         (f (car x)))
        (f (car x))))

  (define (any-2+ f xs)
    (if (every pair? xs)
        ((lambda (result)
           (if result result (any-2+ f (map cdr xs))))
         (apply f (map car xs)))
        #f))

  (if (null? xs)
      (if (pair? x)
          (any-1 f x)
          #f)
      (any-2+ f (cons x xs))))

(define-syntax (letrec* bindings . body)
  ((lambda (definitions)
     `((,lambda () ,@definitions ,@body)) )
   (map (lambda (x) (cons define x)) bindings)))

(define-syntax (let bindings . body)
  (if (identifier? bindings)
      `(,letrec ((,bindings (,lambda ,(map car (car body)) ,@(cdr body))))
         (,bindings ,@(map cadr (car body))))
      `((,lambda ,(map car bindings) ,@body) ,@(map cadr bindings))))

(define-syntax (let* bindings . body)
  (if (or (null? bindings)
          (null? (cdr bindings)))
      `(,let (,(car bindings)) ,@body)
      `(,let (,(car bindings)) (,let* ,(cdr bindings) ,@body))))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (member o x . c) ; for case
  (let ((compare (if (pair? c) (car c) equal?)))
    (let member ((x x))
      (and (pair? x)
           (if (compare o (car x)) x
               (member (cdr x)))))))

(define (memq o x) (member o x eq?))
(define (memv o x) (member o x eqv?))

(define-syntax (case key . clauses)
  (define (body expressions)
    (cond
      ((null? expressions) result)
      ((free-identifier=? => (car expressions)) `(,(cadr expressions) ,result))
      (else `(,begin ,@expressions))))

  (define (each-clause clauses)
    (cond
      ((null? clauses) (unspecified))
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
              ,(each-clause (cdr clauses))))))

  `(,let ((,result ,key)) ,(each-clause clauses)))

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

; ---- 6.1. Equivalence predicates ---------------------------------------------

(define (equal? x y) ; structure=?
  (if (and (pair? x)
           (pair? y))
      (and (equal? (car x)
                   (car y))
           (equal? (cdr x)
                   (cdr y)))
      (eqv? x y)))

; ---- 6.2. Numbers ------------------------------------------------------------

(define (exact? z)
  (define (exact-complex? x)
    (and (%complex? x)
         (exact? (real-part x))
         (exact? (imag-part x))))
  (or (exact-complex? z)
      (ratio? z)
      (exact-integer? z)))

(define (inexact? z)
  (define (inexact-complex? x)
    (and (%complex? x)
         (or (inexact? (real-part x))
             (inexact? (imag-part x)))))
  (define (floating-point? z)
    (or (single-float? z)
        (double-float? z)))
  (or (inexact-complex? z)
      (floating-point? z)))

(define (zero?     n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (odd?      n) (not (even? n)))
(define (even?     n) (= (remainder n 2) 0))

(define (max x . xs)
  (define (max-aux x xs)
    (if (null? xs)
        (inexact x)
        (max-aux (if (< x (car xs)) (car xs) x)
                 (cdr xs))))
  (if (inexact? x)
      (max-aux x xs)
      (let rec ((x x) (xs xs))
        (cond ((null? xs) x)
              ((inexact? (car xs)) (max-aux x xs))
              (else (rec (if (< x (car xs)) (car xs) x)
                         (cdr xs)))))))

(define (min x . xs)
  (define (min-aux x xs)
    (if (null? xs)
        (inexact x)
        (min-aux (if (< (car xs) x) (car xs) x)
                 (cdr xs))))
  (if (inexact? x)
      (min-aux x xs)
      (let rec ((x x) (xs xs))
        (cond ((null? xs) x)
              ((inexact? (car xs)) (min-aux x xs))
              (else (rec (if (< (car xs) x) (car xs) x)
                         (cdr xs)))))))

(define (abs n)
  (if (< n 0) (- n) n))

(define (floor-quotient x y) (floor (/ x y)))

(define (floor-remainder x y) (% (+ y (% x y)) y))

(define (floor/ x y)
  (values (floor-quotient x y)
          (floor-remainder x y)))

(define (truncate-quotient x y) (truncate (/ x y)))

(define truncate-remainder %)

(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))

(define quotient truncate-quotient)

(define remainder truncate-remainder)

(define modulo floor-remainder)

(define (gcd . xs) ; from Chibi-Scheme lib/init7.scm
  (define (gcd-2 a b)
    (if (zero? b)
        (abs a)
        (gcd b (remainder a b))))
  (if (null? xs) 0
      (let rec ((n  (car xs))
                (ns (cdr xs)))
        (if (null? ns) n
            (rec (gcd-2 n (car ns)) (cdr ns))))))

(define (lcm . xs) ; from Chibi-Scheme lib/init7.scm
  (define (lcm-2 a b)
    (abs (quotient (* a b) (gcd a b))))
  (if (null? xs) 1
      (let rec ((n  (car xs))
                (ns (cdr xs)))
        (if (null? ns) n
            (rec (lcm-2 n (car ns)) (cdr ns))))))

(define (numerator x)
  (cond ((ratio? x) (car x))
        ((exact? x) x)
        (else (inexact (numerator (exact x))))))

(define (denominator x)
  (cond ((exact? x) (if (ratio? x) (cdr x) 1))
        ((integer? x) 1.0)
        (else (inexact (denominator (exact x))))))

(define (rationalize x e) ; from Chibi-Scheme lib/scheme/extras.scm (https://ml.cddddr.org/scheme/msg01498.html)
  (define (sr x y return)
    (let ((fx (floor x))
          (fy (floor y)))
      (cond ((>= fx x) (return fx 1))
            ((= fx fy) (sr (/ (- y fy))
                           (/ (- x fx))
                           (lambda (n d)
                             (return (+ d (* fx n)) n))))
            (else (return (+ fx 1) 1)))))
  (let ((return (if (negative? x)
                    (lambda (num den)
                      (/ (- num) den))
                    /))
        (x (abs x))
        (e (abs e)))
    (sr (- x e) (+ x e) return)))

(define (square z) (* z z))

(define (make-rectangular x y) (+ x (* y (sqrt -1))))

(define (make-polar radius phi)
  (make-rectangular (* radius (cos phi))
                    (* radius (sin phi))))

(define (real-part z) (if (%complex? z) (car z) z))
(define (imag-part z) (if (%complex? z) (cdr z) 0))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z)
        (real-part z)))

(define inexact->exact exact)
(define exact->inexact inexact)

; ---- 6.3. Booleans -----------------------------------------------------------

(define (boolean? x)
  (or (eqv? x #t)
      (eqv? x #f)))

(define boolean=? eqv?)

; ---- 6.4. Pairs and lists ----------------------------------------------------

; ---- 6.5 Symbols -------------------------------------------------------------

(define symbol=? eqv?)

; ---- 6.6 Characters ----------------------------------------------------------

(define (char-compare x xs compare)
  (let rec ((compare compare)
            (lhs (char->integer x))
            (xs xs))
    (if (null? xs) #t
        (let ((rhs (char->integer (car xs))))
          (and (compare lhs rhs)
               (rec compare rhs (cdr xs)))))))

(define (char=?  x . xs) (char-compare x xs =))
(define (char<?  x . xs) (char-compare x xs <))
(define (char>?  x . xs) (char-compare x xs >))
(define (char<=? x . xs) (char-compare x xs <=))
(define (char>=? x . xs) (char-compare x xs >=))

(define (char-ci-compare x xs compare)
  (let rec ((compare compare)
            (lhs (char->integer (char-downcase x)))
            (xs xs))
    (if (null? xs) #t
        (let ((rhs (char->integer (char-downcase (car xs)))))
          (and (compare lhs rhs)
               (rec compare rhs (cdr xs)))))))

(define (char-ci=?  x . xs) (char-ci-compare x xs =))
(define (char-ci<?  x . xs) (char-ci-compare x xs <))
(define (char-ci>?  x . xs) (char-ci-compare x xs >))
(define (char-ci<=? x . xs) (char-ci-compare x xs <=))
(define (char-ci>=? x . xs) (char-ci-compare x xs >=))

(define (char-alphabetic? x)
  (<= #,(char->integer #\a)
        (char->integer (char-downcase x))
      #,(char->integer #\z)))

(define (char-numeric? x)
  (<= #,(char->integer #\0)
        (char->integer x)
      #,(char->integer #\9)))

(define (char-whitespace? x)
  (or (eqv? x #\space)
      (eqv? x #\tab)
      (eqv? x #\newline)
      (eqv? x #\return)))

(define (char-upper-case? x)
  (<= #,(char->integer #\A)
        (char->integer x)
      #,(char->integer #\Z)))

(define (char-lower-case? x)
  (<= #,(char->integer #\a)
        (char->integer x)
      #,(char->integer #\z)))

(define (char-downcase c)
  (if (char-lower-case? c) c
      (integer->char (+ (char->integer c) 32))))

(define (char-upcase c)
  (if (char-upper-case? c) c
      (integer->char (- (char->integer c) 32))))

; ---- 6.7 Strings -------------------------------------------------------------

(define (string . xs) (list->string xs))

(define (string-ci=?  . xs) (apply string=?  (map string-foldcase xs)))
(define (string-ci<?  . xs) (apply string<?  (map string-foldcase xs)))
(define (string-ci>?  . xs) (apply string>?  (map string-foldcase xs)))
(define (string-ci<=? . xs) (apply string<=? (map string-foldcase xs)))
(define (string-ci>=? . xs) (apply string>=? (map string-foldcase xs)))

(define substring string-copy)

(define (string-fill! s c . o)
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
                 (rec (- k 1)))))))

; ---- 6.8. Vectors ------------------------------------------------------------

; ---- 6.9. Bytevectors --------------------------------------------------------

; ---- 6.10. Control features --------------------------------------------------

(define (procedure? x)
  (or (closure? x)
      (continuation? x)
      (foreign-function? x)))

(define %current-dynamic-extents '()) ; https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html

(define (dynamic-wind before thunk after)
  (before)
  (set! %current-dynamic-extents (cons (cons before after) %current-dynamic-extents))
  ((lambda (result) ; TODO let-values
     (set! %current-dynamic-extents (cdr %current-dynamic-extents))
     (after)
     result) ; TODO (apply values result)
   (thunk)))

(define (call-with-current-continuation procedure)
  (define (windup! from to)
    (set! %current-dynamic-extents from)
    (cond ((eq? from to))
          ((null? from) (windup! from (cdr to)) ((caar to)))
          ((null? to) ((cdar from)) (windup! (cdr from) to))
          (else ((cdar from)) (windup! (cdr from) (cdr to)) ((caar to))))
    (set! %current-dynamic-extents to))
  (let ((current-dynamic-extents %current-dynamic-extents))
    (call-with-current-continuation! (lambda (k1)
                                       (procedure (lambda (k2)
                                                    (windup! %current-dynamic-extents current-dynamic-extents)
                                                    (k1 k2)))))))

; (define values
;   (lambda xs
;     (call-with-current-continuation
;       (lambda (cc)
;         (apply cc xs)))))

(define <values> (list 'values)) ; Magic Token Trick

(define (values? x)
  (if (pair? x)
      (eq? (car x) <values>)
      #f))

(define (values . xs)
  (if (if (null? xs) #f
          (null? (cdr xs)))
      (car xs)
      (cons <values> xs)))

; (define (call-with-values producer consumer)
;   (let-values ((xs (producer)))
;     (apply consumer xs)))

(define (call-with-values producer consumer)
  ((lambda (vs)
     (if (values? vs)
         (apply consumer (cdr vs))
         (consumer vs)))
   (producer)))

; ---- 6.11. Exceptions --------------------------------------------------------

(define (error-object? x)
  (or (error? x)
      (read-error? x)
      (file-error? x)
      (syntax-error? x)))

(define error-object-message car)

(define error-object-irritants cdr)

; ---- 6.12. Environments and evaluation ---------------------------------------

; ---- 6.13. Input and output --------------------------------------------------

; (define (call-with-port port procedure)
;   (let-values ((results (procedure port)))
;     (close-port port)
;     (apply values results)))

(define (call-with-port port procedure)
  (let ((result (procedure port)))
    (close-port port)
    result))

(define (call-with-input-file path procedure)
  (call-with-port (open-input-file path) procedure))

(define (call-with-output-file path procedure)
  (call-with-port (open-output-file path) procedure))

(define (standard-input-port? x)
  (eq? x (standard-input-port)))

(define (standard-output-port? x)
  (eq? x (standard-output-port)))

(define (standard-error-port? x)
  (eq? x (standard-error-port)))

(define (standard-port? x)
  (or (standard-input-port? x)
      (standard-output-port? x)
      (standard-error-port? x)))

(define (input-port? x)
  (or (input-file-port? x)
      (input-string-port? x)
      (standard-input-port? x)))

(define (output-port? x)
  (or (output-file-port? x)
      (output-string-port? x)
      (standard-output-port? x)
      (standard-error-port? x)))

(define (close-port x)
  (cond ((input-port? x) (close-input-port x))
        ((output-port? x) (close-output-port x))
        (else (unspecified))))

(define (close-input-port x)
  (cond ((input-file-port? x)
         (close-input-file-port x))
        (else (unspecified))))

(define (close-output-port x)
  (cond ((output-file-port? x)
         (close-output-file-port x))
        (else (unspecified))))

(define (read        . x) (%read        (if (pair? x) (car x) (current-input-port))))
(define (read-char   . x) (%read-char   (if (pair? x) (car x) (current-input-port))))
(define (peek-char   . x) (%peek-char   (if (pair? x) (car x) (current-input-port))))
(define (char-ready? . x) (%char-ready? (if (pair? x) (car x) (current-input-port))))

(define (write-simple x . port) (%write-simple x (if (pair? port) (car port) (current-output-port))))
(define (write-char   x . port) (%write-char   x (if (pair? port) (car port) (current-output-port))))

(define write write-simple)

(define (display datum . port)
  (cond ((char?   datum) (apply write-char    datum port))
        ((string? datum) (apply write-string  datum port))
        ((path?   datum) (apply write-path    datum port))
        (else            (apply write         datum port))))

(define (newline . port)
  (apply write-char #\newline port))

(define (write-string string . xs)
  (case (length xs)
    ((0)  (%write-string string (current-output-port)))
    ((1)  (%write-string string (car xs)))
    (else (%write-string (apply string-copy string (cadr xs)) (car xs)))))

(define (flush-output-port . port)
  (%flush-output-port (if (pair? port)
                          (car port)
                          (current-output-port))))
