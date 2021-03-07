; ------------------------------------------------------------------------------
;       ...          =>      abs      and angle append apply      assoc assq
; assv atan       boolean? caaaar caaadr caaar caadar caaddr caadr caar cadaar
; cadadr cadar caddar cadddr caddr cadr call-with-current-continuation
; call-with-input-file call-with-output-file call-with-values     case cdaaar
; cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr
; cddr                           char-alphabetic? char-ci<=? char-ci<? char-ci=?
; char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready?
; char-upcase char-upper-case? char-whitespace? char<=? char<? char=? char>=?
; char>?       close-input-port close-output-port complex? cond
; current-input-port current-output-port        define-syntax delay denominator
; display do dynamic-wind else                 equal?           even?
; exact->inexact exact?                for-each force gcd    imag-part
; inexact->exact inexact? input-port?               integer?
; interaction-environment        lcm length let let* let-syntax letrec
; letrec-syntax list                           list-ref list-tail list?      log
; magnitude make-polar make-rectangular                         map max member
; memq memv min modulo negative? newline not null-environment null?
;                number? numerator odd?                                  or
; output-port?       peek-char positive? procedure? quasiquote       quotient
; rational? rationalize read read-char real-part real? remainder reverse
; scheme-report-environment                                 string
;                                             string-ci<=? string-ci<?
; string-ci=? string-ci>=? string-ci>?             string-fill!
;
; substring                        syntax-rules              values
;
; with-input-from-file with-output-to-file write write-char zero?
; ------------------------------------------------------------------------------

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
  (list fork/csc
    (list lambda '() datum)))

; (define identifier? syntactic-continuation?)

(define (free-identifier=? x y)
  (if (symbol? x)
      (if (symbol? y)
          (eq? x y)
          (if (syntactic-keyword? y)
              (eq? x (car y))
              #f))
      (if (syntactic-keyword? x)
          (if (syntactic-keyword? y)
              (eqv? x y)
              (if (symbol? y)
                  (eq? (car x) y)
                  #f))
          #f)))

(define (current-environment) (fork/csc identity))

; (define (er-macro-transformer transform) ; unstable
;   (fork/csc
;     (define hoge (current-environment))
;     (lambda form
;       (transform form (lambda (x) (eval x hoge)) eqv?))))

(define (er-macro-transformer transform) ; unhygienic-dummy
  (fork/csc
    (lambda form
      (transform form identity eqv?))))

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
  (cond
    ((null? tests) #f)
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

(define-syntax (letrec bindings . body)
  ((lambda (definitions)
     `((,lambda () ,@definitions ,@body)) )
   (map (lambda (x) (cons define x)) bindings)))

(define-syntax letrec* letrec) ; TODO MOVE INTO (scheme base)

(define-syntax (let bindings . body)
  (if (identifier? bindings)
      `(,letrec ((,bindings
                   (,lambda ,(map car (car body)) ,@(cdr body))))
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

; ------------------------------------------------------------------------------
;       ...          =>      abs          angle                   assoc assq
; assv atan       boolean? caaaar caaadr       caadar caaddr            cadaar
; cadadr       caddar cadddr            call-with-current-continuation
; call-with-input-file call-with-output-file call-with-values          cdaaar
; cdaadr       cdadar cdaddr            cddaar cddadr       cdddar cddddr
;                                char-alphabetic? char-ci<=? char-ci<? char-ci=?
; char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready?
; char-upcase char-upper-case? char-whitespace? char<=? char<? char=? char>=?
; char>?       close-input-port close-output-port complex?
; current-input-port current-output-port                      delay denominator
; display    dynamic-wind else                 equal?           even?
; exact->inexact exact?                for-each force gcd    imag-part
; inexact->exact inexact? input-port?               integer?
; interaction-environment        lcm length          let-syntax
; letrec-syntax                                list-ref list-tail list?      log
; magnitude make-polar make-rectangular                             max
;           min modulo negative? newline     null-environment
;                number? numerator odd?
; output-port?       peek-char positive? procedure?                  quotient
; rational? rationalize read read-char real-part real? remainder
; scheme-report-environment                                 string
;                                             string-ci<=? string-ci<?
; string-ci=? string-ci>=? string-ci>?             string-fill!
;
; substring                        syntax-rules              values
;
; with-input-from-file with-output-to-file write write-char zero?
; ------------------------------------------------------------------------------

; ---- 6.1. Equivalence predicates ---------------------------------------------

(define (equal? x y)
  (if (and (pair? x)
           (pair? y))
      (and (equal? (car x)
                   (car y))
           (equal? (cdr x)
                   (cdr y)))
      (eqv? x y)))

; ---- 6.2. Numbers ------------------------------------------------------------

; .
; `-- number?
;      `-- complex?
;           |-- %complex?
;           `-- real?
;                |-- floating-point?
;                |    |-- single-float?
;                |    `-- double-float?
;                `-- rational?
;                     |-- ratio?
;                     `-- integer?
;                          |-- inexact-integer?
;                          `-- exact-integer?

(define (number? x) (complex? x))

(define (complex? x)
  (or (%complex? x)
      (real? x)))

(define (real? x)
  (or (floating-point? x)
      (rational? x)))

(define (floating-point? z)
  (or (single-float? z)
      (double-float? z)))

(define (rational? x)
  (or (ratio? x)
      (integer? x)))

(define (integer? x)
  (or (exact-integer? x) ; TODO ratio e.g. 3/1 is integer.
      (inexact-integer? x)))

(define (inexact-integer? x)
  (and (floating-point? x)
       (= x (truncate x))))

;  .
;  |-- exact?
;  |    |-- exact-complex?
;  |    |-- exact-integer?
;  |    `-- ratio?
;  `-- inexact?
;       |-- inexact-complex?
;       `-- floating-point?
;            |--- single-float?
;            `--- single-float?

(define (exact? z)
  (or (exact-integer? z)
      (ratio? z)
      (exact-complex? z)))

(define (exact-complex? x) ; TODO move into r7rs.ss
  (and (%complex? x)
       (exact? (real-part x))
       (exact? (imag-part x))))

(define (inexact? z)
  (or (floating-point? z)
      (inexact-complex? z)))

(define (inexact-complex? x)
  (and (%complex? x)
       (or (inexact? (real-part x))
           (inexact? (imag-part x)))))

(define (zero?     n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))

(define (odd? n) (not (even? n)))
(define (even? n) (= (remainder n 2) 0))

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

(define (floor-remainder a b) (% (+ b (% a b)) b))

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

(define (log z . base)
  (if (pair? base)
      (/ (ln z)
         (ln (car base)))
      (ln z)))

(define (atan y . o)
  (if (pair? o)
      (atan-2 y (car o))
      (atan-1 y)))

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

; ---- 6.4. Pairs and lists ----------------------------------------------------

; ---- 6.5 Symbols -------------------------------------------------------------

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
  (or (native-procedure? x)
      (closure? x)
      (continuation? x)))

(define %current-dynamic-extents '()) ; https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html

(define (dynamic-wind before thunk after)
  (before)
  (set! %current-dynamic-extents (cons (cons before after) %current-dynamic-extents))
  ((lambda (result) ; TODO let-values
     (set! %current-dynamic-extents (cdr %current-dynamic-extents))
     (after)
     result) ; TODO (apply values result)
   (thunk)))

(define call-with-current-continuation
  (let ((call/cc (lambda (procedure)
                   (call-with-current-continuation procedure))))
    (lambda (procedure)
      (define (windup! from to)
        (set! %current-dynamic-extents from)
        (cond ((eq? from to))
              ((null? from) (windup! from (cdr to)) ((caar to)))
              ((null? to) ((cdar from)) (windup! (cdr from) to))
              (else ((cdar from)) (windup! (cdr from) (cdr to)) ((caar to))))
        (set! %current-dynamic-extents to))
      (let ((current-dynamic-extents %current-dynamic-extents))
        (call/cc (lambda (k1)
                   (procedure (lambda (k2)
                                (windup! %current-dynamic-extents current-dynamic-extents)
                                (k1 k2)))))))))

; (define values
;   (lambda xs
;     (call-with-current-continuation
;       (lambda (cc)
;         (apply cc xs)))))

(define values-tag (list 'values)) ; Magic Token Trick

(define (values? x)
  (if (pair? x)
      (eq? (car x) values-tag)
      #f))

(define (values . xs)
  (if (if (null? xs) #f
          (null? (cdr xs)))
      (car xs)
      (cons values-tag xs)))

; (define (call-with-values producer consumer) ; TODO
;   (let-values ((xs (producer)))
;     (apply consumer xs)))

(define (call-with-values producer consumer)
  ((lambda (vs)
     (if (values? vs)
         (apply consumer (cdr vs))
         (consumer vs)))
   (producer)))

; ---- 6.11. Exceptions --------------------------------------------------------

; ---- 6.12. Environments and evaluation ---------------------------------------

; ---- 6.13. Input and output --------------------------------------------------

(define (call-with-port port procedure) (procedure port)) ; R7RS

(define (call-with-input-file path procedure)
  (call-with-port (open-input-file path) procedure))

(define (call-with-output-file path procedure)
  (call-with-port (open-output-file path) procedure))

(define (input-standard-port? x)
  (eq? x (input-standard-port)))

(define (output-standard-port? x)
  (eq? x (output-standard-port)))

(define (error-standard-port? x)
  (eq? x (error-standard-port)))

(define (standard-port? x)
  (or (input-standard-port? x)
      (output-standard-port? x)
      (error-standard-port? x)))

(define (input-port? x)
  (or (input-file-port? x)
      (input-string-port? x)
      (input-standard-port? x)))

(define (output-port? x)
  (or (output-file-port? x)
      (output-string-port? x)
      (output-standard-port? x)
      (error-standard-port? x)))

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

(define (read        x) (::read        (if (pair? x) (car x) (current-input-port))))
(define (read-char   x) (::read-char   (if (pair? x) (car x) (current-input-port))))
(define (peek-char   x) (::peek-char   (if (pair? x) (car x) (current-input-port))))
(define (char-ready? x) (::char-ready? (if (pair? x) (car x) (current-input-port))))

(define (write-simple datum . port)
  (::write-simple datum (if (pair? port)
                            (car port)
                            (current-output-port))))

(define write write-simple)

(define (display datum . port)
  (cond ((char?   datum) (apply write-char    datum port))
        ((string? datum) (apply write-string  datum port))
        ((path?   datum) (apply write-path    datum port))
        (else            (apply write         datum port))))

(define (newline . port)
  (apply write-char #\newline port))

(define (write-char char . port)
  (::write-char char (if (pair? port)
                         (car port)
                         (current-output-port))))

; ---- 6.14. System interface --------------------------------------------------

; ------------------------------------------------------------------------------
;       ...          =>                                           assoc assq
; assv                     caaaar caaadr       caadar caaddr            cadaar
; cadadr       caddar cadddr
;                                                                      cdaaar
; cdaadr       cdadar cdaddr            cddaar cddadr       cdddar cddddr
;
;
;
;
; current-input-port current-output-port                      delay
;                         else
;                                      for-each force
;
; interaction-environment            length          let-syntax
; letrec-syntax                                list-ref list-tail list?
;
;                                            null-environment
;
;
;
; scheme-report-environment
;
;
;
;                                  syntax-rules
;
; with-input-from-file with-output-to-file
; ------------------------------------------------------------------------------
