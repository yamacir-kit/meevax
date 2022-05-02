(define (identity x) x)

(define (list . xs) xs)

(define (unspecified) (if #f #f))

; ------------------------------------------------------------------------------

(define (traditional-macro-transformer f)
  (lambda (form use-env mac-env)
    (apply f (cdr form))))

(define (sc-macro-transformer f)
  (lambda (form use-env mac-env)
    (make-syntactic-closure mac-env '() (f form use-env))))

(define (rsc-macro-transformer f)
  (lambda (form use-env mac-env)
    (make-syntactic-closure use-env '() (f form mac-env))))

(define (er-macro-transformer f)
  (lambda (form use-env mac-env)
    (define rename:list (list))
    (define (rename x)
      (letrec ((assq (lambda (x alist)
                       (if (null? alist) #f
                           (if (eq? x (caar alist))
                               (car alist)
                               (assq x (cdr alist))))))
               (alist-cons (lambda (key x alist)
                             (cons (cons key x) alist))))
        (define cell (assq x rename:list))
        (if cell
            (cdr cell)
            (begin (set! rename:list (alist-cons x (make-syntactic-closure mac-env '() x) rename:list))
                   (cdar rename:list)))))
    (define (compare x y)
      (eqv? (if (syntactic-closure? x) x
                (make-syntactic-closure use-env '() x))
            (if (syntactic-closure? y) y
                (make-syntactic-closure use-env '() y))))
    (f form rename compare)))

(define-syntax import
  (er-macro-transformer
    (lambda (form rename compare)
      (list (rename 'quote) (cons 'import (cdr form))))))

; ------------------------------------------------------------------------------

(define-syntax cond
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cdr form))
          (unspecified)
          ((lambda (clause)
             (if (compare (rename 'else) (car clause))
                 (cons (rename 'begin) (cdr clause))
                 (if (if (null? (cdr clause)) #t
                         (compare (rename '=>) (cadr clause)))
                     (list (list (rename 'lambda)
                                 (list (rename 'result))
                                 (list (rename 'if)
                                       (rename 'result)
                                       (if (null? (cdr clause))
                                           (rename 'result)
                                           (list (caddr clause)
                                                 (rename 'result)))
                                       (cons (rename 'cond) (cddr form))))
                           (car clause))
                     (list (rename 'if)
                           (car clause)
                           (cons (rename 'begin) (cdr clause))
                           (cons (rename 'cond) (cddr form))))))
           (cadr form))))))

(define-syntax and
  (er-macro-transformer
    (lambda (form rename compare)
      (cond ((null? (cdr form)))
            ((null? (cddr form))
             (cadr form))
            (else (list (rename 'if)
                        (cadr form)
                        (cons (rename 'and)
                              (cddr form))
                        #f))))))

(define-syntax or
  (er-macro-transformer
    (lambda (form rename compare)
      (cond ((null? (cdr form)) #f)
            ((null? (cddr form))
             (cadr form))
            (else (list (list (rename 'lambda)
                              (list (rename 'result))
                              (list (rename 'if)
                                    (rename 'result)
                                    (rename 'result)
                                    (cons (rename 'or)
                                          (cddr form))))
                        (cadr form)))))))

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

(define-syntax quasiquote
  (er-macro-transformer
    (lambda (form rename compare)
      (define (expand x depth)
        (cond ((pair? x)
               (cond ((compare (rename 'unquote) (car x))
                      (if (<= depth 0)
                          (cadr x)
                          (list (rename 'list)
                                (list (rename 'quote) 'unquote)
                                (expand (cadr x) (- depth 1)))))
                     ((compare (rename 'unquote-splicing) (car x))
                      (if (<= depth 0)
                          (list (rename 'cons)
                                (expand (car x) depth)
                                (expand (cdr x) depth))
                          (list (rename 'list)
                                (list (rename 'quote) 'unquote-splicing)
                                (expand (cadr x) (- depth 1)))))
                     ((compare (rename 'quasiquote) (car x))
                      (list (rename 'list)
                            (list (rename 'quote) 'quasiquote)
                            (expand (cadr x) (+ depth 1))))
                     ((and (<= depth 0)
                           (pair? (car x))
                           (compare (rename 'unquote-splicing) (caar x)))
                      (if (null? (cdr x))
                          (cadar x)
                          (list (rename 'append)
                                (cadar x)
                                (expand (cdr x) depth))))
                     (else (list (rename 'cons)
                                 (expand (car x) depth)
                                 (expand (cdr x) depth)))))
              ((vector? x)
               (list (rename 'list->vector)
                     (expand (vector->list x) depth)))
              ((or (identifier? x)
                   (null? x))
               (list (rename 'quote) x))
              (else x)))
      (expand (cadr form) 0))))

(define (not x) (if x #f #t))

(define-syntax when
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'if) ,(cadr form)
                      (,(rename 'begin) ,@(cddr form))))))

(define-syntax unless
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'if) (,(rename 'not) ,(cadr form))
                      (,(rename 'begin) ,@(cddr form))))))

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

(define-syntax let
  (er-macro-transformer
    (lambda (form rename compare)
      (if (identifier? (cadr form))
          `(,(rename 'letrec) ((,(cadr form)
                                 (,(rename 'lambda) ,(map car (caddr form)) ,@(cdddr form))))
                              (,(cadr form) ,@(map cadr (caddr form))))
          `((,(rename 'lambda) ,(map car (cadr form)) ,@(cddr form))
            ,@(map cadr (cadr form)))))))


(define-syntax let*
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cadr form))
          `(,(rename 'let) () ,@(cddr form))
          `(,(rename 'let) (,(caadr form))
                           (,(rename 'let*) ,(cdadr form)
                                            ,@(cddr form)))))))

(define-syntax letrec*
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'let) ()
                       ,@(map (lambda (x) (cons (rename 'define) x))
                              (cadr form))
                       ,@(cddr form)))))

(define (member o x . c) ; for case
  (let ((compare (if (pair? c) (car c) equal?)))
    (let member ((x x))
      (and (pair? x)
           (if (compare o (car x)) x
               (member (cdr x)))))))

(define (memq o x) (member o x eq?))
(define (memv o x) (member o x eqv?))

(define-syntax case
  (er-macro-transformer
    (lambda (form rename compare)
      (define (body xs)
        (cond ((null? xs) (rename 'result))
              ((compare (rename '=>) (car xs)) `(,(cadr xs) ,(rename 'result)))
              (else `(,(rename 'begin) ,@xs))))
      (define (each-clause clauses)
        (cond ((null? clauses)
               (unspecified))
              ((compare (rename 'else) (caar clauses))
               (body (cdar clauses)))
              ((and (pair? (caar clauses))
                    (null? (cdaar clauses)))
               `(,(rename 'if) (,(rename 'eqv?) ,(rename 'result)
                                                (,(rename 'quote) ,(caaar clauses)))
                               ,(body (cdar clauses))
                               ,(each-clause (cdr clauses))))
              (else `(,(rename 'if) (,(rename 'memv) ,(rename 'result)
                                                     (,(rename 'quote) ,(caar clauses)))
                                    ,(body (cdar clauses))
                                    ,(each-clause (cdr clauses))))))
      `(,(rename 'let) ((,(rename 'result) ,(cadr form)))
                       ,(each-clause (cddr form))))))

(define-syntax do
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((body `(,(rename 'begin) ,@(cdddr form)
                                     (,(rename 'rec) ,@(map (lambda (x)
                                                              (if (pair? (cddr x))
                                                                  (caddr x)
                                                                  (car x)))
                                                            (cadr form))))))
        `(,(rename 'let) ,(rename 'rec) ,(map (lambda (x)
                                                (list (car x)
                                                      (cadr x)))
                                              (cadr form))
                         ,(if (null? (cdaddr form))
                              `(,(rename 'let) ((,(rename 'it) ,(caaddr form)))
                                               (,(rename 'if) ,(rename 'it)
                                                              ,(rename 'it)
                                                              ,body))
                              `(,(rename 'if) ,(caaddr form)
                                              (,(rename 'begin) ,@(cdaddr form))
                                              ,body)))))))

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

(define call/cc call-with-current-continuation)

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
  (let ((vs (producer)))
     (if (values? vs)
         (apply consumer (cdr vs))
         (consumer vs))))

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

(define (close-port x)
  (cond ((input-port? x) (close-input-port x))
        ((output-port? x) (close-output-port x))
        (else (unspecified))))

(define (read        . x) (%read        (if (pair? x) (car x) (current-input-port))))
(define (read-char   . x) (%read-char   (if (pair? x) (car x) (current-input-port))))
(define (peek-char   . x) (%peek-char   (if (pair? x) (car x) (current-input-port))))
(define (char-ready? . x) (%char-ready? (if (pair? x) (car x) (current-input-port))))

(define (write-simple x . port) (%write-simple x (if (pair? port) (car port) (current-output-port))))
(define (write-char   x . port) (put-char      x (if (pair? port) (car port) (current-output-port))))

(define write write-simple)

(define (display datum . port)
  (cond ((char?   datum) (apply write-char    datum port))
        ((string? datum) (apply write-string  datum port))
        (else            (apply write         datum port))))

(define (newline . port)
  (apply write-char #\newline port))

(define (write-string string . xs)
  (case (length xs)
    ((0)  (put-string string (current-output-port)))
    ((1)  (put-string string (car xs)))
    (else (put-string (apply string-copy string (cadr xs)) (car xs)))))

(define (flush-output-port . port)
  (%flush-output-port (if (pair? port)
                          (car port)
                          (current-output-port))))
