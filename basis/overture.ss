(define-library (srfi 211 syntactic-closures)
  (import (meevax macro)
          (meevax syntax))

  (begin (define (sc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure mac-env '() (f form use-env))))

         (define (rsc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure use-env '() (f form mac-env)))))

  (export sc-macro-transformer
          rsc-macro-transformer
          make-syntactic-closure
          identifier?))

(define-library (srfi 211 explicit-renaming)
  (import (meevax equivalence)
          (meevax list)
          (meevax macro)
          (meevax pair)
          (meevax syntax))

  (begin (define (er-macro-transformer f)
           (lambda (form use-env mac-env)
             (define renames '())
             (define (rename x)
               (letrec ((assq (lambda (x alist)
                                (if (null? alist) #f
                                    (if (eq? x (caar alist))
                                        (car alist)
                                        (assq x (cdr alist))))))
                        (alist-cons (lambda (key x alist)
                                      (cons (cons key x) alist))))
                 (define key/value (assq x renames))
                 (if key/value
                     (cdr key/value)
                     (begin (set! renames (alist-cons x (make-syntactic-closure mac-env '() x) renames))
                            (cdar renames)))))
             (define (compare x y)
               (eqv? (if (syntactic-closure? x) x
                         (make-syntactic-closure use-env '() x))
                     (if (syntactic-closure? y) y
                         (make-syntactic-closure use-env '() y))))
             (f form rename compare))))

  (export er-macro-transformer
          identifier?))

(define-library (scheme lazy)
  (import
          (meevax equivalence)
          (meevax pair)
          (meevax syntax)
          (srfi 211 explicit-renaming)
          )

  (begin (define (list . xs) xs)

         (define (not x)
           (if x #f #t)))

  (begin (define <promise> (list 'promise))

         (define (promise done? value)
           (cons <promise> (cons done? value)))

         (define (promise? x)
           (if (pair? x)
               (eq? <promise> (car x))
               #f))

         (define promise-done? cadr)

         (define promise-value cddr)

         (define (promise-update! new old)
           (set-car! (cdr old) (promise-done? new))
           (set-cdr! (cdr old) (promise-value new))
           (set-car! new (cdr old)))

         (define (force promise)
           (if (promise-done? promise)
               (promise-value promise)
               ((lambda (promise*)
                  (if (not (promise-done? promise))
                      (promise-update! promise* promise))
                  (force promise))
                ((promise-value promise)))))

         (define-syntax delay-force
           (er-macro-transformer
             (lambda (form rename compare)
               (list (rename 'promise) #f (list (rename 'lambda) '() (cadr form))))))

         (define-syntax delay
           (er-macro-transformer
             (lambda (form rename compare)
               (list (rename 'delay-force) (list (rename 'promise) #t (cadr form))))))

         (define (make-promise x)
           (if (promise? x) x
               (delay x)))
         )

  (export delay delay-force force make-promise promise?))

(define-library (scheme r4rs)
  (import (meevax character)
          (meevax control)
          (meevax equivalence)
          (meevax inexact)
          (meevax list)
          (meevax number)
          (meevax pair)
          (meevax port)
          (meevax string)
          (meevax symbol)
          (meevax syntax)
          (meevax vector)
          (meevax write)
          (srfi 211 explicit-renaming))

  (begin (define (unspecified) (if #f #f))

         (define (list . xs) xs)

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

         (define (reverse xs)
           (if (null? xs) '()
               (append (reverse (cdr xs))
                       (list (car xs)))))

         (define (map f x . xs)
           (define (map-1 f x stack)
             (if (pair? x)
                 (map-1 f
                        (cdr x)
                        (cons (f (car x)) stack))
                 (reverse stack)))
           (if (null? xs)
               (map-1 f x '())
               (letrec ((map (lambda (f xs stack)
                               (if (every pair? xs)
                                   (map f
                                        (map-1 cdr xs '())
                                        (cons (apply f (map-1 car xs '())) stack))
                                   (reverse stack)))))
                 (map f (cons x xs) '()))))

         (define (apply f x . xs)
           (define (apply-1 f xs) (f . xs))
           (if (null? xs)
               (apply-1 f x)
               ((lambda (rxs)
                  (apply-1 f
                           (append (reverse (cdr rxs))
                                   (car rxs))))
                (reverse (cons x xs)))))

         (define (every f x . xs) ; TODO REMOVE
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

         (define (any f x . xs) ; TODO REMOVE
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

         (define (not x)
           (if x #f #t))

         (define (boolean? x)
           (or (eqv? x #t)
               (eqv? x #f)))

         (define (equal? x y)
           (if (and (pair? x)
                    (pair? y))
               (and (equal? (car x)
                            (car y))
                    (equal? (cdr x)
                            (cdr y)))
               (eqv? x y)))

         (define (list? x)
           (let list? ((x x)
                       (lag x))
             (if (pair? x)
                 (let ((x (cdr x)))
                   (if (pair? x)
                       (let ((x (cdr x))
                             (lag (cdr lag)))
                         (and (not (eq? x lag))
                              (list? x lag)))
                       (null? x)))
                 (null? x))))

         (define (length x)
           (let length ((x x)
                        (k 0))
             (if (pair? x)
                 (length (cdr x)
                         (+ k 1))
                 k)))

         (define (list-tail x k)
           (let list-tail ((x x)
                           (k k))
             (if (zero? k) x
                 (list-tail (cdr x)
                            (- k 1)))))

         (define (list-ref x k)
           (car (list-tail x k)))

         (define (member o x . c)
           (let ((compare (if (pair? c) (car c) equal?)))
             (let member ((x x))
               (and (pair? x)
                    (if (compare o (car x)) x
                        (member (cdr x)))))))

         (define (memq o x)
           (member o x eq?))

         (define (memv o x)
           (member o x eqv?))

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

         (define (assoc key alist . compare)
           (let ((compare (if (pair? compare)
                              (car compare)
                              equal?)))
             (let assoc ((alist alist))
               (if (null? alist) #f
                   (if (compare key (caar alist))
                       (car alist)
                       (assoc (cdr alist)))))))

         (define (assq key alist)
           (assoc key alist eq?))

         (define (assv key alist)
           (assoc key alist eqv?))

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

         (define (zero? n)
           (= n 0))

         (define (positive? n)
           (> n 0))

         (define (negative? n)
           (< n 0))

         (define (odd? n)
           (not (even? n)))

         (define (even? n)
           (= (remainder n 2) 0))

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

         (define (quotient x y)
           (truncate (/ x y)))

         (define remainder %)

         (define (modulo x y)
           (% (+ y (% x y)) y))

         (define (gcd . xs)
           (define (gcd-2 a b)
             (if (zero? b)
                 (abs a)
                 (gcd b (remainder a b))))
           (if (null? xs) 0
               (let rec ((n  (car xs))
                         (ns (cdr xs)))
                 (if (null? ns) n
                     (rec (gcd-2 n (car ns)) (cdr ns))))))

         (define (lcm . xs)
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

         (define (make-rectangular x y)
           (+ x (* y (sqrt -1))))

         (define (make-polar radius phi)
           (make-rectangular (* radius (cos phi))
                             (* radius (sin phi))))

         (define (real-part z)
           (if (%complex? z) (car z) z))

         (define (imag-part z)
           (if (%complex? z) (cdr z) 0))

         (define (magnitude z)
           (sqrt (+ (square (real-part z))
                    (square (imag-part z)))))

         (define (angle z)
           (atan (imag-part z)
                 (real-part z)))

         (define inexact->exact exact)

         (define exact->inexact inexact)

         (define (char-compare x xs compare)
           (let rec ((compare compare)
                     (lhs (char->integer x))
                     (xs xs))
             (if (null? xs) #t
                 (let ((rhs (char->integer (car xs))))
                   (and (compare lhs rhs)
                        (rec compare rhs (cdr xs)))))))

         (define (char=? x . xs)
           (char-compare x xs =))

         (define (char<? x . xs)
           (char-compare x xs <))

         (define (char>? x . xs)
           (char-compare x xs >))

         (define (char<=? x . xs)
           (char-compare x xs <=))

         (define (char>=? x . xs)
           (char-compare x xs >=))

         (define (char-ci-compare x xs compare)
           (let rec ((compare compare)
                     (lhs (char->integer (char-downcase x)))
                     (xs xs))
             (if (null? xs) #t
                 (let ((rhs (char->integer (char-downcase (car xs)))))
                   (and (compare lhs rhs)
                        (rec compare rhs (cdr xs)))))))

         (define (char-ci=? x . xs)
           (char-ci-compare x xs =))

         (define (char-ci<? x . xs)
           (char-ci-compare x xs <))

         (define (char-ci>? x . xs)
           (char-ci-compare x xs >))

         (define (char-ci<=? x . xs)
           (char-ci-compare x xs <=))

         (define (char-ci>=? x . xs)
           (char-ci-compare x xs >=))

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

         (define (string . xs)
           (list->string xs))

         (define (string-map f x . xs) ; r7rs
           (define (string-map-1 x)
             (list->string
               (map f (string->list x))))
           (define (string-map-n xs)
             (map list->string
                  (map (lambda (c) (map f c))
                       (map string->list xs))))
           (if (null? xs)
               (string-map-1 x)
               (string-map-n (cons x xs))))

         (define (string-foldcase s) ; r7rs
           (string-map char-downcase s))

         (define (string-ci=? . xs)
           (apply string=? (map string-foldcase xs)))

         (define (string-ci<? . xs)
           (apply string<? (map string-foldcase xs)))

         (define (string-ci>? . xs)
           (apply string>? (map string-foldcase xs)))

         (define (string-ci<=? . xs)
           (apply string<=? (map string-foldcase xs)))

         (define (string-ci>=? . xs)
           (apply string>=? (map string-foldcase xs)))

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

         (define (procedure? x)
           (or (closure? x)
               (continuation? x)
               (foreign-function? x)))

         (define (for-each f x . xs)
           (if (null? xs)
               (letrec ((for-each (lambda (f x)
                                    (if (pair? x)
                                        (begin (f (car x))
                                               (for-each f (cdr x)))))))
                 (for-each f x))
               (begin (apply map f x xs)
                      (if #f #f))))

         (define (call-with-input-file path f)
           (define (call-with-input-port port f)
             (let ((result (f port)))
               (close-input-port port)
               result))
           (call-with-input-port (open-input-file path) f))

         (define (call-with-output-file path f)
           (define (call-with-output-port port f)
             (let ((result (f port)))
               (close-output-port port)
               result))
           (call-with-output-port (open-output-file path) f))

         (define current-input-port standard-input-port)

         (define current-output-port standard-output-port)

         (define (read . port)
           (%read (if (pair? port)
                      (car port)
                      (current-input-port))))

         (define (read-char . port)
           (%read-char (if (pair? port)
                           (car port)
                           (current-input-port))))

         (define (peek-char . port)
           (%peek-char (if (pair? port)
                           (car port)
                           (current-input-port))))

         (define (char-ready? . port)
           (%char-ready? (if (pair? port)
                             (car port)
                             (current-input-port))))

         (define (write x . port)
           (%write-simple x (if (pair? port)
                                (car port)
                                (current-output-port))))

         (define (write-char x . port)
           (put-char x (if (pair? port)
                           (car port)
                           (current-output-port))))

         (define (write-string string . xs)
           (case (length xs)
             ((0)  (put-string string (current-output-port)))
             ((1)  (put-string string (car xs)))
             (else (put-string (apply string-copy string (cadr xs)) (car xs)))))

         (define (display datum . port)
           (cond ((char?   datum) (apply write-char    datum port))
                 ((string? datum) (apply write-string  datum port))
                 (else            (apply write         datum port))))

         (define (newline . port)
           (apply write-char #\newline port))

         )

  (export quote
          lambda
          if
          set!
          cond
          case
          and
          or
          let ; named-let inessential
          let* ; inessential
          letrec
          begin
          do ; inessential
          ; delay ; inessential
          quasiquote
          define
          not
          boolean?
          eqv?
          eq?
          equal?
          pair?
          cons
          car
          cdr
          set-car!
          set-cdr!
          caar
          cadr
          cdar
          cddr
          caaar
          caadr
          cadar
          caddr
          cdaar
          cdadr
          cddar
          cdddr
          caaaar
          caaadr
          caadar
          caaddr
          cadaar
          cadadr
          caddar
          cadddr
          cdaaar
          cdaadr
          cdadar
          cdaddr
          cddaar
          cddadr
          cdddar
          cddddr
          null?
          list?
          list
          length
          append
          reverse
          list-tail ; inessential
          list-ref
          memq
          memv
          member
          assq
          assv
          assoc
          symbol?
          symbol->string
          string->symbol
          number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          =
          <
          >
          <=
          >=
          zero?
          positive?
          negative?
          odd?
          even?
          max
          min
          +
          *
          -
          /
          abs
          quotient
          remainder
          modulo
          gcd
          lcm
          numerator ; inessential
          denominator ; inessential
          floor
          ceiling
          truncate
          round
          rationalize ; inessential
          exp ; inessential
          log ; inessential
          sin ; inessential
          cos ; inessential
          tan ; inessential
          asin ; inessential
          acos ; inessential
          atan ; inessential
          sqrt ; inessential
          expt ; inessential
          make-rectangular ; inessential
          make-polar ; inessential
          real-part ; inessential
          imag-part ; inessential
          magnitude ; inessential
          angle ; inessential
          exact->inexact ; inessential
          inexact->exact ; inessential
          number->string
          string->number
          char?
          char=?
          char<?
          char>?
          char<=?
          char>=?
          char-ci=?
          char-ci<?
          char-ci>?
          char-ci<=?
          char-ci>=?
          char-alphabetic?
          char-numeric?
          char-whitespace?
          char-upper-case?
          char-lower-case?
          char->integer
          integer->char
          char-upcase
          char-downcase
          string?
          make-string
          string
          string-length
          string-ref
          string-set!
          string=?
          string<?
          string>?
          string<=?
          string>=?
          string-ci=?
          string-ci<?
          string-ci>?
          string-ci<=?
          string-ci>=?
          substring
          string-append
          string->list
          list->string
          string-copy ; inessential
          string-fill! ; inessential
          vector?
          make-vector
          vector
          vector-length
          vector-ref
          vector-set!
          vector->list
          list->vector
          vector-fill! ; inessential
          procedure?
          apply
          map
          for-each
          ; force ; inessential
          call-with-current-continuation! ; A version that does not consider dynamic-wind.
          call-with-input-file ; r7rs incompatible (values unsupported)
          call-with-output-file ; r7rs incompatible (values unsupported)
          input-port?
          output-port?
          current-input-port ; r7rs incompatible (current-input-port is standard input)
          current-output-port ; r7rs incompatible (current-output-port is standard output)
          ; with-input-from-file ; inessential
          ; with-output-to-file ; inessential
          open-input-file
          open-output-file
          close-input-port
          close-output-port
          read
          read-char
          peek-char
          eof-object?
          char-ready? ; inessential
          write
          display
          newline
          write-char
          ; load
          )
  )

(define-library (scheme base)
  (import (meevax character) ; for digit-value
          (meevax number) ; for exact-integer?
          (meevax syntax) ; for quote-syntax
          (scheme r4rs)
          (srfi 211 explicit-renaming)
          )
  (begin (define (unspecified) (if #f #f))

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

         (define-syntax letrec*
           (er-macro-transformer
             (lambda (form rename compare)
               `(,(rename 'let) ()
                                ,@(map (lambda (x) (cons (rename 'define) x))
                                       (cadr form))
                                ,@(cddr form)))))

         (define (floor-quotient x y)
           (floor (/ x y)))

         (define floor-remainder modulo)

         (define (floor/ x y)
           (values (floor-quotient x y)
                   (floor-remainder x y)))

         (define truncate-quotient quotient)

         (define truncate-remainder remainder)

         (define (truncate/ x y)
           (values (truncate-quotient x y)
                   (truncate-remainder x y)))

         (define (square z) (* z z))

         (define inexact exact->inexact)

         (define exact inexact->exact)

         (define boolean=? eqv?)

         (define (list-set! x k object)
           (set-car! (list-tail x k) object))

         (define symbol=? eqv?)

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

         )
  (export *
          +
          -
          ; ...
          /
          <
          <=
          =
          ; =>
          >
          >=
          ; _
          abs
          and
          append
          apply
          assoc
          assq
          assv
          begin
          ; binary-port?
          boolean=?
          boolean?
          ; bytevector
          ; bytevector-append
          ; bytevector-copy
          ; bytevector-copy!
          ; bytevector-length
          ; bytevector-u8-ref
          ; bytevector-u8-set!
          ; bytevector?
          caar
          cadr
          call-with-current-continuation
          ; call-with-port
          call-with-values
          call/cc
          car
          case
          cdar
          cddr
          cdr
          ceiling
          char->integer
          ; char-ready?
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          ; close-input-port
          ; close-output-port
          ; close-port
          complex?
          cond
          ; cond-expand
          cons
          ; current-error-port
          ; current-input-port
          ; current-output-port
          define
          ; define-record-type
          define-syntax
          ; define-values
          denominator
          do
          dynamic-wind
          ; else
          ; eof-object
          ; eof-object?
          eq?
          equal?
          eqv?
          ; error
          ; error-object-irritants
          ; error-object-message
          ; error-object?
          even?
          exact
          ; exact-integer-sqrt
          exact-integer?
          exact?
          expt
          ; features
          ; file-error?
          floor
          floor-quotient
          floor-remainder
          floor/
          ; flush-output-port
          for-each
          gcd
          ; get-output-bytevector
          ; get-output-string
          ; guard
          if
          ; include
          ; include-ci
          inexact
          inexact?
          ; input-port-open?
          ; input-port?
          integer->char
          integer?
          lambda
          lcm
          length
          let
          let*
          ; let*-values
          let-syntax
          ; let-values
          letrec
          letrec*
          letrec-syntax
          list
          list->string
          list->vector
          ; list-copy
          list-ref
          list-set!
          list-tail
          list?
          ; make-bytevector
          ; make-list
          ; make-parameter
          make-string
          make-vector
          map
          max
          member
          memq
          memv
          min
          modulo
          negative?
          ; newline
          not
          null?
          number->string
          number?
          numerator
          odd?
          ; open-input-bytevector
          ; open-input-string
          ; open-output-bytevector
          ; open-output-string
          or
          ; output-port-open?
          ; output-port?
          pair?
          ; parameterize
          ; peek-char
          ; peek-u8
          ; port?
          positive?
          procedure?
          quasiquote
          quote
          quotient
          ; raise
          ; raise-continuable
          rational?
          rationalize
          ; read-bytevector
          ; read-bytevector!
          ; read-char
          ; read-error?
          ; read-line
          ; read-string
          ; read-u8
          real?
          remainder
          reverse
          round
          set!
          set-car!
          set-cdr!
          square
          string
          string->list
          string->number
          string->symbol
          ; string->utf8
          ; string->vector
          string-append
          string-copy
          ; string-copy!
          string-fill!
          ; string-for-each
          string-length
          ; string-map
          string-ref
          string-set!
          string<=?
          string<?
          string=?
          string>=?
          string>?
          string?
          substring
          symbol->string
          ; symbol=?
          symbol?
          ; syntax-error
          ; syntax-rules
          ; textual-port?
          truncate
          truncate-quotient
          truncate-remainder
          truncate/
          ; u8-ready?
          unless
          ; unquote
          ; unquote-splicing
          ; utf8->string
          values
          vector
          vector->list
          vector->string
          ; vector-append
          ; vector-copy
          ; vector-copy!
          vector-fill!
          ; vector-for-each
          vector-length
          ; vector-map
          vector-ref
          vector-set!
          vector?
          when
          ; with-exception-handler
          ; write-bytevector
          ; write-char
          ; write-string
          ; write-u8
          zero?
          )
  )

(define-library (scheme cxr)
  (import (meevax pair))
  (export caaar caadr cadar caddr
          cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr
          cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr
          cddaar cddadr cdddar cddddr))

(import (scheme r4rs)
        (scheme base)
        (scheme cxr)
        (srfi 211 explicit-renaming)
        (srfi 211 syntactic-closures)
        )

(define (unspecified) (if #f #f))

(define (traditional-macro-transformer f)
  (lambda (form use-env mac-env)
    (apply f (cdr form))))

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
