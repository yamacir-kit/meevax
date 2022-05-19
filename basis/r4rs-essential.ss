(define-library (scheme r4rs essential)
  (import (meevax character)
          (meevax control)
          (meevax environment)
          (meevax equivalence)
          (meevax foreign-function)
          (meevax list)
          (meevax number)
          (meevax pair)
          (meevax port)
          (meevax read)
          (meevax string)
          (meevax symbol)
          (meevax syntax)
          (meevax vector)
          (meevax write)
          (srfi 211 explicit-renaming))

  (export quote lambda if set! cond case and or let letrec begin quasiquote
          define not boolean? eqv? eq? equal? pair? cons car cdr set-car!
          set-cdr! caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
          cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
          cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr null? list? list
          length append reverse list-ref memq memv member assq assv assoc
          symbol? symbol->string string->symbol number? complex? real? rational?
          integer? exact? inexact? = < > <= >= zero? positive? negative? odd?
          even? max min + * - / abs quotient remainder modulo gcd lcm floor
          ceiling truncate round number->string string->number char? char=?
          char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=?
          char-ci>=? char-alphabetic? char-numeric? char-whitespace?
          char-upper-case? char-lower-case? char->integer integer->char
          char-upcase char-downcase string? make-string string string-length
          string-ref string-set! string=? string<? string>? string<=? string>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
          substring string-append string->list list->string vector? make-vector
          vector vector-length vector-ref vector-set! vector->list list->vector
          procedure? apply map for-each call-with-current-continuation!
          call-with-input-file call-with-output-file input-port? output-port?
          current-input-port current-output-port open-input-file
          open-output-file close-input-port close-output-port read read-char
          peek-char eof-object? write display newline write-char load)

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

         (define-syntax let ; named-let inessential
           (er-macro-transformer
             (lambda (form rename compare)
               (if (identifier? (cadr form))
                   `(,(rename 'letrec) ((,(cadr form)
                                          (,(rename 'lambda) ,(map car (caddr form)) ,@(cdddr form))))
                                       (,(cadr form) ,@(map cadr (caddr form))))
                   `((,(rename 'lambda) ,(map car (cadr form)) ,@(cddr form))
                     ,@(map cadr (cadr form)))))))

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

         (define (list-ref x k)
           (let list-ref ((x x)
                          (k k))
             (if (zero? k)
                 (car x)
                 (list-ref (cdr x)
                           (- k 1)))))

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
           (<= (char->integer #\a)
               (char->integer (char-downcase x))
               (char->integer #\z)))

         (define (char-numeric? x)
           (<= (char->integer #\0)
               (char->integer x)
               (char->integer #\9)))

         (define (char-whitespace? x)
           (or (eqv? x #\space)
               (eqv? x #\tab)
               (eqv? x #\newline)
               (eqv? x #\return)))

         (define (char-upper-case? x)
           (<= (char->integer #\A)
               (char->integer x)
               (char->integer #\Z)))

         (define (char-lower-case? x)
           (<= (char->integer #\a)
               (char->integer x)
               (char->integer #\z)))

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

         (define (call-with-input-file path f) ; r7rs incompatible (values unsupported)
           (define (call-with-input-port port f)
             (let ((result (f port)))
               (close-input-port port)
               result))
           (call-with-input-port (open-input-file path) f))

         (define (call-with-output-file path f) ; r7rs incompatible (values unsupported)
           (define (call-with-output-port port f)
             (let ((result (f port)))
               (close-output-port port)
               result))
           (call-with-output-port (open-output-file path) f))

         (define current-input-port standard-input-port)  ; r7rs incompatible (current-input-port is standard input)

         (define current-output-port standard-output-port)  ; r7rs incompatible (current-output-port is standard output)

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

         (define (write x . port)
           (%write-simple x (if (pair? port)
                                (car port)
                                (current-output-port))))

         (define (write-char x . port)
           (put-char x (if (pair? port)
                           (car port)
                           (current-output-port))))

         (define (write-string string . xs) ; TODO REMOVE!
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

         (define (load filename . environment)
           (%load (if (pair? environment)
                      (car environment)
                      (interaction-environment))
                  filename))
         )
  )
