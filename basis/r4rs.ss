#|
   This library contains many procedure and syntax definitions copied from
   Chibi-Scheme's script lib/init-7.scm. The definitions marked "Chibi-Scheme"
   in this file are those. Such definitions are subject to the following
   Chibi-Scheme license.

   ---

   Copyright (c) 2009-2021 Alex Shinn
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

   3. The name of the author may not be used to endorse or promote products
      derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
   EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
   OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(define-library (scheme r4rs)
  (import (only (meevax boolean) boolean? not)
          (meevax character)
          (only (meevax core) begin define define-syntax if lambda letrec quote set!)
          (only (meevax comparator) eq? eqv? equal?)
          (only (meevax complex) make-rectangular make-polar real-part imag-part magnitude angle)
          (only (meevax continuation) call-with-current-continuation)
          (prefix (only (meevax environment) load) %)
          (only (meevax function) procedure?)
          (only (meevax inexact) exp log sqrt sin cos tan asin acos atan)
          (meevax list)
          (only (meevax macro-transformer) er-macro-transformer identifier?)
          (only (meevax number)
                number? complex? real? rational? integer?
                exact? inexact?
                = < > <= >=
                + * - /
                abs
                % ; deprecated
                numerator denominator
                floor ceiling truncate round
                expt
                exact inexact
                string->number

                ; to be removed
                exact-integer? ; r7rs
                )
          (meevax pair)
          (meevax port)
          (prefix (meevax read) %)
          (meevax string)
          (meevax symbol)
          (meevax vector)
          (prefix (meevax write) %)
          (only (srfi 45) delay force))

  (export quote lambda if set! cond case and or let let* letrec begin do delay
          quasiquote define not boolean? eqv? eq? equal? pair? cons car cdr
          set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar caddr cdaar
          cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar
          cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr null?
          list? list length append reverse list-tail list-ref memq memv member
          assq assv assoc symbol? symbol->string string->symbol number?
          complex? real? rational? integer? exact? inexact? = < > <= >= zero?
          positive? negative? odd? even? max min + * - / abs quotient remainder
          modulo gcd lcm numerator denominator floor ceiling truncate round
          rationalize exp log sin cos tan asin acos atan sqrt expt
          make-rectangular make-polar real-part imag-part magnitude angle
          (rename inexact exact->inexact) (rename exact inexact->exact)
          number->string string->number char? char=? char<? char>? char<=?
          char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
          char-alphabetic? char-numeric? char-whitespace? char-upper-case?
          char-lower-case? char->integer integer->char char-upcase
          char-downcase string? make-string string string-length string-ref
          string-set! string=? string<? string>? string<=? string>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
          substring string-append string->list list->string string-copy
          string-fill! vector? make-vector vector vector-length vector-ref
          vector-set! vector->list list->vector vector-fill! procedure? apply
          map for-each force call-with-current-continuation
          call-with-input-file call-with-output-file input-port? output-port?
          current-input-port current-output-port with-input-from-file
          with-output-to-file open-input-file open-output-file close-input-port
          close-output-port read read-char peek-char eof-object? char-ready?
          write display newline write-char load)

  (begin (define-syntax cond ; Chibi-Scheme
           (er-macro-transformer
             (lambda (form rename compare)
               (if (null? (cdr form))
                   (if #f #f)
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

         (define-syntax and ; Chibi-Scheme
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

         (define-syntax or ; Chibi-Scheme
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

         (define-syntax quasiquote ; Chibi-Scheme
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

         (define (every f xs)
           (if (pair? xs)
               (and (f (car xs))
                    (every f (cdr xs)))
               #t))

         (define (map f x . xs) ; Chibi-Scheme
           (define (map f x a)
             (if (pair? x)
                 (map f
                      (cdr x)
                      (cons (f (car x)) a))
                 (reverse a)))
           (define (map* f xs a)
             (if (every pair? xs)
                 (map* f
                       (map cdr xs '())
                       (cons (apply f (map car xs '())) a))
                 (reverse a)))
           (if (null? xs)
               (map f x '())
               (map* f (cons x xs) '())))

         (define (apply f x . xs) ; Chibi-Scheme
           (letrec ((apply (lambda (f xs)
                             (f . xs))))
             (if (null? xs)
                 (apply f x)
                 ((lambda (xs)
                    (apply f (append (reverse (cdr xs))
                                     (car xs))))
                  (reverse (cons x xs))))))

         (define-syntax let ; Chibi-Scheme
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

         (define-syntax do ; Chibi-Scheme
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

         (define-syntax case ; Chibi-Scheme
           (er-macro-transformer
             (lambda (form rename compare)
               (define (body xs)
                 (cond ((null? xs) (rename 'result))
                       ((compare (rename '=>) (car xs)) `(,(cadr xs) ,(rename 'result)))
                       (else `(,(rename 'begin) ,@xs))))
               (define (each-clause clauses)
                 (cond ((null? clauses)
                        (if #f #f))
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

         (define (member x xs . compare) ; Chibi-Scheme
           (let ((compare (if (pair? compare) (car compare) equal?)))
             (let member ((xs xs))
               (and (pair? xs)
                    (if (compare x (car xs)) xs
                        (member (cdr xs)))))))

         (define (assoc key alist . compare) ; Chibi-Scheme
           (let ((compare (if (pair? compare) (car compare) equal?)))
             (let assoc ((alist alist))
               (if (null? alist) #f
                   (if (compare key (caar alist))
                       (car alist)
                       (assoc (cdr alist)))))))

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

         (define (max x . xs) ; Chibi-Scheme
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

         (define (min x . xs) ; Chibi-Scheme
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

         (define (quotient x y)
           (truncate (/ x y)))

         (define remainder %)

         (define (modulo x y)
           (% (+ y (% x y)) y))

         (define (gcd . xs) ; Chibi-Scheme
           (define (gcd-2 a b)
             (if (zero? b)
                 (abs a)
                 (gcd b (remainder a b))))
           (if (null? xs) 0
               (let rec ((n  (car xs))
                         (ns (cdr xs)))
                 (if (null? ns) n
                     (rec (gcd-2 n (car ns)) (cdr ns))))))

         (define (lcm . xs) ; Chibi-Scheme
           (define (lcm-2 a b)
             (abs (quotient (* a b) (gcd a b))))
           (if (null? xs) 1
               (let rec ((n  (car xs))
                         (ns (cdr xs)))
                 (if (null? ns) n
                     (rec (lcm-2 n (car ns)) (cdr ns))))))

         (define (rationalize x e) ; IEEE Std 1178-1990 ANNEX C.4
           (define (simplest-rational x y)
             (define (simplest-rational-internal x y)
               (let ((fx (floor x))
                     (fy (floor y)))
                 (cond ((not (< fx x)) fx)
                       ((= fx fy)
                        (+ fx
                           (/ (simplest-rational-internal (/ (- y fy))
                                                          (/ (- x fx))))))
                       (else (+ 1 fx)))))
             (cond ((< y x)
                    (simplest-rational y x))
                   ((not (< x y))
                    (if (rational? x) x (error x)))
                   ((positive? x)
                    (simplest-rational-internal x y))
                   ((negative? x)
                    (- (simplest-rational-internal (- y)
                                                   (- x))))
                   (else (if (and (exact? x)
                                  (exact? y))
                             0
                             0.0))))
           (simplest-rational (- x e)
                              (+ x e)))

         (define (char-compare x xs compare) ; Chibi-Scheme
           (let rec ((compare compare)
                     (lhs (char->integer x))
                     (xs xs))
             (if (null? xs) #t
                 (let ((rhs (char->integer (car xs))))
                   (and (compare lhs rhs)
                        (rec compare rhs (cdr xs)))))))

         (define (char=? x . xs) ; Chibi-Scheme
           (char-compare x xs =))

         (define (char<? x . xs) ; Chibi-Scheme
           (char-compare x xs <))

         (define (char>? x . xs) ; Chibi-Scheme
           (char-compare x xs >))

         (define (char<=? x . xs) ; Chibi-Scheme
           (char-compare x xs <=))

         (define (char>=? x . xs) ; Chibi-Scheme
           (char-compare x xs >=))

         (define (char-ci-compare x xs compare) ; Chibi-Scheme
           (let rec ((compare compare)
                     (lhs (char->integer (char-downcase x)))
                     (xs xs))
             (if (null? xs) #t
                 (let ((rhs (char->integer (char-downcase (car xs)))))
                   (and (compare lhs rhs)
                        (rec compare rhs (cdr xs)))))))

         (define (char-ci=? x . xs) ; Chibi-Scheme
           (char-ci-compare x xs =))

         (define (char-ci<? x . xs) ; Chibi-Scheme
           (char-ci-compare x xs <))

         (define (char-ci>? x . xs) ; Chibi-Scheme
           (char-ci-compare x xs >))

         (define (char-ci<=? x . xs) ; Chibi-Scheme
           (char-ci-compare x xs <=))

         (define (char-ci>=? x . xs) ; Chibi-Scheme
           (char-ci-compare x xs >=))

         (define (string . xs) ; Chibi-Scheme
           (list->string xs))

         (define (string-map f x . xs) ; R7RS
           (if (null? xs)
               (list->string (map f (string->list x)))
               (list->string (apply map f (map string->list (cons x xs))))))

         (define (string-foldcase s) ; R7RS
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

         (define (string-fill! s c . o) ; Chibi-Scheme
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

         (define (for-each f x . xs) ; Chibi-Scheme
           (if (null? xs)
               (letrec ((for-each (lambda (f x)
                                    (if (pair? x)
                                        (begin (f (car x))
                                               (for-each f (cdr x)))))))
                 (for-each f x))
               (begin (apply map f x xs)
                      (if #f #f))))

         (define (call-with-input-file path f) ; R7RS incompatible (values unsupported)
           (define (call-with-input-port port f)
             (let ((result (f port)))
               (close-input-port port)
               result))
           (call-with-input-port (open-input-file path) f))

         (define (call-with-output-file path f) ; R7RS incompatible (values unsupported)
           (define (call-with-output-port port f)
             (let ((result (f port)))
               (close-output-port port)
               result))
           (call-with-output-port (open-output-file path) f))

         (define %current-input-port (standard-input-port))

         (define (current-input-port) %current-input-port)

         (define %current-output-port (standard-output-port))

         (define (current-output-port) %current-output-port)

         (define (with-input-from-file path thunk)
           (let ((previous-input-port (current-input-port)))
             (set! %current-input-port (open-input-file path))
             (thunk)
             (set! %current-input-port previous-input-port)))

         (define (with-output-to-file path thunk)
           (let ((previous-output-port (current-output-port)))
             (set! %current-output-port (open-output-file path))
             (thunk)
             (set! %current-output-port previous-output-port)))

         (define close-input-port close)

         (define close-output-port close)

         (define (read . xs)
           (%read (if (pair? xs)
                      (car xs)
                      (current-input-port))))

         (define (read-char . xs)
           (%get-char (if (pair? xs)
                          (car xs)
                          (current-input-port))))

         (define (peek-char . xs)
           (%peek-char (if (pair? xs)
                           (car xs)
                           (current-input-port))))

         (define (char-ready? . xs)
           (%get-char-ready? (if (pair? xs)
                                 (car xs)
                                 (current-input-port))))

         (define (write x . port)
           (%write x (if (pair? port)
                         (car port)
                         (current-output-port))))

         (define (write-char x . port)
           (%put-char x (if (pair? port)
                            (car port)
                            (current-output-port))))

         (define (display x . xs)
           (cond ((char? x)
                  (apply write-char x xs))
                 ((string? x)
                  (%put-string x (if (pair? xs) ; NOTE: The procedure write-string is not defined in R4RS.
                                     (car xs)
                                     (current-output-port))))
                 (else (apply write x xs))))

         (define (newline . port)
           (apply write-char #\newline port))

         (define (load filename . xs)
           (%load (if (pair? xs)
                      (car xs)
                      (%interaction-environment))
                  filename))))
