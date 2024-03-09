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
  (import (only (meevax apply) apply)
          (only (meevax boolean) boolean? not)
          (only (meevax character) char? char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char->integer integer->char char-upcase char-downcase)
          (only (meevax comparator) eq? eqv? equal?)
          (only (meevax complex) make-rectangular make-polar real-part imag-part magnitude angle)
          (only (meevax continuation) call-with-current-continuation)
          (only (meevax core) begin define define-syntax if lambda letrec quote set!)
          (only (meevax inexact) exp log sqrt sin cos tan asin acos atan)
          (only (meevax list) null? list? list length append reverse list-tail list-ref memq memv assq assv)
          (only (meevax macro-transformer) er-macro-transformer identifier?)
          (only (meevax map) map)
          (only (meevax number) number? complex? real? rational? integer? exact? inexact? = < > <= >= zero? positive? negative? odd? even? max min + * - / abs quotient remainder modulo gcd lcm numerator denominator floor ceiling truncate round expt exact inexact number->string string->number)
          (only (meevax pair) pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
          (only (meevax port) input-port? output-port? standard-input-port standard-output-port open-input-file open-output-file close eof-object?)
          (only (meevax procedure) procedure?)
          (only (meevax string) string? make-string string string-length string-ref string-set! string=? string<? string>? string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=? string-append string->list list->string string-copy string-fill!)
          (only (meevax symbol) symbol? symbol->string string->symbol)
          (only (meevax vector) vector? make-vector vector vector-length vector-ref vector-set! vector->list list->vector vector-fill!)
          (prefix (meevax read) %)
          (prefix (meevax write) %)
          (prefix (only (meevax environment) load) %)
          (only (srfi 39) make-parameter parameterize)
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
          (rename string-copy substring) string-append string->list
          list->string string-copy string-fill! vector? make-vector vector
          vector-length vector-ref vector-set! vector->list list->vector
          vector-fill! procedure? apply map for-each force
          call-with-current-continuation call-with-input-file
          call-with-output-file input-port? output-port? current-input-port
          current-output-port with-input-from-file with-output-to-file
          open-input-file open-output-file close-input-port close-output-port
          read read-char peek-char eof-object? char-ready? write display
          newline write-char load)

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
                   ((not (< x y)) x)
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

         (define current-input-port
           (make-parameter (standard-input-port)))

         (define current-output-port
           (make-parameter (standard-output-port)))

         (define (with-input-from-file path thunk)
           (parameterize ((current-input-port (open-input-file path)))
             (let ((result (thunk)))
               (close-input-port (current-input-port))
               result)))

         (define (with-output-to-file path thunk)
           (parameterize ((current-output-port (open-output-file path)))
             (let ((result (thunk)))
               (close-output-port (current-output-port))
               result)))

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
