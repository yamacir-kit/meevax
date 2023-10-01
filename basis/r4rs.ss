(define-library (scheme r4rs)
  (import (meevax inexact)
          (only (meevax core) define-syntax)
          (only (meevax list) list-tail)
          (only (meevax macro-transformer) er-macro-transformer)
          (only (meevax number) exact-integer? expt exact inexact ratio? ratio-numerator ratio-denominator)
          (prefix (meevax port) %)
          (prefix (meevax read) %)
          (only (meevax string) string-copy)
          (only (meevax vector) vector-fill!)
          (scheme r4rs essential)
          (srfi 45))

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

  #|
     This library contains many procedure and syntax definitions copied from
     Chibi-Scheme's script lib/init-7.scm. The definitions marked
     "Chibi-Scheme" in this file are those. Such definitions are subject to the
     following Chibi-Scheme license.

     ---

     Copyright (c) 2009-2021 Alex Shinn
     All rights reserved.

     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions are
     met:
     1. Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
     3. The name of the author may not be used to endorse or promote products
        derived from this software without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
     EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
     TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
     PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  |#

  (begin (define-syntax let*
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

         (define (numerator x) ; Chibi-Scheme
           (cond ((ratio? x) (ratio-numerator x))
                 ((exact? x) x)
                 (else (inexact (numerator (exact x))))))

         (define (denominator x) ; Chibi-Scheme
           (cond ((ratio? x) (ratio-denominator x))
                 ((exact? x) 1)
                 ((integer? x) 1.0)
                 (else (inexact (denominator (exact x))))))

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

         (define (make-rectangular x y) ; Chibi-Scheme
           (+ x (* y (sqrt -1))))

         (define (make-polar radius phi) ; Chibi-Scheme
           (make-rectangular (* radius (cos phi))
                             (* radius (sin phi))))

         (define (real-part z)
           (if (imaginary? z) (car z) z))

         (define (imag-part z)
           (if (imaginary? z) (cdr z) 0))

         (define (magnitude z) ; Chibi-Scheme
           (sqrt (+ (square (real-part z))
                    (square (imag-part z)))))

         (define (angle z) ; Chibi-Scheme
           (atan (imag-part z)
                 (real-part z)))

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

         (define %current-input-port (%standard-input-port))

         (define (current-input-port)
           %current-input-port)

         (define %current-output-port (%standard-output-port))

         (define (current-output-port)
           %current-output-port)

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

         (define (char-ready? . xs)
           (%get-char-ready? (if (pair? xs)
                                 (car xs)
                                 (current-input-port))))))
