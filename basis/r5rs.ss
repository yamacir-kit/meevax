(define-library (scheme r5rs continuation)
  (import (meevax context)
          (only (meevax dynamic-environment) load-auxiliary store-auxiliary)
          (only (meevax syntax) define-syntax call-with-current-continuation!)
          (except (scheme r4rs) call-with-current-continuation))

  (export call-with-current-continuation dynamic-wind exit)

  ; https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html

  (begin (define (current-dynamic-extents)
           (load-auxiliary 0))

         (define (install-dynamic-extents! extents)
           (store-auxiliary 0 extents))

         (define (dynamic-wind before thunk after)
           (before)
           (install-dynamic-extents! (cons (cons before after)
                                           (current-dynamic-extents)))
           ((lambda (result) ; TODO let-values
              (install-dynamic-extents! (cdr (current-dynamic-extents)))
              (after)
              result) ; TODO (apply values result)
            (thunk)))

         (define (call-with-current-continuation procedure)
           (define (windup! from to)
             (install-dynamic-extents! from)
             (cond ((eq? from to))
                   ((null? from) (windup! from (cdr to)) ((caar to)))
                   ((null? to) ((cdar from)) (windup! (cdr from) to))
                   (else ((cdar from)) (windup! (cdr from) (cdr to)) ((caar to))))
             (install-dynamic-extents! to))
           (let ((dynamic-extents (current-dynamic-extents)))
             (call-with-current-continuation!
               (lambda (continue)
                 (procedure (lambda (x)
                              (windup! (current-dynamic-extents) dynamic-extents)
                              (continue x)))))))

         (define (exit . normally?)
           (for-each (lambda (before/after)
                       ((cdr before/after)))
                     (current-dynamic-extents))
           (apply emergency-exit normally?))))

(define-library (scheme r5rs)
  (import (only (meevax environment) environment)
          (only (meevax evaluate) eval)
          (only (meevax syntax) define-syntax let-syntax letrec-syntax)
          (except (scheme r4rs) call-with-current-continuation)
          (except (scheme r5rs continuation) exit)
          (srfi 149))

  (export quote lambda if set! cond case and or let let* letrec begin do delay
          quasiquote let-syntax letrec-syntax syntax-rules define define-syntax
          eqv? eq? equal? number? complex? real? rational? integer? exact?
          inexact? = < > <= >= zero? positive? negative? odd? even? max min + *
          - / abs quotient remainder modulo gcd lcm numerator denominator floor
          ceiling truncate round rationalize exp log sin cos tan asin acos atan
          sqrt expt make-rectangular make-polar real-part imag-part magnitude
          angle exact->inexact inexact->exact number->string string->number not
          boolean? pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar
          caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar
          cddadr cdddar cddddr null? list? list length append reverse list-tail
          list-ref memq memv member assq assv assoc symbol? symbol->string
          string->symbol char? char=? char<? char>? char<=? char>=? char-ci=?
          char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic?
          char-numeric? char-whitespace? char-upper-case? char-lower-case?
          char->integer integer->char char-upcase char-downcase string?
          make-string string string-length string-ref string-set! string=?
          string<? string>? string<=? string>=? string-ci=? string-ci<?
          string-ci>? string-ci<=? string-ci>=? substring string-append
          string->list list->string string-copy string-fill! vector? make-vector
          vector vector-length vector-ref vector-set! vector->list list->vector
          vector-fill! procedure? apply map for-each force
          call-with-current-continuation values call-with-values dynamic-wind
          eval scheme-report-environment null-environment
          interaction-environment call-with-input-file call-with-output-file
          input-port? output-port? current-input-port current-output-port
          with-input-from-file with-output-to-file open-input-file
          open-output-file close-input-port close-output-port read read-char
          peek-char eof-object? char-ready? write display newline write-char
          load)

  (begin ; (define values
         ;   (lambda xs
         ;     (call-with-current-continuation
         ;       (lambda (cc)
         ;         (apply cc xs)))))

         (define <values> (list 'values))

         (define (values? x)
           (if (pair? x)
               (eq? <values> (car x))
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

         (define (scheme-report-environment version)
           (environment `(scheme ,(string->symbol (string-append "r" (number->string version) "rs")))))

         (define (null-environment version)
           (environment `(only (scheme ,(string->symbol (string-append "r" (number->string version) "rs")))
                               quote lambda if set! cond case and or let let*
                               letrec begin do delay quasiquote let-syntax
                               letrec-syntax syntax-rules define define-syntax)))))
