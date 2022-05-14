(define-library (scheme r5rs)
  (import (meevax evaluate)
          (meevax inexact)
          (meevax number) ; for exact-integer?
          (meevax port) ; for read-ready?
          (meevax string) ; for string-copy
          (meevax syntax) ; for let-syntax letrec-syntax
          (meevax vector) ; for vector-fill!
          (scheme r4rs essential)
          (srfi 45)
          (srfi 211 explicit-renaming)
          )

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

  (begin (define-syntax let*
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

         (define exact->inexact inexact)

         (define inexact->exact exact)

         (define (list-tail x k)
           (let list-tail ((x x)
                           (k k))
             (if (zero? k) x
                 (list-tail (cdr x)
                            (- k 1)))))

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

         ; (define values
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

         (define (char-ready? . port)
           (read-ready? (if (pair? port)
                            (car port)
                            (current-input-port))))

         )
  )
