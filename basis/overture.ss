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
