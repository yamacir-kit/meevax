(define-library (scheme base)
  (import (meevax character) ; for digit-value
          (meevax number) ; for exact-integer?
          (meevax syntax) ; for quote-syntax
          (meevax vector) ; for vector->string
          (scheme r5rs)
          (srfi 39) ; Parameter objects
          (srfi 211 explicit-renaming)
          )

  (export quote
          lambda
          if
          set!
          ; include
          ; include-ci
          cond
          ; else
          ; =>
          case
          and
          or
          when
          unless
          ; cond-expand
          let
          let*
          letrec
          letrec*
          ; let-values
          ; let*-values
          begin
          do
          make-parameter
          parameterize
          ; guard
          quasiquote
          ; unquote
          ; unquote-splicing
          let-syntax
          letrec-syntax
          ; syntax-rules
          ; _
          ; ...
          ; syntax-error
          define
          ; define-values
          define-syntax
          ; define-record-type
          eqv?
          eq?
          equal?
          number?
          complex?
          real?
          rational?
          integer?
          exact?
          inexact?
          exact-integer?
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
          floor/
          floor-quotient
          floor-remainder
          truncate/
          truncate-quotient
          truncate-remainder
          quotient
          remainder
          modulo
          gcd
          lcm
          numerator
          denominator
          floor
          ceiling
          truncate
          round
          rationalize
          square
          ; exact-integer-sqrt
          expt
          inexact
          exact
          number->string
          string->number
          not
          boolean?
          boolean=?
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
          null?
          list?
          ; make-list
          list
          length
          append
          reverse
          list-tail
          list-ref
          list-set!
          memq
          memv
          member
          assq
          assv
          assoc
          ; list-copy
          symbol?
          ; symbol=?
          symbol->string
          string->symbol
          char?
          char=?
          char<?
          char>?
          char<=?
          char>=?
          char->integer
          integer->char
          string?
          make-string
          string
          string-length
          string-ref
          string-set!
          string=?
          string>?
          string<?
          string<=?
          string>=?
          substring
          string-append
          string->list
          list->string
          string-copy
          ; string-copy!
          string-fill!
          vector?
          make-vector
          vector
          vector-length
          vector-ref
          vector-set!
          vector->list
          list->vector
          vector->string
          ; string->vector
          ; vector-copy
          ; vector-copy!
          ; vector-append
          vector-fill!
          ; bytevector?
          ; make-bytevector
          ; bytevector
          ; bytevector-length
          ; bytevector-u8-ref
          ; bytevector-u8-set!
          ; bytevector-copy
          ; bytevector-copy!
          ; bytevector-append
          ; utf8->string
          ; string->utf8
          procedure?
          apply
          map
          ; string-map
          ; vector-map
          for-each
          ; string-for-each
          ; vector-for-each
          call-with-current-continuation
          call/cc
          values
          call-with-values
          dynamic-wind
          ; with-exception-handler
          ; raise
          ; raise-continuable
          ; error
          ; error-object?
          ; error-object-message
          ; error-object-irritants
          ; read-error?
          ; file-error?
          ; call-with-port
          ; input-port?
          ; output-port?
          ; textual-port?
          ; binary-port?
          ; port?
          ; input-port-open?
          ; output-port-open?
          ; current-input-port
          ; current-output-port
          ; current-error-port
          ; close-port
          ; close-input-port
          ; close-output-port
          ; open-input-string
          ; open-output-string
          ; get-output-string
          ; open-input-bytevector
          ; open-output-bytevector
          ; get-output-bytevector
          ; read-char
          ; peek-char
          ; read-line
          ; eof-object?
          ; eof-object
          ; char-ready?
          ; read-string
          ; read-u8
          ; peek-u8
          ; u8-ready?
          ; read-bytevector
          ; read-bytevector!
          ; newline
          ; write-char
          ; write-string
          ; write-u8
          ; write-bytevector
          ; flush-output-port
          ; features
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

         (define call/cc call-with-current-continuation)

         )
  )

(define-library (scheme delay)
  (export delay
          delay-force
          force
          promise?
          make-promise
          )
  )

(define-library (scheme case-lambda)
  (export case-lambda
          )
  )

(define-library (scheme inexact)
  (export finite?
          infinite?
          nan?
          exp
          log
          sin
          cos
          tan
          asin
          acos
          atan
          sqrt
          )
  )

(define-library (scheme complex)
  (export make-rectangular
          make-polar
          real-part
          imag-part
          angle
          )
  )

(define-library (scheme cxr)
  (import (meevax pair))
  (export caaar
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
          cddddr))

(define-library (scheme char)
  (export char-ci=?
          char-ci<?
          char-ci>?
          char-ci<=?
          char-ci>=?
          char-alphabetic?
          char-numeric?
          char-whitespace?
          char-upper-case?
          char-lower-case?
          digit-value
          char-upcase
          char-downcase
          char-foldcase
          string-ci=?
          string-ci<?
          string-ci>?
          string-ci<=?
          string-ci>=?
          string-upcase
          string-downcase
          string-foldcase
          )
  )

(define-library (scheme eval)
  (export environment
          eval
          )
  )

(define-library (scheme file)
  (export call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          open-input-file
          open-binary-input-file
          open-output-file
          open-binary-output-file
          file-exists?
          delete-file
          )
  )

(define-library (scheme read)
  (export read)
  )

(define-library (scheme write)
  (export write
          write-shared
          write-simple
          display
          )
  )

(define-library (scheme load)
  (export load
          )
  )

(define-library (scheme process-context)
  (export command-line
          exit
          emergency-exit
          get-environment-variable
          get-environment-variables
          )
  )

(define-library (scheme time)
  (export current-second
          current-jiffy
          jiffies-per-second
          )
  )

(import (scheme r5rs)
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

(define (read        . x) (%read       (if (pair? x) (car x) (current-input-port))))
(define (read-char   . x) (%read-char  (if (pair? x) (car x) (current-input-port))))
(define (peek-char   . x) (%peek-char  (if (pair? x) (car x) (current-input-port))))
(define (char-ready? . x) (read-ready? (if (pair? x) (car x) (current-input-port))))

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
