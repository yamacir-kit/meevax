(define-library (scheme base)
  (import (only (meevax exception) error? read-error? file-error? syntax-error?)
          (only (meevax number) exact-integer?)
          (only (meevax vector) vector->string)
          (only (meevax port)
                binary-port?
                textual-port?
                port?
                input-port-open?
                output-port-open?
                standard-input-port
                standard-output-port
                standard-error-port
                eof-object
                %read-char
                %peek-char
                read-ready?
                put-char
                put-string
                %flush-output-port
                )
          (meevax version)
          (scheme r5rs)
          (srfi  6) ; Basic String Ports
          (srfi 23) ; Error reporting mechanism
          (srfi 34) ; Exception Handling for Programs
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
          let-values
          let*-values
          begin
          do
          make-parameter
          parameterize
          guard
          quasiquote
          ; unquote
          ; unquote-splicing
          let-syntax
          letrec-syntax
          syntax-rules
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
          (rename eqv? boolean=?)
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
          make-list
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
          list-copy
          symbol?
          (rename eqv? symbol=?)
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
          string-map
          ; vector-map
          for-each
          ; string-for-each
          ; vector-for-each
          call-with-current-continuation
          (rename call-with-current-continuation call/cc)
          values
          call-with-values
          dynamic-wind
          with-exception-handler
          raise
          raise-continuable
          error
          error-object?
          (rename car error-object-message)
          (rename cdr error-object-irritants)
          read-error?
          file-error?
          call-with-port
          input-port?
          output-port?
          textual-port?
          binary-port?
          port?
          input-port-open?
          output-port-open?
          current-input-port
          current-output-port
          current-error-port
          close-port
          close-input-port
          close-output-port
          open-input-string
          open-output-string
          get-output-string
          ; open-input-bytevector
          ; open-output-bytevector
          ; get-output-bytevector
          read-char
          peek-char
          ; read-line
          eof-object?
          eof-object
          char-ready?
          ; read-string
          ; read-u8
          ; peek-u8
          ; u8-ready?
          ; read-bytevector
          ; read-bytevector!
          newline
          write-char
          write-string
          ; write-u8
          ; write-bytevector
          flush-output-port
          features)

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

         (define (make-list k . x)
           (let ((x (if (pair? x) (car x) #f)))
             (do ((i k (- i 1))
                  (xs '() (cons x xs)))
                 ((<= i 0) xs))))

         (define (list-set! xs k x)
           (set-car! (list-tail xs k) x))

         (define (list-copy x)
           (let list-copy ((x x))
             (if (pair? x)
                 (cons (car x)
                       (list-copy (cdr x)))
                 x)))

         (define (string-map f x . xs)
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

         (define (error-object? x)
           (or (error? x)
               (read-error? x)
               (file-error? x)
               (syntax-error? x)))

         ; (define (call-with-port port procedure)
         ;   (let-values ((results (procedure port)))
         ;     (close-port port)
         ;     (apply values results)))

         (define (call-with-port port procedure)
           (let ((result (procedure port)))
             (close-port port)
             result))

         (define current-input-port
           (make-parameter (standard-input-port)
             (lambda (x)
               (cond ((not (input-port? x))
                      (error "current-input-port: not input-port" x))
                     ((not (input-port-open? x))
                      (error "current-input-port: not input-port-open" x))
                     (else x)))))

         (define current-output-port
           (make-parameter (standard-output-port)
             (lambda (x)
               (cond ((not (output-port? x))
                      (error "current-output-port: not output-port" x))
                     ((not (output-port-open? x))
                      (error "current-output-port: not output-port-open" x))
                     (else x)))))

         (define current-error-port
           (make-parameter (standard-error-port)
             (lambda (x)
               (cond ((not (output-port? x))
                      (error "current-error-port: not output-port" x))
                     ((not (output-port-open? x))
                      (error "current-error-port: not output-port-open" x))
                     (else x)))))

         (define (close-port x)
           (cond ((input-port? x) (close-input-port x))
                 ((output-port? x) (close-output-port x))
                 (else (unspecified))))

         (define (read-char . x)
           (%read-char (if (pair? x)
                           (car x)
                           (current-input-port))))

         (define (peek-char . x)
           (%peek-char (if (pair? x)
                           (car x)
                           (current-input-port))))

         (define (char-ready? . x)
           (read-ready? (if (pair? x)
                            (car x)
                            (current-input-port))))

         (define (write-char x . port)
           (put-char x (if (pair? port)
                           (car port)
                           (current-output-port))))

         (define (write-string string . xs)
           (case (length xs)
             ((0)  (put-string string (current-output-port)))
             ((1)  (put-string string (car xs)))
             (else (put-string (apply string-copy string (cadr xs)) (car xs)))))

         (define (newline . port)
           (apply write-char #\newline port))

         (define (flush-output-port . port)
           (%flush-output-port (if (pair? port)
                                   (car port)
                                   (current-output-port))))
         )

  (begin (define-syntax cond
           (syntax-rules (else =>)
             ((cond (else result1 result2 ...))
              (begin result1 result2 ...))
             ((cond (test => result))
              (let ((temp test))
                (if temp (result temp))))
             ((cond (test => result) clause1 clause2 ...)
              (let ((temp test))
                (if temp
                    (result temp)
                    (cond clause1 clause2 ...))))
             ((cond (test)) test)
             ((cond (test) clause1 clause2 ...)
              (let ((temp test))
                (if temp
                    temp
                    (cond clause1 clause2 ...))))
             ((cond (test result1 result2 ...))
              (if test (begin result1 result2 ...)))
             ((cond (test result1 result2 ...)
                    clause1 clause2 ...)
              (if test
                  (begin result1 result2 ...)
                  (cond clause1 clause2 ...)))))

         (define-syntax case ; errata version
           (syntax-rules (else =>)
             ((case (key ...) clauses ...)
              (let ((atom-key (key ...)))
                (case atom-key clauses ...)))
             ((case key
                (else => result))
              (result key))
             ((case key
                (else result1 result2 ...))
              (begin result1 result2 ...))
             ((case key
                ((atoms ...) => result))
              (if (memv key '(atoms ...))
                  (result key)))
             ((case key ((atoms ...) => result) clause clauses ...)
              (if (memv key '(atoms ...))
                  (result key)
                  (case key clause clauses ...)))
             ((case key ((atoms ...) result1 result2 ...))
              (if (memv key '(atoms ...))
                  (begin result1 result2 ...)))
             ((case key ((atoms ...) result1 result2 ...) clause clauses ...)
              (if (memv key '(atoms ...))
                  (begin result1 result2 ...)
                  (case key clause clauses ...)))))

         (define-syntax and
           (syntax-rules ()
             ((and) #t)
             ((and test) test)
             ((and test1 test2 ...)
              (if test1 (and test2 ...) #f))))

         (define-syntax or
           (syntax-rules ()
             ((or) #f)
             ((or test) test)
             ((or test1 test2 ...)
              (let ((x test1))
                (if x x (or test2 ...))))))

         (define-syntax when
           (syntax-rules ()
             ((when test result1 result2 ...)
              (if test
                  (begin result1 result2 ...)))))

         (define-syntax unless
           (syntax-rules ()
             ((unless test result1 result2 ...)
              (if (not test)
                  (begin result1 result2 ...)))))

         (define-syntax let
           (syntax-rules ()
             ((let ((name val) ...) body1 body2 ...)
              ((lambda (name ...) body1 body2 ...) val ...))
             ((let tag ((name val) ...) body1 body2 ...)
              ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag) val ...))))

         (define-syntax let*
           (syntax-rules ()
             ((let* () body1 body2 ...)
              (let () body1 body2 ...))
             ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
              (let ((name1 val1))
                (let* ((name2 val2) ...) body1 body2 ...)))))

         (define-syntax letrec*
           (syntax-rules ()
             ((letrec* ((var1 init1) ...) body1 body2 ...)
              (let ((var1 <undefined>) ...)
                (set! var1 init1)
                ...
                (let () body1 body2 ...)))))

         (define-syntax let-values
           (syntax-rules ()
             ((let-values (binding ...) body0 body1 ...)
              (let-values "bind" (binding ...) () (begin body0 body1 ...)))
             ((let-values "bind" () tmps body)
              (let tmps body))
             ((let-values "bind" ((b0 e0) binding ...) tmps body)
              (let-values "mktmp" b0 e0 () (binding ...) tmps body))
             ((let-values "mktmp" () e0 args bindings tmps body)
              (call-with-values
                (lambda () e0)
                (lambda args
                  (let-values "bind" bindings tmps body))))
             ((let-values "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
              (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
             ((let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body)
              (call-with-values
                (lambda () e0)
                (lambda (arg ... . x)
                  (let-values "bind" bindings (tmp ... (a x)) body))))))

         ; (define-syntax let-values
         ;   (er-macro-transformer
         ;     (lambda (form rename compare)
         ;       (if (null? (cadr form))
         ;           `(,(rename 'let) () ,@(cddr form))
         ;           `(,(rename 'call-with-values)
         ;              (,(rename 'lambda) () ,(cadar (cadr form)))
         ;              (,(rename 'lambda) ,(caar (cadr form))
         ;                                 (,(rename 'let-values) ,(cdr (cadr form))
         ;                                                        ,@(cddr form))))))))

         (define-syntax let*-values
           (syntax-rules ()
             ((let*-values () body0 body1 ...)
              (let () body0 body1 ...))
             ((let*-values (binding0 binding1 ...)
                body0 body1 ...)
              (let-values (binding0)
                (let*-values (binding1 ...)
                  body0 body1 ...)))))

         ; (define-syntax let*-values
         ;   (er-macro-transformer
         ;     (lambda (form rename compare)
         ;       (if (null? (cadr form))
         ;           `(,(rename 'let) () ,@(cddr form))
         ;           `(,(rename 'let-values) (,(caadr form))
         ;                                   (,(rename 'let*-values) ,(cdadr form)
         ;                                                           ,@(cddr form)))))))

         (define-syntax do
           (syntax-rules ()
             ((do ((var init step ...) ...)
                (test expr ...)
                command ...)
              (letrec
                ((loop
                   (lambda (var ...)
                     (if test
                         (begin
                           (if #f #f)
                           expr ...)
                         (begin
                           command
                           ...
                           (loop (do "step" var step ...)
                                 ...))))))
                (loop init ...)))
             ((do "step" x)
              x)
             ((do "step" x y)
              y)))))

(define-library (scheme lazy)
  (import (srfi 45))
  (export delay
          (rename lazy delay-force)
          force
          promise?
          (rename eager make-promise)))

(define-library (scheme case-lambda)
  (export case-lambda
          )
  )

(define-library (scheme inexact)
  (import (only (meevax inexact) finite? infinite? nan?)
          (only (scheme r5rs) exp log sin cos tan asin acos atan sqrt))
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
          sqrt))

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
  (import (only (meevax character) char-codepoint)
          (only (scheme r5rs)
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
                char-upcase
                char-downcase
                string-ci=?
                string-ci<?
                string-ci>?
                string-ci<=?
                string-ci>=?)
          (only (scheme base) define string-map))

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
          (rename char-codepoint digit-value)
          char-upcase
          char-downcase
          (rename char-downcase char-foldcase)
          string-ci=?
          string-ci<?
          string-ci>?
          string-ci<=?
          string-ci>=?
          string-upcase
          string-downcase
          string-foldcase)

  (begin (define (string-upcase x)
           (string-map char-upcase x))

         (define (string-downcase x)
           (string-map char-downcase x))

         (define (string-foldcase x)
           (string-map char-foldcase x))))

(define-library (scheme eval)
  (import (only (meevax environment) environment)
          (only (meevax evaluate) eval))
  (export environment eval))

(define-library (scheme file)
  (import (only (meevax port) open-input-file open-output-file)
          (only (scheme r5rs) call-with-input-file call-with-output-file)
          (only (scheme base) define parameterize current-input-port current-output-port)
          )
  (export call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          open-input-file
          ; open-binary-input-file
          open-output-file
          ; open-binary-output-file
          ; file-exists?
          ; delete-file
          )
  (begin (define (with-input-from-file path thunk)
           (parameterize ((current-input-port (open-input-file path)))
             (thunk)))

         (define (with-output-to-file path thunk)
           (parameterize ((current-output-port (open-output-file path)))
             (thunk)))
         )
  )

(define-library (scheme read)
  (import (meevax read)
          (scheme base))
  (export read)
  (begin (define (read . x)
           (%read (if (pair? x)
                      (car x)
                      (current-input-port))))))

(define-library (scheme write)
  (import (scheme base)
          (only (meevax write) %write-simple)
          (only (meevax port) put-char)
          )
  (export write
          ; write-shared
          write-simple
          display
          )
  (begin (define (write-simple x . port)
           (%write-simple x (if (pair? port)
                                (car port)
                                (current-output-port))))

         (define write write-simple) ; DUMMY

         (define (display datum . port)
           (cond ((char?   datum) (apply write-char   datum port))
                 ((string? datum) (apply write-string datum port))
                 (else            (apply write        datum port))))

    )
  )

(define-library (scheme load)
  (import (only (scheme r5rs) load))
  (export load))

(define-library (scheme process-context)
  (import (meevax context)
          (scheme r5rs continuation)
          )
  (export ; command-line
          exit
          emergency-exit
          ; get-environment-variable
          ; get-environment-variables
          )
  )

(define-library (scheme time)
  (export current-second
          current-jiffy
          jiffies-per-second
          )
  )
