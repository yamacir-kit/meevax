(define-library (scheme base)
  (import (only (meevax error) error-object? read-error? file-error?)
          (only (meevax macro-transformer) er-macro-transformer)
          (only (meevax number) exact-integer? exact-integer-square-root)
          (only (meevax port) binary-port? textual-port? port? input-port open? output-port flush error-port eof-object)
          (only (meevax read) get-char get-char! get-ready?)
          (only (meevax string) string-copy! vector->string)
          (only (meevax vector homogeneous) u8vector? make-u8vector u8vector u8vector-length u8vector-ref u8vector-set! u8vector-copy u8vector-copy! u8vector-append u8vector->string string->u8vector)
          (only (meevax vector) vector-append vector-copy vector-copy! string->vector)
          (only (meevax version) features)
          (only (meevax write) put-char put-string)
          (scheme r5rs)
          (srfi 6)
          (srfi 9)
          (srfi 11)
          (srfi 23)
          (srfi 34)
          (srfi 39))

  (export ; 4.1. Primitive expression types
          quote lambda if set!
          ; include include-ci
          cond else => case and or when unless
          ; cond-expand
          let let* letrec letrec* let-values let*-values begin do
          make-parameter parameterize guard quasiquote unquote unquote-splicing
          let-syntax letrec-syntax syntax-rules _ ... syntax-error

          ; 5.3. Variable definitions
          define define-values define-syntax define-record-type

          ; 6.1. Equivalence predicates
          eqv? eq? equal?

          ; 6.2. Numbers
          number? complex? real? rational? integer? exact? inexact?
          exact-integer? = < > <= >= zero? positive? negative? odd? even? max
          min + * - / abs floor/ floor-quotient floor-remainder truncate/
          truncate-quotient truncate-remainder quotient remainder modulo gcd
          lcm numerator denominator floor ceiling truncate round rationalize
          square exact-integer-sqrt expt inexact exact number->string
          string->number

          ; 6.3. Booleans
          not boolean? boolean=?

          ; 6.4. Pairs and lists
          pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr null? list?
          make-list list length append reverse list-tail list-ref list-set!
          memq memv member assq assv assoc list-copy

          ; 6.5. Symbols
          symbol? symbol=? symbol->string string->symbol

          ; 6.6. Characters
          char? char=? char<? char>? char<=? char>=? char->integer
          integer->char

          ; 6.7. Strings
          string? make-string string string-length string-ref string-set!
          string=? string>? string<? string<=? string>=? substring
          string-append string->list list->string string-copy string-copy!
          string-fill!

          ; 6.8. Vectors
          vector? make-vector vector vector-length vector-ref vector-set!
          vector->list list->vector vector->string string->vector vector-copy
          vector-copy! vector-append vector-fill!

          ; 6.9. Bytevectors
          bytevector? make-bytevector bytevector bytevector-length
          bytevector-u8-ref bytevector-u8-set! bytevector-copy bytevector-copy!
          bytevector-append utf8->string string->utf8

          ; 6.10. Control features
          procedure? apply map string-map vector-map for-each
          ; string-for-each vector-for-each
          call-with-current-continuation call/cc values call-with-values
          dynamic-wind

          ; 6.11. Exceptions
          with-exception-handler raise raise-continuable error error-object?
          error-object-message error-object-irritants read-error? file-error?

          ; 6.13. Input and output
          call-with-port input-port? output-port? textual-port? binary-port?
          port? input-port-open? output-port-open? current-input-port
          current-output-port current-error-port close-port close-input-port
          close-output-port open-input-string open-output-string
          get-output-string
          ; open-input-bytevector open-output-bytevector get-output-bytevector
          read-char peek-char
          ; read-line
          eof-object? eof-object char-ready?
          ; read-string read-u8 peek-u8 u8-ready? read-bytevector
          ; read-bytevector!
          newline write-char write-string
          ; write-u8 write-bytevector
          flush-output-port

          ; 6.14. System interface
          features)

  (begin (define-syntax when
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

         (define-syntax syntax-error
           (er-macro-transformer
             (lambda (form rename compare)
               (apply error (cdr form)))))

         (define-syntax define-values
           (syntax-rules ()
             ((define-values () expression)
              (define dummy
                (call-with-values (lambda () expression)
                                  (lambda xs #f))))
             ((define-values (identifier) expression)
              (define identifier expression))
             ((define-values (id-0 id-1 ... id-n) expression)
              (begin (define id-0
                       (call-with-values (lambda () expression) list))
                     (define id-1
                       (let ((x (cadr id-0)))
                         (set-cdr! id-0 (cddr id-0))
                         x)) ...
                     (define id-n
                       (let ((x (cadr id-0)))
                         (set! id-0 (car id-0))
                         x))))
             ((define-values (id-0 id-1 ... . id-n) expression)
              (begin (define id-0
                       (call-with-values (lambda () expression) list))
                     (define id-1
                       (let ((x (cadr id-0)))
                         (set-cdr! id-0 (cddr id-0))
                         x)) ...
                     (define id-n
                       (let ((x (cdr id-0)))
                         (set! id-0 (car id-0))
                         x))))
             ((define-values identifier expression)
              (define identifier
                (call-with-values (lambda () expression) list)))))

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

         (define (square z)
           (* z z))

         (define (exact-integer-sqrt k)
           (let ((x (exact-integer-square-root k)))
             (values (car x)
                     (cdr x))))

         (define inexact exact->inexact)

         (define exact inexact->exact)

         (define boolean=? eqv?)

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

         (define symbol=? eqv?)

         (define bytevector? u8vector?)

         (define make-bytevector make-u8vector)

         (define bytevector u8vector)

         (define bytevector-length u8vector-length)

         (define bytevector-u8-ref u8vector-ref)

         (define bytevector-u8-set! u8vector-set!)

         (define bytevector-copy u8vector-copy)

         (define bytevector-copy! u8vector-copy!)

         (define bytevector-append u8vector-append)

         (define utf8->string u8vector->string)

         (define string->utf8 string->u8vector)

         (define (string-map f x . xs)
           (if (null? xs)
               (list->string (map f (string->list x)))
               (list->string (apply map f (map string->list (cons x xs))))))

         (define (vector-map f x . xs)
           (if (null? xs)
               (list->vector (map f (vector->list x)))
               (list->vector (apply map f (map vector->list (cons x xs))))))

         (define call/cc call-with-current-continuation)

         (define error-object-message car)

         (define error-object-irritants cdr)

         (define (call-with-port port procedure)
           (let-values ((xs (procedure port)))
             (close-port port)
             (apply values xs)))

         (define input-port-open? open?)

         (define output-port-open? open?)

         (define current-input-port
           (make-parameter (input-port)
             (lambda (x)
               (cond ((not (input-port? x))
                      (error "current-input-port: not input-port" x))
                     ((not (input-port-open? x))
                      (error "current-input-port: not input-port-open" x))
                     (else x)))))

         (define current-output-port
           (make-parameter (output-port)
             (lambda (x)
               (cond ((not (output-port? x))
                      (error "current-output-port: not output-port" x))
                     ((not (output-port-open? x))
                      (error "current-output-port: not output-port-open" x))
                     (else x)))))

         (define current-error-port
           (make-parameter (error-port)
             (lambda (x)
               (cond ((not (output-port? x))
                      (error "current-error-port: not output-port" x))
                     ((not (output-port-open? x))
                      (error "current-error-port: not output-port-open" x))
                     (else x)))))

         (define (close-port x)
           (cond ((input-port? x) (close-input-port x))
                 ((output-port? x) (close-output-port x))
                 (else (if #f #f))))

         (define (read-char . x)
           (get-char! (if (pair? x)
                          (car x)
                          (current-input-port))))

         (define (peek-char . x)
           (get-char (if (pair? x)
                         (car x)
                         (current-input-port))))

         (define (char-ready? . x)
           (get-ready? (if (pair? x)
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
             (else (put-string (apply string-copy string (cdr xs)) (car xs)))))

         (define (newline . port)
           (apply write-char #\newline port))

         (define (flush-output-port . port)
           (flush (if (pair? port)
                      (car port)
                      (current-output-port))))))

(define-library (scheme lazy)
  (import (srfi 45))
  (export delay (rename lazy delay-force) force promise? (rename eager make-promise)))

(define-library (scheme case-lambda)
  (import (scheme base))
  (export case-lambda)
  (begin (define-syntax case-lambda
           (syntax-rules ()
             ((case-lambda (params body0 ...) ...)
              (lambda args
                (let ((len (length args)))
                  (let-syntax
                    ((cl (syntax-rules ::: ()
                           ((cl)
                            (error "no matching clause"))
                           ((cl ((p :::) . body) . rest)
                            (if (= len (length '(p :::)))
                                (apply (lambda (p :::) . body) args)
                                (cl . rest)))
                           ((cl ((p ::: . tail) . body) . rest)
                            (if (>= len (length '(p :::)))
                                (apply (lambda (p ::: . tail) . body) args)
                                (cl . rest))))))
                    (cl (params body0 ...) ...)))))))))

(define-library (scheme inexact)
  (import (only (meevax inexact) finite? infinite? nan?)
          (only (scheme r5rs) exp log sin cos tan asin acos atan sqrt))
  (export finite? infinite? nan? exp log sin cos tan asin acos atan sqrt))

(define-library (scheme complex)
  (import (meevax complex)
          (scheme base)
          (scheme inexact))

  (export make-rectangular make-polar real-part imag-part magnitude angle)

  (begin (define (make-polar magnitude angle)
           (make-rectangular (* magnitude (cos angle))
                             (* magnitude (sin angle))))

         (define (magnitude z)
           (let ((re (real-part z))
                 (im (imag-part z)))
             (sqrt (+ (* re re)
                      (* im im)))))

         (define (angle z)
           (atan (imag-part z)
                 (real-part z)))))

(define-library (scheme cxr)
  (import (meevax pair))
  (export caaar caaaar cdaaar
          caadr caaadr cdaadr
          cadar caadar cdadar
          caddr caaddr cdaddr
          cdaar cadaar cddaar
          cdadr cadadr cddadr
          cddar caddar cdddar
          cdddr cadddr cddddr))

(define-library (scheme char)
  (import (only (meevax character) digit-value)
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
          digit-value
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
  (import (only (meevax environment) environment eval))
  (export environment eval))

(define-library (scheme file)
  (import (only (scheme r5rs) call-with-input-file call-with-output-file open-input-file open-output-file)
          (only (scheme base) define parameterize current-input-port current-output-port))
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
  (import (rename (meevax read)
                  (read %read))
          (scheme base))
  (export read)
  (begin (define (read . x)
           (%read (if (pair? x)
                      (car x)
                      (current-input-port))))))

(define-library (scheme repl)
  (import (only (meevax environment) interaction-environment))
  (export interaction-environment))

(define-library (scheme write)
  (import (prefix (meevax write) %)
          (scheme base)
          (srfi 38))

  (begin (define (write x . port)
           (%write x (if (pair? port)
                         (car port)
                         (current-output-port))))

         (define (write-shared x . port)
           (write-with-shared-structure x (if (pair? port)
                                              (car port)
                                              (current-output-port))))

         (define (write-simple x . port)
           (%write-simple x (if (pair? port)
                                (car port)
                                (current-output-port))))

         (define (display x . xs)
           (cond ((char? x)
                  (apply write-char x xs))
                 ((string? x)
                  (apply write-string x xs))
                 (else (apply write x xs)))))

  (export write write-shared write-simple display))

(define-library (scheme load)
  (import (only (scheme r5rs) load))
  (export load))

(define-library (scheme process-context)
  (import (only (meevax context) command-line emergency-exit)
          (only (meevax continuation) exit)
          (srfi 98))
  (export command-line
          exit
          emergency-exit
          get-environment-variable
          get-environment-variables))

(define-library (scheme time)
  (import (only (meevax time) current-jiffy jiffies-per-second)
          (only (scheme base) / define inexact))
  (export current-second current-jiffy jiffies-per-second)
  (begin (define (current-second)
           (inexact (/ (current-jiffy)
                       (jiffies-per-second))))))
