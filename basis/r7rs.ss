(define-library (scheme base)
  (import (only (meevax core) include include-case-insensitive)
          (only (meevax error) error-object? read-error? file-error?)
          (only (meevax list) make-list list-copy)
          (only (meevax macro-transformer) er-macro-transformer er-macro-transformer-v2)
          (only (meevax number) exact-integer? exact-integer-square-root)
          (only (meevax port) binary-port? eof-object flush get-output-u8vector open-input-u8vector open-output-u8vector open? port? standard-error-port standard-input-port standard-output-port textual-port?)
          (only (meevax string) string-copy!)
          (only (meevax vector homogeneous) u8vector? make-u8vector u8vector u8vector-length u8vector-ref u8vector-set! u8vector-copy u8vector-copy! u8vector-append u8vector->string string->u8vector)
          (only (meevax vector) vector-append vector-copy vector-copy! vector->string string->vector)
          (only (meevax version) features)
          (prefix (meevax read) %)
          (prefix (meevax write) %)
          (scheme r5rs)
          (srfi 0)
          (srfi 6)
          (srfi 9)
          (srfi 11)
          (srfi 23)
          (srfi 34)
          (srfi 39))

  (export ; 4.1. Primitive expression types
          quote lambda if set! include include-ci cond else => case and or when
          unless cond-expand let let* letrec letrec* let-values let*-values
          begin do make-parameter parameterize guard quasiquote unquote
          unquote-splicing let-syntax letrec-syntax syntax-rules _ ...
          syntax-error

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
          procedure? apply map string-map vector-map for-each string-for-each
          vector-for-each call-with-current-continuation call/cc values
          call-with-values dynamic-wind

          ; 6.11. Exceptions
          with-exception-handler raise raise-continuable error error-object?
          error-object-message error-object-irritants read-error? file-error?

          ; 6.13. Input and output
          call-with-port input-port? output-port? textual-port? binary-port?
          port? input-port-open? output-port-open? current-input-port
          current-output-port current-error-port close-port close-input-port
          close-output-port open-input-string open-output-string
          get-output-string open-input-bytevector open-output-bytevector
          get-output-bytevector read-char peek-char read-line eof-object?
          eof-object char-ready? read-string read-u8 peek-u8 u8-ready?
          read-bytevector read-bytevector! newline write-char write-string
          write-u8 write-bytevector flush-output-port

          ; 6.14. System interface
          features)

  (begin (define include-ci include-case-insensitive)

         (define-syntax when
           (er-macro-transformer-v2
             (lambda (form rename compare)
               `(,(rename 'if) ,(cadr form)
                               (,(rename 'begin) ,@(cddr form))))))

         (define-syntax unless
           (er-macro-transformer-v2
             (lambda (form rename compare)
               `(,(rename 'if) (,(rename 'not) ,(cadr form))
                               (,(rename 'begin) ,@(cddr form))))))

         (define-syntax letrec*
           (er-macro-transformer-v2
             (lambda (form rename compare)
               `(,(rename 'let) ()
                                ,@(map (lambda (x) (cons (rename 'define) x))
                                       (cadr form))
                                ,@(cddr form)))))

         (define-syntax syntax-error
           (er-macro-transformer-v2
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

         (define (list-set! xs k x)
           (set-car! (list-tail xs k) x))

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

         (define (string-for-each f x . xs)
           (if (null? xs)
               (for-each f (string->list x))
               (apply for-each f (map string->list (cons x xs)))))

         (define (vector-for-each f x . xs)
           (if (null? xs)
               (for-each f (vector->list x))
               (apply for-each f (map vector->list (cons x xs)))))

         (define call/cc call-with-current-continuation)

         (define error-object-message car)

         (define error-object-irritants cdr)

         (define (call-with-port port procedure)
           (let-values ((xs (procedure port)))
             (close-port port)
             (apply values xs)))

         (define input-port-open? open?)

         (define output-port-open? open?)

         (define current-error-port
           (make-parameter (standard-error-port)))

         (define (close-port x)
           (cond ((input-port? x) (close-input-port x))
                 ((output-port? x) (close-output-port x))
                 (else (if #f #f))))

         (define open-input-bytevector open-input-u8vector)

         (define open-output-bytevector open-output-u8vector)

         (define get-output-bytevector get-output-u8vector)

         (define (read-char . xs)
           (%get-char (if (pair? xs)
                          (car xs)
                          (current-input-port))))

         (define (peek-char . xs)
           (%peek-char (if (pair? xs)
                           (car xs)
                           (current-input-port))))

         (define (read-line . xs)
           (%get-line (if (pair? xs)
                          (car xs)
                          (current-input-port))))

         (define (char-ready? . xs)
           (%get-char-ready? (if (pair? xs)
                                 (car xs)
                                 (current-input-port))))

         (define (read-string x . xs)
           (%get-string x
                        (if (pair? xs)
                            (car xs)
                            (current-input-port))))

         (define (read-u8 . xs)
           (%get-u8 (if (pair? xs)
                        (car xs)
                        (current-input-port))))

         (define (peek-u8 . xs)
           (%peek-u8 (if (pair? xs)
                         (car xs)
                         (current-input-port))))

         (define (u8-ready? . xs)
           (%get-u8-ready? (if (pair? xs)
                               (car xs)
                               (current-input-port))))

         (define (read-bytevector x . xs)
           (%get-u8vector x
                          (if (pair? xs)
                              (car xs)
                              (current-input-port))))

         (define (read-bytevector! x . xs)
           (let* ((start (if (and (pair? xs)
                                  (pair? (cdr xs)))
                             (cadr xs)
                             0))
                  (end (if (and (pair? xs)
                                (pair? (cdr xs))
                                (pair? (cddr xs)))
                           (caddr xs)
                           (bytevector-length x)))
                  (v (read-bytevector (- end start)
                                      (if (pair? xs)
                                          (car xs)
                                          (current-input-port)))))
             (if (eof-object? v)
                 (eof-object)
                 (bytevector-copy! x start v))))

         (define (write-char x . xs)
           (%put-char x (if (pair? xs)
                            (car xs)
                            (current-output-port))))

         (define (write-string x . xs)
           (%put-string (if (< 1 (length x))
                            (apply string-copy x (cdr xs))
                            x)
                        (if (pair? xs)
                            (car xs)
                            (current-output-port))))

         (define (write-u8 x . xs)
           (%put-u8 x (if (pair? xs)
                          (car xs)
                          (current-output-port))))

         (define (write-bytevector x . xs)
           (%put-u8vector (if (< 1 (length xs))
                              (apply bytevector-copy x (cdr xs))
                              x)
                          (if (pair? xs)
                              (car xs)
                              (current-output-port))))

         (define (newline . xs)
           (apply write-char #\newline xs))

         (define (flush-output-port . xs)
           (flush (if (pair? xs)
                      (car xs)
                      (current-output-port))))))

(define-library (scheme box)
  (import (srfi 111))
  (export box box? unbox set-box!))

(define-library (scheme case-lambda)
  (import (srfi 16))
  (export case-lambda))

(define-library (scheme char)
  (import (only (meevax character) digit-value)
          (only (scheme r5rs) char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char-upcase char-downcase string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
          (only (scheme base) define string-map))

  (export char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-alphabetic?
          char-numeric? char-whitespace? char-upper-case? char-lower-case?
          digit-value char-upcase char-downcase char-foldcase string-ci=?
          string-ci<? string-ci>? string-ci<=? string-ci>=? string-upcase
          string-downcase string-foldcase)

  (begin (define char-foldcase char-downcase)

         (define (string-upcase x)
           (string-map char-upcase x))

         (define (string-downcase x)
           (string-map char-downcase x))

         (define (string-foldcase x)
           (string-map char-foldcase x))))

(define-library (scheme complex)
  (import (only (scheme r5rs) make-rectangular make-polar real-part imag-part magnitude angle))
  (export make-rectangular make-polar real-part imag-part magnitude angle))

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

(define-library (scheme eval)
  (import (only (meevax environment) environment eval))
  (export environment eval))

(define-library (scheme file)
  (import (only (meevax file) delete-file file-exists?)
          (only (meevax port) open-binary-input-file open-binary-output-file)
          (only (scheme base) close-input-port close-output-port current-input-port current-output-port define parameterize)
          (only (scheme r5rs) call-with-input-file call-with-output-file open-input-file open-output-file with-input-from-file with-output-to-file))
  (export call-with-input-file call-with-output-file delete-file file-exists?
          open-binary-input-file open-binary-output-file open-input-file
          open-output-file with-input-from-file with-output-to-file))

(define-library (scheme flonum)
  (import (srfi 144))
  (export fl-e fl-1/e fl-e-2 fl-e-pi/4 fl-log2-e fl-log10-e fl-log-2 fl-1/log-2
          fl-log-3 fl-log-pi fl-log-10 fl-1/log-10 fl-pi fl-1/pi fl-2pi fl-pi/2
          fl-pi/4 fl-pi-squared fl-degree fl-2/pi fl-2/sqrt-pi fl-sqrt-2
          fl-sqrt-3 fl-sqrt-5 fl-sqrt-10 fl-1/sqrt-2 fl-cbrt-2 fl-cbrt-3
          fl-4thrt-2 fl-phi fl-log-phi fl-1/log-phi fl-euler fl-e-euler
          fl-sin-1 fl-cos-1 fl-gamma-1/2 fl-gamma-1/3 fl-gamma-2/3 fl-greatest
          fl-least fl-epsilon fl-fast-fl+* fl-integer-exponent-zero
          fl-integer-exponent-nan flonum fladjacent flcopysign make-flonum
          flinteger-fraction flexponent flinteger-exponent
          flnormalized-fraction-exponent flsign-bit flonum? fl=? fl<? fl>?
          fl<=? fl>=? flunordered? flinteger? flzero? flpositive? flnegative?
          flodd? fleven? flfinite? flinfinite? flnan? flnormalized?
          fldenormalized? flmax flmin fl+ fl* fl+* fl- fl/ flabs flabsdiff
          flposdiff flsgn flnumerator fldenominator flfloor flceiling flround
          fltruncate flexp flexp2 flexp-1 flsquare flsqrt flcbrt flhypot flexpt
          fllog fllog1+ fllog2 fllog10 make-fllog-base flsin flcos fltan flasin
          flacos flatan flsinh flcosh fltanh flasinh flacosh flatanh flquotient
          flremainder flremquo flgamma flloggamma flfirst-bessel
          flsecond-bessel flerf flerfc))

(define-library (scheme inexact)
  (import (only (meevax inexact) finite? infinite? nan?)
          (only (scheme r5rs) exp log sin cos tan asin acos atan sqrt))
  (export finite? infinite? nan? exp log sin cos tan asin acos atan sqrt))

(define-library (scheme lazy)
  (import (srfi 45))
  (export delay (rename lazy delay-force) force promise? (rename eager make-promise)))

(define-library (scheme list)
  (import (srfi 1))
  (export cons list xcons cons* make-list list-tabulate list-copy circular-list
          iota pair? null? proper-list? circular-list? dotted-list? not-pair?
          null-list? list= car cdr caar cadr cdar cddr caaar caadr cadar caddr
          cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
          caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          list-ref first second third fourth fifth sixth seventh eighth ninth
          tenth car+cdr take take! take-right drop drop-right drop-right!
          split-at split-at! last last-pair length length+ append append!
          concatenate concatenate! reverse reverse! append-reverse
          append-reverse! zip unzip1 unzip2 unzip3 unzip4 unzip5 count map map!
          filter-map map-in-order fold fold-right unfold unfold-right pair-fold
          pair-fold-right reduce reduce-right append-map append-map! for-each
          pair-for-each filter filter! partition partition! remove remove! memq
          memv member find find-tail any every list-index take-while
          take-while! drop-while span span! break break! delete delete!
          delete-duplicates delete-duplicates! assq assv assoc alist-cons
          alist-copy alist-delete alist-delete! lset<= lset= lset-adjoin
          lset-union lset-union! lset-intersection lset-intersection!
          lset-difference lset-difference! lset-xor lset-xor!
          lset-diff+intersection lset-diff+intersection! set-car! set-cdr!))

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

(define-library (scheme read)
  (import (prefix (meevax read) %)
          (only (scheme base) define if pair? car current-input-port))
  (export read)
  (begin (define (read . xs)
           (%read (if (pair? xs)
                      (car xs)
                      (current-input-port))))))

(define-library (scheme repl)
  (import (only (meevax environment) interaction-environment))
  (export interaction-environment))

(define-library (scheme time)
  (import (only (meevax time) current-jiffy jiffies-per-second)
          (only (scheme base) / define inexact))
  (export current-second current-jiffy jiffies-per-second)
  (begin (define (current-second)
           (inexact (/ (current-jiffy)
                       (jiffies-per-second))))))

(define-library (scheme write)
  (import (prefix (meevax write) %)
          (scheme base)
          (only (srfi 38) write-with-shared-structure))

  (export write write-shared write-simple display)

  (begin (define (write x . xs)
           (%write x (if (pair? xs)
                         (car xs)
                         (current-output-port))))

         (define (write-shared x . xs)
           (write-with-shared-structure x (if (pair? xs)
                                              (car xs)
                                              (current-output-port))))

         (define (write-simple x . xs)
           (%write-simple x (if (pair? xs)
                                (car xs)
                                (current-output-port))))

         (define (display x . xs)
           (cond ((char? x)
                  (apply write-char x xs))
                 ((string? x)
                  (apply write-string x xs))
                 (else (apply write x xs))))))
