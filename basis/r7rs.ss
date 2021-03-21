; ------------------------------------------------------------------------------
;       ...          =>
;
;
;
;
;
;
;
;
; current-input-port current-output-port
;                         else
;
;
; interaction-environment                            let-syntax
; letrec-syntax                                         list-tail
;
;                                            null-environment
;
;
;
; scheme-report-environment
;
;
;
;                                  syntax-rules
;
; with-input-from-file with-output-to-file
; ------------------------------------------------------------------------------

; ---- 4.2.5. Delayed evaluation -----------------------------------------------

(define delay-force lazy) ; from SRFI-45

; ---- 6.1. Equivalence predicates ---------------------------------------------

; ---- 6.2. Numbers ------------------------------------------------------------

(define (finite? z) (not (infinite? z)))

(define (infinite? z)
  (or (= +inf.0 z)
      (= -inf.0 z)))

(define (nan? z)
  (if (%complex? z)
      (or (%nan? (real-part z))
          (%nan? (imag-part z)))
      (%nan? z)))

(define (square z) (* z z))

; TODO exact-integer-sqrt

; ---- 6.3. Booleans -----------------------------------------------------------

(define (boolean=? x y . xs)
  (and (eqv? x y)
       (or (not (pair? xs))
           (apply boolean=? y xs))))

; ---- 6.4. Pairs and lists ----------------------------------------------------

(define list-tail drop) ; from SRFI-1

(define (list-set! x k object) (set-car! (list-tail x k) object))

; ---- 6.5 Symbols -------------------------------------------------------------

(define (symbol=? x y . xs)
  (and (eqv? x y)
       (or (not (pair? xs))
           (apply boolean=? y xs))))

; ---- 6.6 Characters ----------------------------------------------------------

(define char-foldcase char-downcase)

; ---- 6.7 Strings -------------------------------------------------------------

(define (string-upcase   x) (string-map char-upcase   x))
(define (string-downcase x) (string-map char-downcase x))
(define (string-foldcase x) (string-map char-foldcase x))

; string-copy!

; ---- 6.8. Vectors ------------------------------------------------------------

; ---- 6.9. Bytevectors --------------------------------------------------------

(define (bytevector? x) #f)

; ---- 6.10. Control features --------------------------------------------------

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

; TODO vector-map

; TODO string-for-each
; TODO vector-for-each

(define call/cc call-with-current-continuation)

; ---- 6.11. Exceptions --------------------------------------------------------

; error => SRFI-23

; TODO with-exception-handler
; TODO raise
; TODO raise-continuable

(define (error-object? x)
  (or (error? x)
      (continuable-error? x)
      (read-error? x)
      (file-error? x)
      (syntax-error? x)))

(define error-object-message car)
(define error-object-irritants cdr)

; ---- 6.12. Environments and evaluation ---------------------------------------

; TODO environment
; TODO scheme-report-environment
; TODO null-environment

; ---- 6.13. Input and output --------------------------------------------------

(define (textual-port? x)
  (or (input-file-port? x)
      (input-string-port? x)
      (output-file-port? x)
      (output-string-port? x)
      (standard-port? x)))

(define (binary-port? x) #f)

(define (port? x)
  (or (input-port? x)
      (output-port? x)))

(define (input-port-open? x)
  (cond ((input-file-port? x)
         (input-file-port-open? x))
        ((input-string-port? x) #t)
        ((input-standard-port? x) #t)
        (else #f)))

(define (output-port-open? x)
  (cond ((output-file-port? x)
         (output-file-port-open? x))
        ((output-string-port? x) #t)
        ((output-standard-port? x) #t)
        ((error-standard-port? x) #t)
        (else #f)))

(define current-input-port
  (make-parameter (input-standard-port)
    (lambda (x)
      (cond ((not (input-port? x))
             (error "current-input-port: not input-port" x))
            ((not (input-port-open? x))
             (error "current-input-port: not input-port-open" x))
            (else x)))))

(define current-output-port
  (make-parameter (output-standard-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-output-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-output-port: not output-port-open" x))
            (else x)))))

(define current-error-port
  (make-parameter (error-standard-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-error-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-error-port: not output-port-open" x))
            (else x)))))

(define (with-input-from-file path thunk)
  (parameterize ((current-input-port (open-input-file path)))
    (thunk)))

(define (with-output-to-file path thunk)
  (parameterize ((current-output-port (open-output-file path)))
    (thunk)))

; TODO open-input-bytevector
; TODO open-output-bytevector
; TODO get-output-bytevector

(define (write-string string . xs)
  (case (length xs)
    ((0)  (::write-string string (current-output-port)))
    ((1)  (::write-string string (car xs)))
    (else (::write-string (apply string-copy string (cadr xs)) (car xs)))))

(define (write-path path . x)
  (::write-path path (if (pair? x)
                         (car x)
                         (current-output-port))))

; TODO write-u8
; TODO write-bytevector

(define (flush-output-port . port)
  (::flush-output-port (if (pair? port)
                           (car port)
                           (current-output-port))))


; ---- 6.14. System interface --------------------------------------------------

; TODO file-exists?
; TODO delete-file
; TODO command-line

; ------------------------------------------------------------------------------
;
;  (exit)                                     process-context library procedure
;  (exit obj)                                 process-context library procedure
;
;  Runs all outstanding dynamic-wind after procedures, terminates the running
;  program, and communicates an exit value to the operating system. If no
;  argument is supplied, or if obj is #t, the exit procedure should communicate
;  to the operating system that the program exited normally. If obj is #f, the
;  exit procedure should communicate to the operating system that the program
;  exited abnormally. Otherwise, exit should translate obj into an appropriate
;  exit value for the operating system, if possible.
;
;  The exit procedure must not signal an exception or return to its
;  continuation.
;
;  Note: Because of the requirement to run handlers, this procedure is not just
;  the operating system’s exit procedure.
;
; ------------------------------------------------------------------------------

(define (exit . normally?)
  (for-each (lambda (before/after)
              ((cdr before/after)))
            %current-dynamic-extents)
  (emergency-exit normally?))

; (dynamic-wind
;   (lambda () (display "before\n"))
;   (lambda () (exit))
;   (lambda () (display "after\n")))

; TODO get-environment-variable
; TODO get-environment-variables

; TODO current-second
; TODO current-jiffy
; TODO jiffies-per-second

; ------------------------------------------------------------------------------
;       ...          =>
;
;
;
;
;
;
;
;
;
;                         else
;
;
; interaction-environment                            let-syntax
; letrec-syntax
;
;                                            null-environment
;
;
;
; scheme-report-environment
;
;
;
;                                  syntax-rules
;
;
; ------------------------------------------------------------------------------

(define interaction-environment
  (let ((e (fork/csc identity)))
    (lambda () e)))
