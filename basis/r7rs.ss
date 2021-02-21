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

; TODO with-exception-handler
; TODO raise ; SRFI-18
; TODO raise-continuable

(define (error message . irritants) ; SRFI-23
  (display "error: ")
  (display message)
  (for-each (lambda (each)
              (display " ")
              (write each))
            irritants)
  (newline)
  (exit 1)))

(define (error-object? x) #false)

; TODO error-object-message
; TODO error-object-irritants
; TODO read-error?
; TODO file-error?

; ---- 6.12. Environments and evaluation ---------------------------------------

; TODO environment
; TODO scheme-report-environment
; TODO null-environment

; ---- 6.13. Input and output --------------------------------------------------

; ------------------------------------------------------------------------------

(define (current-environment) (fork/csc identity))

(define interaction-environment
  (let ((e (fork/csc identity)))
    (lambda () e)))
