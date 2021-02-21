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

; ------------------------------------------------------------------------------

(define interaction-environment
  (let ((e (fork/csc identity)))
    (lambda () e)))
