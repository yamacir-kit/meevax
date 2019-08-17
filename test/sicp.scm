(load "../bootstrap.scm")
(load "../test/expect.scm")

; ------------------------------------------------------------------------------
;   1 Building Abstractions with Procedures
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;       1.1 The Elements of Programming
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;           1.1.1 Expressions
; ------------------------------------------------------------------------------

(expect 486 486)

(expect 486
  (+ 137 349))

(expect 666
  (- 1000 334))

(expect 495
  (* 5 99))

(expect 2
  (/ 10 5))

; (expect 12.7
  (+ 2.7 10)
  ; )

(expect 75
  (+ 21 35 12 7))

(expect 1200
  (* 25 4 12))

(expect 19
  (+ (* 3 5)
     (- 10 6)))

(expect 57
  (+ (* 3
        (+ (* 2 4)
           (+ 3 5)))
     (+ (- 10 7)
        6)))

; ------------------------------------------------------------------------------
;           1.1.2 Naming and the Environment
; ------------------------------------------------------------------------------

(define size 2)

(expect 2 size)

(expect 10
  (* 5 size))

(define pi 3.14159)

(define radius 10)

; (expect 314.159
  (* pi (* radius radius))
  ; )

(define circumference (* 2 pi radius ))

; (expect 62.8318
  circumference
  ; )

; ------------------------------------------------------------------------------
;           1.1.3 Evaluating Combinations
; ------------------------------------------------------------------------------

(expect 390
  (* (+ 2 (* 4 6))
     (+ 3 5 7)))

; ------------------------------------------------------------------------------
;           1.1.4 Compound Procedures
; ------------------------------------------------------------------------------

(define square
  (lambda (x)
    (* x x)))

(expect 441
  (square 21))

(expect 49
  (square (+ 2 5)))

(expect 81
  (square (square 3)))

(define sum-of-squares
  (lambda (x y)
    (+ (square x)
       (square y))))

(expect 25
  (sum-of-squares 3 4))

(define f
  (lambda (a)
    (sum-of-squares (+ a 1)
                    (* a 2))))

(expect 136
  (f 5))

; ------------------------------------------------------------------------------
;           1.1.5 The Substitution Model for Procedure Application
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;           1.1.6 Conditional Expressions and Predicates
; ------------------------------------------------------------------------------

(define abs
  (lambda (x)
    (cond ((> x 0) x)
          ((= x 0) 0)
          ((< x 0)
           (- x)))))

(define abs
  (lambda (x)
    (cond ((< x 0)
           (- x))
          (else x))))

(define abs
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

; (and (> x 5) (< x 10))

(define >=
  (lambda (x y)
    (or (> x y)
        (= x y))))

(define >=
  (lambda (x y)
    (not (< x y))))

; ------------------------------------------------------------------------------
;           1.1.7 Example: Square Roots by Newton's Method
; ------------------------------------------------------------------------------

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x) guess
        (sqrt-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(sqrt 9)

(sqrt (+ 100 37))

(sqrt (+ (sqrt 2)
         (sqrt 3)))

(square (sqrt 1000))

; ------------------------------------------------------------------------------
;           1.1.8 Procedures as Black-Box Abstractions
; ------------------------------------------------------------------------------

(define square
  (lambda (x)
    (* x x)))

(define square
  (lambda (x)
    (exp (double (log x)))))

(define double
  (lambda (x)
    (+ x x)))

(define square (lambda (x) (* x x)))
(define square (lambda (y) (* y y)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x))
       0.001)))

(define sqrt
  (lambda (x)
    (define good-enough?
      (lambda (guess x)
        (< (abs (- (square guess) x)) 0.001)))
    (define improve
      (lambda (guess x)
        (average guess (/ x guess))))
    (define sqrt-iter
      (lambda (guess x)
        (if (good-enough? guess x) guess
            (sqrt-iter (improve guess x) x))))
    (sqrt-iter 1.0 x)))

(define sqrt
  (lambda (x)
    (define good-enough?
      (lambda (guess)
        (< (abs (- ( square guess ) x)) 0.001)))
    (define improve
      (lambda (guess)
        (/ x guess)))
    (define sqrt-iter
      (lambda (guess)
        (if (good-enough? guess) guess
            (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0)))


