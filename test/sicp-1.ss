(check-set-mode! 'report)

; ---- Section 1.1.1 -----------------------------------------------------------

(check 486 => 486)

(check (+ 137 349) => 486)

(check (- 1000 334) => 666)

(check (* 5 99) => 495)

(check (/ 10 5) => 2)

(check (+ 2.7 10) => 12.7)

(check (+ 21 35 12 7) => 75)

(check (* 25 4 12) => 1200)

(check (+ (* 3 5) (- 10 6)) => 19)

(check (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) => 57)

; ---- Section 1.1.2 -----------------------------------------------------------

(define size 2)

(check size => 2)

(check (* 5 size) => 10)

(define pi 3.14159)

(define radius 10)

(check (* pi (* radius radius)) => 314.159)

(define circumference (* 2 pi radius))

(check circumference => 62.8318)

; ---- Section 1.1.3 -----------------------------------------------------------

(check
  (* (+ 2 (* 4 6))
          (+ 3 5 7))
  => 390)

; ---- Section 1.1.4 -----------------------------------------------------------

(define (square x) (* x x))

(check (square 21) => 441)

(check (square (+ 2 5)) => 49)

(check (square (square 3)) => 81)

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(check (sum-of-squares 3 4) => 25)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(check (f 5) => 136)

; ---- Section 1.1.5 -----------------------------------------------------------

(check (f 5) => 136)

; ---- Section 1.1.6 -----------------------------------------------------------

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y) (or (> x y) (= x y)))

(define (>= x y) (not (< x y)))

; ---- Exercise 1.1 ------------------------------------------------------------

(check 10 => 10)

(check (+ 5 3 4) => 12)

(check (- 9 1) => 8)

(check (/ 6 2) => 3)

(check (+ (* 2 4) (- 4 6)) => 6)

(define a 3)

(define b (+ a 1))

(check (+ a b (* a b)) => 19)

(check (= a b) => #false)

(check (if (and (> b a) (< b (* a b))) b a) => 4)

(check
  (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25))
  => 16)

(check (+ 2 (if (> b a) b a)) => 6)

(check
  (* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
     (+ a 1))
  => 16)

; ---- Exercise 1.2 ------------------------------------------------------------

; TODO
; (check (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;   => -37/150)

; ---- Exercise 1.3 ------------------------------------------------------------

(define (f a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= b a) (>= c a)) (sum-of-squares b c))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))))

(check (f 1 2 3) => 13)
(check (f 1 2 2) =>  8)
(check (f 1 1 2) =>  5)
(check (f 1 1 1) =>  2)

; ---- Exercise 1.4 ------------------------------------------------------------

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(check (a-plus-abs-b 1  3) => 4)
(check (a-plus-abs-b 1 -3) => 4)

; ---- Exercise 1.5 ------------------------------------------------------------

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p)) => never terminates

; ---- Section 1.1.7 -----------------------------------------------------------

(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(check (sqrt 9) => 3.00009155413138)

(check (sqrt (+ 100 37)) => 11.704699917758145)

(check (sqrt (+ (sqrt 2) (sqrt 3))) => 1.7739279023207892)

(check (square (sqrt 1000)) => 1000.000369924366)

; ---- Exercise 1.7 ------------------------------------------------------------

(define (good-enough? guess x)
  (= (improve guess x) guess))

(check (sqrt 9) => 3.0)

(check (sqrt 0.0001) => 0.01)

(check (sqrt 10000000000000.0001) => 3162277.6601683795)

(check (sqrt 100000000000000000000) => 10000000000.0)

(check (sqrt 100000000000000000000000000) => 10000000000000.0)

; TODO
; (check (sqrt 0.0000000000001) => 3.162277660168379e-007)

; TODO
; (check (sqrt 0) => 0.0)

; ---- Exercise 1.8 ------------------------------------------------------------

(define (square guess)
  (* guess guess))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (cube-root-iter guess x)
  (if (good-enough? guess x) guess
      (cube-root-iter (improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.1 x))

(check (cube-root 5) => 1.709975946676697)

(check (cube-root -2) => -1.2599210498948732)

(check (cube-root 27) => 3.0)

; TODO
; (check (cube-root 0) => 4.9406564584125e-324)

(check (cube-root 100000000000000.0001) => 46415.88833612779)

; ---- Section 1.1.8 -----------------------------------------------------------

(define (square x) (* x x))
(define (square x) (exp (double (log x))))
(define (double x) (+ x x))

(define (square x) (* x x))
(define (square y) (* y y))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; ---- Section 1.2.1 -----------------------------------------------------------

; SRFI-78

(check-report)

(exit (if (check-passed? check::correct) exit-success exit-failure))
