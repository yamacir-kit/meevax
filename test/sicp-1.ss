(import (srfi 78))

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

; (check (sqrt 100000000000000000000) => 10000000000.0) ; BUG: MEMORY-LEAK

; (check (sqrt 100000000000000000000000000) => 10000000000000.0) ; BUG: MEMORY-LEAK

; TODO
; (check (sqrt 0.0000000000001) => 3.162277660168379e-007)

; TODO
; (check (sqrt 0) => 0.0)

; ---- Exercise 1.8 ------------------------------------------------------------

(let ()
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

  (check (cube-root 100000000000000.0001) => 46415.88833612779))

; ---- Section 1.1.8 -----------------------------------------------------------

(define (square x) (* x x))
(define (square x) (exp (double (log x))))
(define (double x) (+ x x))

(define (square x) (* x x))
(define (square y) (* y y))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess x)
    (average guess (/ x guess)))

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

(let ()
  (define (factorial n)
    (if (= n 1) 1
        (* n (factorial (- n 1)))))

  (check (factorial 6) => 720))

(let ()
  (define (factorial n)
    (fact-iter 1 1 n))

  (define (fact-iter product counter max-count)
    (if (> counter max-count) product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

  (check (factorial 6) => 720))

; ---- Exercise 1.9 ------------------------------------------------------------

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(let ()
  (define (+ a b)
    (if (= a 0) b (inc (+ (dec a) b))))
  (check (+ 4 5) => 9))

(let ()
  (define (+ a b)
    (if (= a 0) b (+ (dec a) (inc b))))
  (check (+ 4 5) => 9))

; ---- Exercise 1.10 -----------------------------------------------------------

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(check (A 1 10) => 1024)

(check (A 2 4) => 65536)

(check (A 3 3) => 65536)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

; ---- Section 1.2.2 -----------------------------------------------------------

(let ()
  (define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))
  (check (fib 5) => 5))

(let ()
  (define (fib n)
    (fib-iter 1 0 n))
  (define (fib-iter a b count)
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))))
  (check (fib 5) => 5))

(let ()
  (define (count-change amount)
    (cc amount 5))

  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))

  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

  (check (count-change 100) => 292))

; ---- Exercise 1.11 -----------------------------------------------------------

(let ()
  (define (f n)
    (cond ((< n 3) n)
          (else (+ (f (- n 1))
                   (* 2 (f (- n 2)))
                   (* 3 (f (- n 3)))))))

  (check (f   -1) => -1)
  (check (f    0) =>  0)
  (check (f    5) => 25)
  ; (check (f 1000) => 1200411335581569104197621183222182410228690281055710781687044573790661709343985308756380381850406620666042607564631605876156610535933789714780132607755663854744223225249491730428647795602251203632973677695221003056803565827035107926395650932180708300409716979009255557336360673626403040863408122386349183735643342985009827495351241264386090544972951146415009560371824341466875)
 )

; (let () ; BUG: MEMORY-LEAK
;   (define (f n)
;     (define (f-i a b c count)
;       (cond ((< n 3) n)
;             ((<= count 0) a)
;             (else (f-i (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
;     (f-i 2 1 0 (- n 2)))
;
;   (check (f   -1) => -1)
;   (check (f    0) =>  0)
;   (check (f    5) => 25)
;   (check (f 1000) => 1200411335581569104197621183222182410228690281055710781687044573790661709343985308756380381850406620666042607564631605876156610535933789714780132607755663854744223225249491730428647795602251203632973677695221003056803565827035107926395650932180708300409716979009255557336360673626403040863408122386349183735643342985009827495351241264386090544972951146415009560371824341466875)
;   )

; ---- Exercise 1.12 -----------------------------------------------------------

(let ()
  (define (pascal r c)
    (if (or (= c 1)
            (= c r))
        1
        (+ (pascal (- r 1) (- c 1))
           (pascal (- r 1) c))))

  (check (pascal 1 1) => 1)
  (check (pascal 2 2) => 1)
  (check (pascal 3 2) => 2)
  (check (pascal 4 2) => 3)
  (check (pascal 5 2) => 4)
  (check (pascal 5 3) => 6))

; ---- Exercise 1.13 -----------------------------------------------------------

; ---- Section 1.2.3 -----------------------------------------------------------

; ---- Exercise 1.14 -----------------------------------------------------------

(let ()
  (define (cube x)
    (* x x x))

  (define (p x)
    (- (* 3 x)
       (* 4 (cube x))))

  (define (sine angle)
    (if (not (> (abs angle) 0.1)) angle
        (p (sine (/ angle 3.0)))))

  ; (check (sine 12.15) => 0)
 )

; ---- Section 1.2.4 -----------------------------------------------------------

(let ()
  (define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))

  (check (expt 1 1) => 1)
  (check (expt 2 2) => 4)
  (check (expt 2 8) => 256)
  (check (expt 2 16) => 65536))

(let ()
  (define (expt b n)
    (expt-iter b n 1))

  (define (expt-iter b counter product)
    (if (= counter 0) product
        (expt-iter b
                   (- counter 1)
                   (* b product))))

  (check (expt 1 1) => 1)
  (check (expt 2 2) => 4)
  (check (expt 2 8) => 256)
  (check (expt 2 16) => 65536))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(let ()
  (define (even? n)
    (= (remainder n 2) 0))

  (check (even? 1) => #f)
  (check (even? 2) => #t)
  )


(check-report)

(exit (check-passed? 78))
