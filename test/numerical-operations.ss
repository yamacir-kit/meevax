; ---- 6.2.6. Numerical operations ---------------------------------------------

(check (rational? 1/3) => #t)
(check (rational? 0.5) => #t)

(let ((x (+ 1 2 3)))

  (check x => 6)

  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #t)

  (check (exact?    x) => #t)
  (check (inexact?  x) => #f)
  )

(let ((x (+ 1 1/2)))

  (check x => 3/2)

  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #f)

  (check (exact?    x) => #t)
  (check (inexact?  x) => #f)
  )

(let ((x (* 2 1/2)))

  (check x => 1)

  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #t)

  (check (exact?    x) => #t)
  (check (inexact?  x) => #f)
  )

(let ((x (+ 1/3 1/3 1/3)))

  (check x => 1)

  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #t)

  (check (exact?    x) => #t)
  (check (inexact?  x) => #f)
  )

(check (+ 1   1.0) => 2.0)
(check (+ 1.0 1  ) => 2.0)

(check (+ 1   1/2) => 3/2)
(check (+ 1.0 1/2) => 1.5)

(check (+ 1/2 1  ) => 3/2)
(check (+ 1/2 1.0) => 1.5)

(check (* 2   1/2) => 1  )
(check (* 2.0 1/2) => 1.0)

(check (modulo  13  4) =>  1)
(check (modulo -13  4) =>  3)
(check (modulo  13 -4) => -3)
(check (modulo -13 -4) => -1)

(check (remainder  13  4)   =>  1)
(check (remainder -13  4)   => -1)
(check (remainder  13 -4)   =>  1)
(check (remainder -13 -4)   => -1)
(check (remainder -13 -4.0) => -1.0) ; inexact

(check (log 0.0) => -inf.0)

(check (sin          0)  (=> =) 0)
(check (sin (/ fl-pi 6)) (=> =) 0.5)
; (check (sin (/ fl-pi 4)) (=> =) 0.707107)
; (check (sin (/ fl-pi 3)) (=> =) 0.866025)
(check (sin (/ fl-pi 2)) (=> =) 1)
(check (sin    fl-pi)    (=> =) 0)

(check (atan  0.0  1.0) =>  0.000000)
; (check (atan  1.0  1.0) =>  0.785398)
; (check (atan  1.0  0.0) =>  1.570796)
; (check (atan  1.0 -1.0) =>  2.356194)
; (check (atan  0.0 -1.0) =>  3.141593)
; (check (atan -1.0 -1.0) => -2.356194)
; (check (atan -1.0  0.0) => -1.570796)
; (check (atan -1.0  1.0) => -0.785398)

(check (exact 0.333333333333) => 1/3)

; ---- SRFI-78 -----------------------------------------------------------------

(check-report)

(exit (check-passed? check:correct))
