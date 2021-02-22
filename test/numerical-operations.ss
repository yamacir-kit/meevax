; ---- 6.2.6. Numerical operations ---------------------------------------------

(check (rational? 1/3) => #t)
(check (rational? 0.5) => #f)

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

; ---- SRFI-78 -----------------------------------------------------------------

(check-report)

(exit (if (check-passed? check:correct) 0 1))
