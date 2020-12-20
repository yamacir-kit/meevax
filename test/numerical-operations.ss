; ---- SRFI-78 -----------------------------------------------------------------

(check-set-mode! 'report)

; ---- 6.2.6. Numerical operations ---------------------------------------------

(check (rational? 1/3) => #t)
(check (rational? 0.5) => #f)

(let ((x (+ 1 2 3)))
  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #t))

(let ((x (+ 1 1/2)))
  (check x => 3/2)
  (check (number?   x) => #t)
  (check (complex?  x) => #t)
  (check (real?     x) => #t)
  (check (rational? x) => #t)
  (check (integer?  x) => #f)
  )

; ---- SRFI-78 -----------------------------------------------------------------

(check-report)

(exit (if (check-passed? check::correct) exit-success exit-failure))
