(import (scheme base)
        (scheme inexact)
        (scheme process-context)
        (only (meevax binary32) binary32?)
        (only (meevax binary64) binary64?)
        (srfi 78)
        (srfi 144))

(check (real?     1.0e0) => #t)
(check (binary32? 1.0e0) => #f)
(check (binary64? 1.0e0) => #t)

(check (real?     1.0f0) => #t)
(check (binary32? 1.0f0) => #t)
(check (binary64? 1.0f0) => #f)

(check (real?     1.0d0) => #t)
(check (binary32? 1.0d0) => #f)
(check (binary64? 1.0d0) => #t)

(check (rational? 1/3) => #t)
(check (rational? 0.5) => #t)

(check (+ 1 2 3) (=> =) 6)
(check (number?   (+ 1 2 3)) => #t)
(check (complex?  (+ 1 2 3)) => #t)
(check (real?     (+ 1 2 3)) => #t)
(check (rational? (+ 1 2 3)) => #t)
(check (integer?  (+ 1 2 3)) => #t)
(check (exact?    (+ 1 2 3)) => #t)
(check (inexact?  (+ 1 2 3)) => #f)

(check (+ 1 1/2) (=> =) 3/2)
(check (number?   (+ 1 1/2)) => #t)
(check (complex?  (+ 1 1/2)) => #t)
(check (real?     (+ 1 1/2)) => #t)
(check (rational? (+ 1 1/2)) => #t)
(check (integer?  (+ 1 1/2)) => #f)
(check (exact?    (+ 1 1/2)) => #t)
(check (inexact?  (+ 1 1/2)) => #f)

(check (* 2 1/2) (=> =) 1)
(check (number?   (* 2 1/2)) => #t)
(check (complex?  (* 2 1/2)) => #t)
(check (real?     (* 2 1/2)) => #t)
(check (rational? (* 2 1/2)) => #t)
(check (integer?  (* 2 1/2)) => #t)
(check (exact?    (* 2 1/2)) => #t)
(check (inexact?  (* 2 1/2)) => #f)

(check (+ 1/3 1/3 1/3) (=> =) 1)
(check (number?   (+ 1/3 1/3 1/3)) => #t)
(check (complex?  (+ 1/3 1/3 1/3)) => #t)
(check (real?     (+ 1/3 1/3 1/3)) => #t)
(check (rational? (+ 1/3 1/3 1/3)) => #t)
(check (integer?  (+ 1/3 1/3 1/3)) => #t)
(check (exact?    (+ 1/3 1/3 1/3)) => #t)
(check (inexact?  (+ 1/3 1/3 1/3)) => #f)

(check (+ 1   1.0) (=> =) 2.0)
(check (+ 1.0 1  ) (=> =) 2.0)

(check (+ 1   1/2) (=> =) 3/2)
(check (+ 1.0 1/2) (=> =) 1.5)

(check (+ 1/2 1  ) (=> =) 3/2)
(check (+ 1/2 1.0) (=> =) 1.5)

(check (* 2   1/2) (=> =) 1  )
(check (* 2.0 1/2) (=> =) 1.0)

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
; (check (sin    fl-pi)    (=> =) 0)

(check (atan  0.0  1.0) =>  0.000000)
; (check (atan  1.0  1.0) =>  0.785398)
; (check (atan  1.0  0.0) =>  1.570796)
; (check (atan  1.0 -1.0) =>  2.356194)
; (check (atan  0.0 -1.0) =>  3.141593)
; (check (atan -1.0 -1.0) => -2.356194)
; (check (atan -1.0  0.0) => -1.570796)
; (check (atan -1.0  1.0) => -0.785398)

(check (exact 0.333333) => 3002396749180579/9007199254740992)

(check-report)

(exit (check-passed? 66))
