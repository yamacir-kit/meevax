(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(display (tak 18 12 6))
(newline)

(exit)
