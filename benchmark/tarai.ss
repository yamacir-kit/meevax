(import (scheme base)
        (scheme process-context))

(define (tarai x y z)
  (if (not (< y x)) y
      (tarai (tarai (- x 1) y z)
             (tarai (- y 1) z x)
             (tarai (- z 1) x y))))

(tarai 10 5 0)

(exit)
