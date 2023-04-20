(import (scheme base)
        (scheme process-context)
        (srfi 31)
        (srfi 78))

(define F (rec (F N)
               ((rec (G K L)
                     (if (zero? K) L
                         (G (- K 1) (* K L)))) N 1)))

(check (procedure? F) => #t)

(check (F 0) => 1)

(check (F 10) => 3628800)

(check-report)

(exit (check-passed? 3))
